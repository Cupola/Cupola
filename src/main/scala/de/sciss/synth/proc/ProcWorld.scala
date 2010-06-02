package de.sciss.synth.proc

import de.sciss.synth.{ Server, SynthGraph }
import actors.{ Actor, DaemonActor, TIMEOUT }
import de.sciss.synth.osc.{ OSCSyncedMessage, OSCResponder }

trait ProcWorldLike {
//   def server: Server
   def synthGraphs: Map[ SynthGraph, RichSynthDef ]
//   def addSynthGraph( graph: SynthGraph ) : String
}

class ProcWorld( val synthGraphs: Map[ SynthGraph, RichSynthDef ])
extends ProcWorldLike

object ProcWorld {
   val empty = new ProcWorld( Map.empty )
}

object ProcWorldActor {
//   private case class Tx( fun: (ProcTransaction) => Unit )
   private case object OpenTx
   private case class CloseTx( tx: ProcTransaction, world: ProcWorld )
   private case object Stop
   private case class Synced( id: Int )
   private case class Sync( id: Int )

   var default: ProcWorldActor = null 

   private def add( wa: ProcWorldActor ) {
      if( default == null ) default = wa
   }
}

class ProcWorldActor( val server: Server ) extends Actor {
   wa =>

   import ProcWorldActor._

   add( this )
   
   private var worldVar: ProcWorld = ProcWorld.empty

   private val syncActor = new DaemonActor {
      def act = {
         var seen = -1
         loop { react {
            case Sync( id ) if( seen >= id ) => reply( Synced( seen ))
            case Synced( id ) => seen = math.max( seen, id )
         }}
      }
   }

   def act = {
      var running = true
      syncActor.start
      val resp = OSCResponder.add({
         case OSCSyncedMessage( id ) => syncActor ! Synced( id )
      }, server )
      loopWhile( running ) {
         react {
            case OpenTx => {
               println( "OpenTx" )
               val tx = ProcTransaction( wa, worldVar )
               reply( tx )
               println( "OpenTx : replied" )
               react {
                  case CloseTx( tx2, newWorld ) => {
                     println( "CloseTx" )
                     if( tx2 != tx ) {
                        println( "ERROR: trying to close an old transaction??" )
                     } else {
                        worldVar = newWorld
                     }
                  }
               }
            }
            case Stop => {
               resp.remove
               running = false
            }
         }
      }
   }

   def sync( id: Int ) : Future[ Any ] = syncActor !! Sync( id )

   def stop {
      wa ! Stop
   }

   private def await[ A ]( timeOut: Long, fut: Future[ A ])( handler: Function1[ Option[ A ], Unit ]) {
      fut.inputChannel.reactWithin( timeOut ) {
         case TIMEOUT => handler( None )
         case a       => handler( Some( a.asInstanceOf[ A ]))
      }
   }
   
//   def tx( fun: (ProcTransaction) => Unit ) {
//      wa ! Tx( fun )
//   }

   def openTx : Future[ ProcTransaction ] = {
      wa !! (OpenTx, { case tx: ProcTransaction => tx })
   }

   private[proc] def closeTx( tx: ProcTransaction, world: ProcWorld ) {
      wa ! CloseTx( tx, world ) 
   }
}

class ProcWorldBuilder( previous: ProcWorld ) extends ProcWorldLike {
//   def server: Server = previous.server
   var synthGraphs: Map[ SynthGraph, RichSynthDef ] = previous.synthGraphs

   def build: ProcWorld = new ProcWorld( synthGraphs )
}

//object ProcWorld {
//   def apply( server: Server ) : ProcWorld = new Impl( server )
//
//   private class Impl( val server: Server ) {
////      def addSynthGraph( graph: SynthGraph ) : String
//   }
//}