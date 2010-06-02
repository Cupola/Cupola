package de.sciss.synth.proc

import de.sciss.synth.{ AddAction, ControlSetMap, Node, Synth, SynthDef, SynthGraph }
import de.sciss.scalaosc.{ OSCBundle, OSCMessage }
import actors.{ DaemonActor, Future, TIMEOUT }
import collection.immutable.{ Queue => IQueue }
import de.sciss.synth.osc.OSCSyncMessage

trait ProcTransaction {
//   def addSynthGraph( graph: SynthGraph ) : String
   def addSynth( graph: SynthGraph, args: Seq[ ControlSetMap ], target: Node, addAction: AddAction ) : Synth
   def abort : Unit
   def commit : Future[ ProcWorld ]
}

object ProcTransaction {
   def apply( wa: ProcWorldActor, world: ProcWorld ) : ProcTransaction = {
      val tx = new Impl( wa, world )
      tx.start
      tx
   }

   private var uniqueDefID    = 0
   private def nextDefID      = { val res = uniqueDefID; uniqueDefID += 1; res }
   private var uniqueSyncID   = 0
   private def nextSyncID     = { val res = uniqueSyncID; uniqueSyncID += 1; res }

   private object Impl {
      class Exec( thunk: => Unit ) { def exec = thunk }
      case object Abort
      case object Commit
      case class Committed( world: ProcWorld )
   }

   private class Impl( wa: ProcWorldActor, world: ProcWorld ) extends DaemonActor with ProcTransaction {
      tx =>

      import Impl._

      val builder          = new ProcWorldBuilder( world )
      var firstMsgs        = IQueue.empty[ OSCMessage ]
      var secondMsgs       = IQueue.empty[ OSCMessage ]
      lazy val syncID      = nextSyncID
      private var waitID   = -1

      def act = {
         var done = false
         loopWhile( !done ) {
            react {
               case e: Exec => e.exec
               case Commit => {
                  println( "tx : Commit" )
                  val query = sender
                  if( firstMsgs.nonEmpty ) {
                     wa.server ! OSCBundle( firstMsgs.enqueue( OSCSyncMessage( syncID )): _* )
                  }
                  def complete() {
                     wa.server ! OSCBundle( secondMsgs: _* )
                     val newWorld = builder.build
                     query ! Committed( newWorld )
                     wa.closeTx( tx, newWorld )
                     done = true
                  }
                  if( waitID >= 0 ) {
                     val fut = wa.sync( waitID )
                     fut.inputChannel.reactWithin( 10000L ) {
                        case TIMEOUT => {
                           println( "Oh noooo. Transaction timeout" )
                           query ! Committed( world ) // = oldWorld
                           wa.closeTx( tx, world )
                           done = true
                        }
                        case _ => complete()
                     }
                  } else {
                     complete()
                  }
               }
               case Abort => {
                  done = true
               }
            }
         }
      }

      private def exec( thunk: => Unit ) = {
         tx ! new Exec( thunk )
      }

      def abort : Unit = tx ! Abort
      def commit : Future[ ProcWorld ] = tx !! (Commit, { case Committed( world ) => world })

      def addSynth( graph: SynthGraph, args: Seq[ ControlSetMap ], target: Node, addAction: AddAction ) : Synth = {
         println( "addSynth" )
         val synth = Synth( target.server )
         exec( txAddSynth( synth, graph, args, target, addAction ))
         synth
      }

//      def addCueBuffer( buf: Buffer, ... )

      def txAddSynth( synth: Synth, graph: SynthGraph, args: Seq[ ControlSetMap ], target: Node, addAction: AddAction ) {
         val rd = builder.synthGraphs.get( graph ).getOrElse({
            val name = "proc" + nextDefID
            val rd   = RichSynthDef( SynthDef( name, graph ), RichSynthDef.Pending( syncID ))
            builder.synthGraphs += graph -> rd
            addFirst( rd.synthDef.recvMsg )
            rd
         })
         println( "txAddSynth : " + rd.synthDef.name )
         val msg = synth.newMsg( rd.synthDef.name, target, args )
         rd.state match {
            case RichSynthDef.Online => addFirst( msg )
            case RichSynthDef.Pending( syncID ) => {
               waitID = math.max( waitID, syncID )
               addSecond( msg )
            }
         }
      }

      private def addFirst( msg: OSCMessage ) {
         firstMsgs = firstMsgs.enqueue( msg )
      }

      private def addSecond( msg: OSCMessage ) {
         secondMsgs = secondMsgs.enqueue( msg )
      }
   }

//      def addSynth( synth: Synth, name: String ) {
//         XXX
//      }
//   }
}