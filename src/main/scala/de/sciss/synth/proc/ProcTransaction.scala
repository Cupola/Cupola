package de.sciss.synth.proc

import collection.immutable.{ Queue => IQueue }
import de.sciss.synth.osc.OSCSyncMessage
import de.sciss.synth._
import de.sciss.scalaosc.{OSCPacket, OSCBundle, OSCMessage}
import actors.{Futures, DaemonActor, Future, TIMEOUT}

trait ProcTransaction {
//   def addSynthGraph( graph: SynthGraph ) : String
   def addSynth( graph: SynthGraph, newMsg: String => OSCMessage, bufs: Seq[ RichBuffer ] = Nil ) : Unit
   def addBuffer( buf: Buffer, allocMsg: OSCMessage ) : RichBuffer
   def addProc( proc: Proc ) : Unit
   def addEdge( e: ProcTopology.Edge ) : Unit // Future[ Boolean ]
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
      var abortFuns        = IQueue.empty[ Function0[ Unit ]]
      
      lazy val syncID      = nextSyncID
      private var waitID   = -1

      def act = {
         var done = false
         loopWhile( !done ) {
            react {
               case e: Exec => e.exec
               case Commit => {
//                  println( "tx : Commit" )
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
                           performAborts
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
                  performAborts
                  done = true
               }
            }
         }
      }

      private def performAborts {
         abortFuns.foreach( fun => try { fun() } catch { case e => e.printStackTrace() })
      }

      private def exec( thunk: => Unit ) = {
         tx ! new Exec( thunk )
      }

      def abort : Unit = tx ! Abort
      def commit : Future[ ProcWorld ] = tx !! (Commit, { case Committed( world ) => world })

      def addSynth( graph: SynthGraph, newMsg: String => OSCMessage, bufs: Seq[ RichBuffer ]) {
         exec( txAddSynth( graph, newMsg, bufs ))
      }

      def addBuffer( buf: Buffer, allocMsg: OSCMessage ) : RichBuffer = {
         val rb = RichBuffer( buf, RichObject.Pending( syncID ))
         exec( txAddBuffer( rb, allocMsg ))
         rb
      }

      def addProc( proc: Proc ) {
         exec( txAddProc( proc ))
      }

      def addEdge( e: ProcTopology.Edge ) {
         exec( txAddEdge( e ))
//         tx !!( new Exec( txAddEdge( e )), { case b: Boolean => b })
      }

      private def txAddBuffer( rb: RichBuffer, allocMsg: OSCMessage ) {
         addFirst( allocMsg )
         abortFuns = abortFuns.enqueue( () => rb.buf.release )
      }

      private def txAddSynth( graph: SynthGraph, newMsg: String => OSCMessage, bufs: Seq[ RichBuffer ]) {
         val rd = builder.synthGraphs.get( graph ).getOrElse({
            val name = "proc" + nextDefID
            val rd   = RichSynthDef( SynthDef( name, graph ), RichObject.Pending( syncID ))
            builder.synthGraphs += graph -> rd
            addFirst( rd.synthDef.recvMsg )
            rd
         })
//         println( "txAddSynth : " + rd.synthDef.name )
         val msg = newMsg( rd.synthDef.name )
         val ids = (rd +: bufs).map( _.state ).collect({ case RichObject.Pending( syncID ) => syncID })
         if( ids.isEmpty ) {
            addFirst( msg )
         } else {
            waitID = math.max( waitID, ids.max )
            addSecond( msg )
         }
      }

      private def txAddProc( proc: Proc ) {
         builder.topology = builder.topology.addVertex( proc )
      }

      private def txAddEdge( e: ProcTopology.Edge ) {
println( "txAddEdge" )
         val res = builder.topology.addEdge( e )
         if( res.isEmpty ) {
            tx.abort
//            who ! false
            return
         }

         val Some( (newTopo, source, affected) ) = res
         builder.topology        = newTopo
         if( affected.isEmpty ) {
            tx.commit
//            who ! true
            return
         }

println( "txAddEdge -----1" )
         val futSrcGroup      = source.group
         val futTgtGroups     = affected.map( p => (p, p.group) )
         var futSets          = List.empty[ Future[ Any ]]

         def startMoving( g: Group ) {
println( "txAddEdge -----4" )
            var succ          = g
            var pred : Group  = null
            val iter          = futTgtGroups.iterator
            loopWhile( iter.hasNext ) {
println( "txAddEdge -----5" )
               pred = succ
               val (target, fut) = iter.next
               fut.inputChannel.react {
                  case Some( g ) => {
println( "txAddEdge -----6" )
                     addFirst( g.moveAfterMsg( pred ))
                     succ = g
                  }
                  case None => {
println( "txAddEdge -----7" )
                     val g = Group( wa.server )
                     addFirst( g.newMsg( pred, addAfter ))
                     futSets ::= target.setGroup( tx, g )
                     succ = g
                  }
               }
            } andThen {
println( "txAddEdge -----8" )
               val success = !Futures.awaitAll( 5000L, futSets: _* ).contains( None )
//               who ! success
               if( success ) tx.commit else tx.abort
            }
         }

println( "txAddEdge -----2" )
         futSrcGroup.inputChannel.react {
            case None => {
println( "txAddEdge -----3" )
               val g = Group( wa.server )
               addFirst( g.newMsg( wa.server.defaultGroup, addToHead ))
               futSets ::= source.setGroup( tx, g )
               startMoving( g )
            }
            case Some( g ) => startMoving( g )
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