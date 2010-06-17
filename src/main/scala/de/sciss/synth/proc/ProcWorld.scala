/*
 *  ProcWorld.scala
 *  (ScalaCollider-Proc)
 *
 *  Copyright (c) 2010 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 *
 *
 *  Changelog:
 */

package de.sciss.synth.proc

import de.sciss.synth.osc.{ OSCSyncedMessage, OSCResponder }
import de.sciss.synth._
import de.sciss.scalaosc.OSCMessage
import actors.{Future, Actor, DaemonActor, TIMEOUT}

/**
 *    @version 0.12, 15-Jun-10
 */
//trait ProcWorldLike {
////   def server: Server
//   def synthGraphs: Map[ SynthGraph, RichSynthDef ]
//   def topology: ProcTopology
////   def addSynthGraph( graph: SynthGraph ) : String
//}

class ProcWorld {
   val synthGraphs = Ref( Map.empty[ SynthGraph, RichSynthDef ])  // XXX per server?
   val topology    = Ref( ProcTopology.empty )
}

//object ProcWorld {
//   def empty = ProcWorld( Map.empty, ProcTopology.empty )
//}

object ProcWorldActor {
//   private case class Tx( fun: (ProcTransaction) => Unit )
//   private case class OpenTx( preparePos: Long, latency: Int )
//   private case class CloseTx( tx: ProcTransaction, world: ProcWorld )
   private case object Stop
   private case class Synced( id: Int )
   private case class Sync( id: Int )

   var default: ProcWorldActor = null 

   private val syn = new AnyRef
   private var syncActors = Map.empty[ Server, SyncActor ]

   def sync( server: Server, id: Int ) : Future[ Any ] = syn.synchronized {
      val sa = syncActors.getOrElse( server, {
         val syncActor = new SyncActor
         syncActors += server -> syncActor
         syncActor.start
         OSCResponder.add({
            case OSCSyncedMessage( i ) => syncActor ! Synced( i )
         }, server )
         syncActor
      })
      sa !! Sync( id )
   }

   private def add( wa: ProcWorldActor ) {
      if( default == null ) default = wa
   }

   private var uniqueDefID    = 0
   private def nextDefID      = { val res = uniqueDefID; uniqueDefID += 1; res }

   private class SyncActor extends DaemonActor {
      def act = {
         var seen = -1
         loop { react {
            case Sync( id ) if( seen >= id ) => reply( Synced( seen ))
            case Synced( id ) => seen = math.max( seen, id )
         }}
      }
   }
}

class ProcWorldActor { // ( val server: Server )
   wa =>

   import ProcWorldActor._

   ProcWorldActor.add( this )

   // commented out for debugging inspection
   /* private */ val world = new ProcWorld
//   val transport = ProcTransport( server.sampleRate, (server.sampleRate * 0.5).toInt )

//   def act = {
//      var running = true
////      syncActor.start
//      val resp = OSCResponder.add({
//         case OSCSyncedMessage( id ) => syncActor ! Synced( id )
//      }, server )
//      loopWhile( running ) {
//         react {
////            case OpenTx( preparePos, latency ) => {
////               println( "OpenTx " + preparePos + ", " + latency )
////               val tx = ProcTransaction( wa, world )
////               reply( tx )
//////               println( "OpenTx : replied" )
////               react {
////                  case CloseTx( tx2, newWorld ) => {
////                     println( "CloseTx" )
////                     if( tx2 != tx ) {
////                        println( "ERROR: trying to close an old transaction??" )
////                     } else {
////                        worldVar = newWorld
////                     }
////                  }
////               }
////            }
//            case Stop => {
//               resp.remove
//               running = false
//            }
//         }
//      }
//   }
//
//   def sync( id: Int ) : Future[ Any ] = syncActor !! Sync( id )

//   def stop {
//      wa ! Stop
//   }

//   private def await[ A ]( timeOut: Long, fut: Future[ A ])( handler: Function1[ Option[ A ], Unit ]) {
//      fut.inputChannel.reactWithin( timeOut ) {
//         case TIMEOUT => handler( None )
//         case a       => handler( Some( a.asInstanceOf[ A ]))
//      }
//   }
   
//   def tx( fun: (ProcTransaction) => Unit ) {
//      wa ! Tx( fun )
//   }

//   def openTx( preparePos: Long, latency: Int ) : Future[ ProcTransaction ] = {
//      wa !! (OpenTx( preparePos: Long, latency: Int ), { case tx: ProcTransaction => tx })
//   }
//
//   private[proc] def closeTx( tx: ProcTransaction, world: ProcWorld ) {
//      wa ! CloseTx( tx, world )
//   }

   def addEdge( e: ProcTopology.Edge )( implicit tx: ProcTxn ) {
      val res = world.topology().addEdge( e )
      if( res.isEmpty ) error( "Could not add edge" )

      val Some( (newTopo, source, affected) ) = res
      world.topology.set( newTopo )
      if( affected.isEmpty ) {
         return
      }

      val srcGroup     = source.group
      val tgtGroups    = affected.map( p => (p, p.group) )

      def startMoving( g: Group ) {
         var succ          = g
         var pred : Group  = null
         val iter          = tgtGroups.iterator
         while( iter.hasNext ) {
            pred = succ
            val (target, tgtGroup) = iter.next
            tgtGroup match {
               case Some( g ) => {
                  tx.addFirst( g.server, g.moveAfterMsg( pred ))
                  succ = g
               }
               case None => {
                  val g = Group( target.server )
                  tx.addFirst( g.server, g.newMsg( pred, addAfter ))
                  target.setGroup( g )
                  succ = g
               }
            }
         }
      }

      srcGroup match {
         case None => {
            val g = Group( source.server )
            tx.addFirst( g.server, g.newMsg( g.server.defaultGroup, addToHead ))
            source.setGroup( g )
            startMoving( g )
         }
         case Some( g ) => startMoving( g )
      }
   }

//   def addBuffer( buf: Buffer, allocMsg: OSCMessage, freeMsg: OSCMessage )( implicit tx: ProcTxn ) : RichBuffer = {
//      val rb = RichBuffer( buf, RichObject.Pending( syncID ))
//      tx.addFirst( allocMsg )
//      tx.addFirstAbort( rb.buf.release )
//      tx.addSecondAbort( freeMsg )
//      rb
//   }

   def addSynth( synth: Synth, graph: SynthGraph, newMsg: String => OSCMessage, bufs: Seq[ RichBuffer ])
               ( implicit tx: ProcTxn ) : RichSynth = {
      val rs = RichSynth( synth, RichObject.Pending( tx.syncID ))
      val server = rs.server
      val rd = world.synthGraphs().get( graph ).getOrElse({
         val name = "proc" + nextDefID
         val rd   = RichSynthDef( server, SynthDef( name, graph ), RichObject.Pending( tx.syncID ))
         world.synthGraphs.transform( _ + (graph -> rd) )
         tx.addFirst( server, rd.synthDef.recvMsg )
         rd
      })
      val msg = newMsg( rd.synthDef.name )
      val ids = (rd +: bufs).map( _.state ).collect({ case RichObject.Pending( syncID ) => syncID })
      if( ids.isEmpty ) {
         tx.addFirst( server, msg )
      } else {
         tx.waitFor( server, ids: _* )
         tx.addSecond( server, msg )
      }
      rs
   }

   def add( ro: RichObject, msg: OSCMessage )( implicit tx: ProcTxn ) {
      ro.state match {
         case RichObject.Pending( syncID ) => {
            tx.waitFor( ro.server, syncID )
            tx.addSecond( ro.server, msg )
         }

         case _ => tx.addFirst( ro.server, msg )
      }
   }
}

//class ProcWorldBuilder( previous: ProcWorld ) extends ProcWorldLike {
////   def server: Server = previous.server
//   var synthGraphs: Map[ SynthGraph, RichSynthDef ]   = previous.synthGraphs
//   var topology: ProcTopology                         = previous.topology
//
//   def build: ProcWorld = new ProcWorld( synthGraphs, topology )
//}
//
////object ProcWorld {
////   def apply( server: Server ) : ProcWorld = new Impl( server )
////
////   private class Impl( val server: Server ) {
//////      def addSynthGraph( graph: SynthGraph ) : String
////   }
////}