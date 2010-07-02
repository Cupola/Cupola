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
 *    @version 0.12, 02-Jul-10
 */
object ProcWorld {
   case class ProcsRemoved( procs: Proc* )
   case class ProcsAdded( procs: Proc* )
//   case class EdgesRemoved( edges: ProcEdge* )
//   case class EdgesAdded( edges: ProcEdge* )
}
class ProcWorld extends Model {
   import ProcWorld._
   
   val synthGraphs = Ref( Map.empty[ SynthGraph, RichSynthDef ])
   val topology    = Ref.withObserver( Topology.empty[ Proc, ProcEdge ]) { (oldTop, newTop) =>
      // getting nasty... we should track the changes eventually
      // inside a customized Ref object XXX
      val verticesRemoved  = oldTop.vertices.diff( newTop.vertices )
      val verticesAdded    = newTop.vertices.diff( oldTop.vertices )
//      val oldEdges         = oldTop.edgeMap.values.flatten.toSeq // ayayay
//      val newEdges         = newTop.edgeMap.values.flatten.toSeq
//      val edgesRemoved     = oldEdges.diff( newEdges )
//      val edgesAdded       = newEdges.diff( oldEdges )
//      if( edgesRemoved.nonEmpty )      dispatch( EdgesRemoved( edgesRemoved: _* ))
      if( verticesRemoved.nonEmpty )   dispatch( ProcsRemoved( verticesRemoved: _* ))
      if( verticesAdded.nonEmpty )     dispatch( ProcsAdded( verticesAdded: _* ))
//      if( edgesAdded.nonEmpty )        dispatch( EdgesAdded( edgesAdded: _* ))
   }
}

object ProcDemiurg { // ( val server: Server )
   demi =>

   private val syn = new AnyRef
   private var servers = Set.empty[ Server ]

   private var uniqueDefID    = 0
   private def nextDefID      = { val res = uniqueDefID; uniqueDefID += 1; res }

   def addServer( server: Server ) : Unit = syn.synchronized {
      if( servers.contains( server )) return
      servers += server
      worlds += server -> new ProcWorld
   }

   // commented out for debugging inspection
   var worlds = Map.empty[ Server, ProcWorld ] // new ProcWorld

   def addVertex( e: Proc )( implicit tx: ProcTxn ) : Unit = syn.synchronized {
      val world = worlds( e.server )
      world.topology.transform( _.addVertex( e ))
   }

   def addEdge( e: ProcEdge )( implicit tx: ProcTxn ) : Unit = syn.synchronized {
//      val world = worlds( e._1.proc.server )
      val world = worlds( e.sourceVertex.server )
      val res = world.topology().addEdge( e )
      if( res.isEmpty ) error( "Could not add edge" )

      val Some( (newTopo, source, affected) ) = res
      world.topology.set( newTopo )
      if( affected.isEmpty ) {
         return
      }

      val srcGroup     = source.groupOption
      val tgtGroups    = affected.map( p => (p, p.groupOption) )

      def startMoving( g: RichGroup ) {
         var succ                = g
         var pred : RichGroup    = null
         val iter                = tgtGroups.iterator
         while( iter.hasNext ) {
            pred = succ
            val (target, tgtGroup) = iter.next
            tgtGroup match {
               case Some( g ) => {
//                  tx.addFirst( g.server, g.moveAfterMsg( pred ))
                  g.moveAfter( true, pred )
                  succ = g
               }
               case None => {
                  val g = RichGroup( Group( target.server ))
//                  tx.addFirst( g.server, g.newMsg( pred, addAfter ))
                  g.play( pred, addAfter )
                  target.group = g
                  succ = g
               }
            }
         }
      }

      srcGroup match {
         case None => {
            val g = RichGroup( Group( source.server ))
//            tx.addFirst( g.server, g.newMsg( g.server.defaultGroup, addToHead ))
            g.play( RichGroup.default( g.server ))
            source.group = g
            startMoving( g )
         }
         case Some( g ) => startMoving( g )
      }
   }

   def getSynthDef( server: Server, graph: SynthGraph )( implicit tx: ProcTxn ) : RichSynthDef = syn.synchronized {
      val w    = worlds( server )
      w.synthGraphs().get( graph ).getOrElse({
         val name = "proc" + nextDefID
         val rd   = RichSynthDef( server, SynthDef( name, graph ))
         w.synthGraphs.transform( _ + (graph -> rd) )
//         tx.add( server, rd.synthDef.recvMsg )
         rd
      })
   }
}