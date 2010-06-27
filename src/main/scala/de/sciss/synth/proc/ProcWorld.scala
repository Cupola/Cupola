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
 *    @version 0.12, 21-Jun-10
 */
class ProcWorld {
   val synthGraphs = Ref( Map.empty[ SynthGraph, RichSynthDef ])  // XXX per server?
//   val topology    = Ref( ProcTopology.empty )
   val topology    = Ref( Topology.empty[ Proc, ProcEdge ])
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

      val srcGroup     = source.group
      val tgtGroups    = affected.map( p => (p, p.group) )

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
                  target.setGroup( g )
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
            source.setGroup( g )
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