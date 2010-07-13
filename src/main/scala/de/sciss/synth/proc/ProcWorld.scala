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
import actors.{ Actor, DaemonActor, Future, TIMEOUT }
import collection.immutable.{ IndexedSeq => IIdxSeq, Seq => ISeq, Set => ISet }

/**
 *    @version 0.12, 06-Jul-10
 */
object ProcWorld {
//   case class ProcsRemoved( procs: Proc* )
//   case class ProcsAdded( procs: Proc* )
////   case class EdgesRemoved( edges: ProcEdge* )
////   case class EdgesAdded( edges: ProcEdge* )

   case class Update( procsAdded: ISet[ Proc ], procsRemoved: ISet[ Proc ])
   type Listener = TxnModel.Listener[ Update ]
}

class ProcWorld extends TxnModel[ ProcWorld.Update ] {
   import ProcWorld._

   private type Topo = Topology[ Proc, ProcEdge ] 
   val synthGraphs = Ref( Map.empty[ SynthGraph, RichSynthDef ])
   private val topologyRef = Ref[ Topo ]( Topology.empty )
//   val topology    = Ref.withObserver( Topology.empty[ Proc, ProcEdge ]) { (oldTop, newTop) =>
//      // getting nasty... we should track the changes eventually
//      // inside a customized Ref object XXX
//      val verticesRemoved  = oldTop.vertices.diff( newTop.vertices )
//      val verticesAdded    = newTop.vertices.diff( oldTop.vertices )
////      val oldEdges         = oldTop.edgeMap.values.flatten.toSeq // ayayay
////      val newEdges         = newTop.edgeMap.values.flatten.toSeq
////      val edgesRemoved     = oldEdges.diff( newEdges )
////      val edgesAdded       = newEdges.diff( oldEdges )
////      if( edgesRemoved.nonEmpty )      dispatch( EdgesRemoved( edgesRemoved: _* ))
//      if( verticesRemoved.nonEmpty )   dispatch( ProcsRemoved( verticesRemoved: _* ))
//      if( verticesAdded.nonEmpty )     dispatch( ProcsAdded( verticesAdded: _* ))
////      if( edgesAdded.nonEmpty )        dispatch( EdgesAdded( edgesAdded: _* ))
//   }

   protected def fullUpdate( implicit tx: ProcTxn ) = Update( topologyRef().vertices.toSet, Set.empty )
   protected def emptyUpdate = Update( Set.empty, Set.empty )

   def topology( implicit tx: ProcTxn ) = topologyRef()

   def addProc( p: Proc )( implicit tx: ProcTxn ) {
      touch
      topologyRef.transform( _ addVertex p )
      updateRef.transform( u => if( u.procsRemoved.contains( p )) {
          u.copy( procsRemoved = u.procsRemoved - p )
      } else {
          u.copy( procsAdded   = u.procsAdded   + p )
      })
   }

   def removeProc( p: Proc )( implicit tx: ProcTxn ) {
      touch
      topologyRef.transform( _ removeVertex p )
      updateRef.transform( u => if( u.procsAdded.contains( p )) {
          u.copy( procsAdded = u.procsAdded - p )
      } else {
          u.copy( procsRemoved = u.procsRemoved + p )
      })
   }

   def addEdge( e: ProcEdge )( implicit tx: ProcTxn ) : Option[ (Topo, Proc, IIdxSeq[ Proc ])] = {
      val res = topologyRef().addEdge( e )
      res.foreach( tup => topologyRef.set( tup._1 ))
      res
   }

   def removeEdge( e: ProcEdge )( implicit tx: ProcTxn ) {
      topologyRef.transform( _.removeEdge( e ))
   }
}

/**
 *    @todo should also announce world creations
 */
case class ProcDemiurgUpdate( factoriesAdded: ISet[ ProcFactory ], factoriesRemoved: ISet[ ProcFactory ])

object ProcDemiurg extends TxnModel[ ProcDemiurgUpdate ] { // ( val server: Server )
   demi =>

   type Update    = ProcDemiurgUpdate
   type Listener  = TxnModel.Listener[ Update ]

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

   val factories = Ref( Set.empty[ ProcFactory ])

   protected def fullUpdate( implicit tx: ProcTxn ) = ProcDemiurgUpdate( factories(), Set.empty )
   protected def emptyUpdate = ProcDemiurgUpdate( Set.empty, Set.empty )

   def addFactory( pf: ProcFactory )( implicit tx: ProcTxn ) {
      touch
      factories.transform( _ + pf )
      updateRef.transform( u => if( u.factoriesRemoved.contains( pf )) {
          u.copy( factoriesRemoved = u.factoriesRemoved - pf )
      } else {
          u.copy( factoriesAdded = u.factoriesAdded + pf )
      })
   }

   def removeFactory( pf: ProcFactory )( implicit tx: ProcTxn ) {
      touch
      factories.transform( _ - pf )
      updateRef.transform( u => if( u.factoriesAdded.contains( pf )) {
          u.copy( factoriesAdded = u.factoriesAdded - pf )
      } else {
          u.copy( factoriesRemoved = u.factoriesRemoved + pf )
      })
   }

   def addVertex( e: Proc )( implicit tx: ProcTxn ) : Unit = syn.synchronized {
      val world = worlds( e.server )
//      world.topology.transform( _.addVertex( e ))
      world.addProc( e )
   }

   def removeVertex( e: Proc )( implicit tx: ProcTxn ) : Unit = syn.synchronized {
      val world = worlds( e.server )
      world.removeProc( e )
   }

   def addEdge( e: ProcEdge )( implicit tx: ProcTxn ) : Unit = syn.synchronized {
//      val world = worlds( e._1.proc.server )
      val world = worlds( e.sourceVertex.server )
      val res = world.addEdge( e )
      if( res.isEmpty ) error( "Could not add edge" )

      val Some( (newTopo, source, affected) ) = res
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

   def removeEdge( e: ProcEdge )( implicit tx: ProcTxn ) : Unit = syn.synchronized {
      val world = worlds( e.sourceVertex.server )
      world.removeEdge( e )
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