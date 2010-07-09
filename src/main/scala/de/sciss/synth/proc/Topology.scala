/*
 *  ProcTopology.scala
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

import collection.immutable.{ IndexedSeq => IIdxSeq, Map => IMap, Seq => ISeq, Set => ISet }
import collection.mutable.{ HashSet => MHashSet, Set => MSet, Stack => MStack }

/**
 *    @version 0.12, 09-Jun-10
 */
object Topology {
   def empty[ V, E <: Edge[ V ]] = apply( Vector.empty[ V ], ISet.empty[ E ])( 0, Map.empty[ V, ISet[ E ]])

   trait Edge[ V ] {
      def sourceVertex : V
      def targetVertex : V
   }
}
case class Topology[ V, E <: Topology.Edge[ V ]]( vertices: IIdxSeq[ V ], edges: ISet[ E ])
                                                ( unpositioned: Int, edgeMap: IMap[ V, ISet[ E ]]) {
   import Topology._

   type T = Topology[ V, E ]

   def addEdge( e: E ) : Option[ (T, V, IIdxSeq[ V ])] = {
      val source	   = e.sourceVertex
      val target	   = e.targetVertex
      val upBound	   = vertices.indexOf( source )
      val loBound	   = vertices.indexOf( target )
      require( (loBound >= 0) && (upBound >= 0) )
      val newEdgeMap = edgeMap + (source -> (edgeMap.getOrElse( source,  Set.empty ) + e))
      val newEdgeSet = edges + e

      // dealing with unpositioned elements
      if( upBound < unpositioned ) { // first edge for source
         if( loBound < unpositioned ) { // first edge for target
            val min        = math.min( upBound, loBound )
            val max        = math.max( upBound, loBound )
            val newUnpos   = unpositioned - 2
            val newVertices= vertices.patch( min, Vector.empty, 1 ).patch( max - 1, Vector.empty, 1 )
               .patch( newUnpos, Vector( source, target ), 0 )
            Some( (copy( newVertices, newEdgeSet )( newUnpos, newEdgeMap ), source, Vector( target )))
         } else {
//            Some( (this, Vector.empty) )
            val newUnpos   = unpositioned - 1
            val newVertices= vertices.patch( upBound, Vector.empty, 1 )
               .patch( loBound - 1, Vector( source ), 0 )
            Some( (copy( newVertices, newEdgeSet )( newUnpos, newEdgeMap ), target, Vector( source )))
         }

      // regular algorithm
      } else if( loBound > upBound ) {
         Some( (this, source, Vector.empty) )
      } else if( loBound < upBound ) {
         val visited    = new MHashSet[ V ]()
         if( !discovery( visited, newEdgeMap, target, upBound )) {
            None  // Cycle --> Abort
         } else {
            val (newVertices, affected) = shift( visited, loBound, upBound )
            Some( (copy( newVertices, newEdgeSet )( unpositioned, newEdgeMap ), source, affected) )
         }
      } else { // loBound == upBound
         None
      }
   }

   def removeEdge( e: E ) : T = {
      if( edges.contains( e )) {
         val source = e.sourceVertex
         copy( edges = edges - e )( unpositioned, edgeMap + (source -> (edgeMap( source ) - e)) )
      } else this
   }

   def addVertex( v: V ) : T = {
      require( !vertices.contains( v ))
// XXX TEST
//      copy( vertices.patch( unpositioned, Vector( v ), 0 ), unpositioned + 1 )
      copy( v +: vertices )( unpositioned + 1, edgeMap )
   }

   def removeVertex( v: V ) : T = {
      val idx = vertices.indexOf( v )
      if( idx >= 0 ) {
         val newUnpos = if( idx >= unpositioned ) unpositioned - 1 else unpositioned
         copy( vertices.patch( idx, Vector.empty, 1 ))( newUnpos, edgeMap - v )
      } else this
   }

   // note: assumes audio rate
   private def discovery( visited: MSet[ V ], newEdgeMap: IMap[ V, ISet[ E ]], v: V, upBound: Int ) : Boolean = {
      var targets = MStack( v )
      while( targets.nonEmpty ) {
         val v = targets.pop
         visited += v
//         val moreTargets   = v.audioOutputs.flatMap( _.edges ).map( _.target.proc )
         val moreTargets   = newEdgeMap.getOrElse( v, Set.empty ).map( _.targetVertex )
         val grouped       = moreTargets.groupBy( vertices.indexOf( _ ).compare( upBound ))
         if( grouped.contains( 0 )) return false // cycle detected
         // visit s if it was not not already visited
         // and if it is in affected region
//         grouped.get( -1 ).foreach( targets.pushAll( _.diff( visited )))
         targets.pushAll( grouped.getOrElse( -1, Set.empty ).filter( !visited.contains( _ )))
      }
      true
   }

   // initial cond: loBound (target) < upBound (source)
   private def shift( visited: collection.Set[ V ], loBound: Int, upBound: Int ) : (IIdxSeq[ V ], IIdxSeq[ V ]) = {
      // shift vertices in affected region down ord
      val (a, b)                 = vertices.splitAt( upBound )
      val (begin, target)        = a.splitAt( loBound )
//      val (source, end)          = b.splitAt( 1 )
      val source                 = b.head
      val end                    = b.tail
      val (affected, unaffected) = target.partition( visited.contains( _ ))

      val shifted = begin ++ unaffected ++ (source +: affected) ++ end

      // push to transaction
//      error( "NOT YET IMPLEMENTED" )
//      affected.foldLeft( source )( (pred, succ) => { succ.moveAfter( tx, pred ); succ })

      (shifted, affected)
   }
}