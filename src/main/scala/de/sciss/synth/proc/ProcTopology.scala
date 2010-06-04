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

//case class ProcEdge( source: ProcAudioOutput, target: ProcAudioInput )

/**
 *    @version 0.11, 04-Jun-10
 */
object ProcTopology {
   type Edge = (ProcAudioOutput, ProcAudioInput)
   def empty = apply( Vector.empty, 0, Map.empty ) 
}
case class ProcTopology( vertices: IIdxSeq[ Proc ], unpositioned: Int,
                         edgeMap: IMap[ Proc, ISet[ ProcTopology.Edge ]]) {
   import ProcTopology._
   
   def addEdge( e: Edge ) : Option[ (ProcTopology, Proc, IIdxSeq[ Proc ])] = {
      val source	   = e._1.proc
      val target	   = e._2.proc
      val upBound	   = vertices.indexOf( source )
      val loBound	   = vertices.indexOf( target )
      val newEdgeMap = edgeMap + (source -> (edgeMap.getOrElse( source,  Set.empty ) + e))

      // dealing with unpositioned elements
      if( upBound < unpositioned ) { // first edge for source
         if( loBound < unpositioned ) { // first edge for target
            val min        = math.min( upBound, loBound )
            val max        = math.max( upBound, loBound )
            val newUnpos   = unpositioned - 2
            val newVertices= vertices.patch( min, Vector.empty, 1 ).patch( max - 1, Vector.empty, 1 )
               .patch( newUnpos, Vector( source, target ), 0 )
            Some( (copy( newVertices, newUnpos, newEdgeMap ), source, Vector( target )))
         } else {
//            Some( (this, Vector.empty) )
            val newUnpos   = unpositioned - 1
            val newVertices= vertices.patch( upBound, Vector.empty, 1 )
               .patch( loBound - 1, Vector( source ), 0 )
            Some( (copy( newVertices, newUnpos, newEdgeMap ), target, Vector( source )))
         }

      // regular algorithm
      } else if( loBound > upBound ) {
         Some( (this, source, Vector.empty) )
      } else if( loBound < upBound ) {
         val visited    = new MHashSet[ Proc ]()
         if( !discovery( visited, newEdgeMap, target, upBound )) {
            None  // Cycle --> Abort
         } else {
            val (newVertices, affected) = shift( visited, loBound, upBound )
            Some( (copy( newVertices, unpositioned, newEdgeMap ), source, affected) )
         }
      } else { // loBound == upBound
         None
      }
   }

   def addVertex( v: Proc ) : ProcTopology = {
      copy( v +: vertices, unpositioned + 1 )
   }

   // note: assumes audio rate
   private def discovery( visited: MSet[ Proc ], newEdgeMap: IMap[ Proc, ISet[ Edge ]], v: Proc,
                          upBound: Int ) : Boolean = {
      var targets = MStack( v )
      while( targets.nonEmpty ) {
         val v = targets.pop
         visited += v
//         val moreTargets   = v.audioOutputs.flatMap( _.edges ).map( _.target.proc )
         val moreTargets   = newEdgeMap.getOrElse( v, Set.empty ).map( _._2.proc )
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
   private def shift( visited: collection.Set[ Proc ], loBound: Int,
                      upBound: Int ) : (IIdxSeq[ Proc], IIdxSeq[ Proc]) = {
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