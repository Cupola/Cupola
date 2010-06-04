package de.sciss.synth.proc

import collection.immutable.{ IndexedSeq => IIdxSeq, Map => IMap, Seq => ISeq }
import collection.mutable.{ HashSet => MHashSet, Set => MSet, Stack => MStack }

trait ProcEdge {
   def source: ProcAudioOutput
   def target: ProcAudioInput
}

/**
 *    @version 0.11, 03-Jun-10
 */
case class ProcTopology( vertices: IIdxSeq[ Proc ], edgeMap: IMap[ Proc, ISeq[ ProcEdge ]]) {
   private def checkAddedEdge( tx: ProcTransaction, e: ProcEdge ) : Option[ ProcTopology ] = {
      val source	= e.source.proc
      val target	= e.target.proc
      val loBound	= vertices.indexOf( target )
      val upBound	= vertices.indexOf( source )
      if( loBound > upBound ) {
         Some( this )
      } else if( loBound < upBound ) {
         val visited = new MHashSet[ Proc ]()
         if( !discovery( visited, target, upBound )) {
            // Cycle --> Abort
//          e.protRemove
            None
         } else {
//            Some( copy( shift( bndl, visited, target, source )))
            Some( copy( shift( tx, visited, loBound, upBound )))
         }
      } else { // loBound == upBound
         None
      }
   }

   // note: assumes audio rate
   private def discovery( visited: MSet[ Proc ], v: Proc, upBound: Int ) : Boolean = {
      var targets = MStack( v )
      while( targets.nonEmpty ) {
         val v = targets.pop
         visited += v
//         val moreTargets   = v.audioOutputs.flatMap( _.edges ).map( _.target.proc )
         val moreTargets   = edgeMap.get( v ).getOrElse( Nil ).map( _.target.proc )
         val grouped       = moreTargets.groupBy( vertices.indexOf( _ ).compare( upBound ))
         if( grouped.contains( 0 )) return false // cycle detected
         // visit s if it was not not already visited
         // and if it is in affected region
//         grouped.get( -1 ).foreach( targets.pushAll( _.diff( visited )))
         targets.pushAll( grouped.get( -1 ).getOrElse( Nil ).filter( !visited.contains( _ )))
      }
      true
   }

   // initial cond: loBound (target) < upBound (source)
   private def shift( tx: ProcTransaction, visited: collection.Set[ Proc ], loBound: Int,
                      upBound: Int ) : IIdxSeq[ Proc] = {
      // shift vertices in affected region down ord
      val (a, b)                 = vertices.splitAt( upBound )
      val (begin, target)        = a.splitAt( loBound )
//      val (source, end)          = b.splitAt( 1 )
      val source                 = b.head
      val end                    = b.tail
      val (affected, unaffected) = target.partition( visited.contains( _ ))

      val shifted = begin ++ unaffected ++ (source +: affected) ++ end

      // push to transaction
      error( "NOT YET IMPLEMENTED" )
//      affected.foldLeft( source )( (pred, succ) => { succ.moveAfter( tx, pred ); succ })

      shifted
   }
}