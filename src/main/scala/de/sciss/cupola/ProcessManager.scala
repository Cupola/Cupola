/*
 *  ProcessManager.scala
 *  (Cupola)
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

package de.sciss.cupola

import actors.Actor
import Cupola._
import util.Random
import de.sciss.synth.proc._
import DSL._
import java.util.{TimerTask, Timer}

/**
 *    @version 0.12, 09-Aug-10
 */
object ProcessManager {
   var verbose = false
}

class ProcessManager {
   import Util._
   import ProcessManager._
   
   private val startTimeRef   = Ref( 0.0 )
   private val lastTimeRef    = Ref( 0.0 )
   private val lastScaleRef   = Ref( 0.0 )
   private val scaleAccumRef  = Ref( 0.0 )

   private val timer = {
      val res = new Timer()
      res.schedule( new TimerTask {
         def run = timerUpdate
      }, 2000, 2000 )
      res
   }

   private val validRef = Ref( false )
   private val procsRunningRef = Ref( Set.empty[ RunningProc ])

   private def timerUpdate {
      ProcTxn.atomic { implicit tx =>
         if( !validRef() ) return

         val newTime = System.currentTimeMillis * 0.001 // tx.time
         val oldTime = lastTimeRef.swap( newTime )
         val oldScale = lastScaleRef()
         val oldAccum = scaleAccumRef.swap( 0.0 )
         val dt = newTime - oldTime
         val accum = oldAccum + (dt * oldScale)
         val oldStartTime = startTimeRef.swap( newTime )
         val totalTime = newTime - oldStartTime
         if( totalTime <= 0.0 ) return
         val scale = accum / totalTime
         scaleUpdate( scale )
      }
   }

   private def scaleUpdate( scale: Double )( implicit tx: ProcTxn ) {
      if( verbose ) println( "MEAN SCALE = " + scale )

      val procsRunning = procsRunningRef()
      val now = System.currentTimeMillis
      val toStop = procsRunning filter { rp =>
         rp.deathTime <= now || rp.context.scaleStart > scale || rp.context.scaleStop < scale
      }
      var newRunning = procsRunning -- toStop
      toStop foreach { rp => xfade( exprand( rp.context.minFade, rp.context.maxFade )) {
         if( verbose ) println( "STOPPING OBSOLETE " + rp )
         rp.proc.dispose // stop
      }}
      val (minProcs0, maxProcs0) = newRunning.foldLeft( (1, Int.MaxValue) ) { (v, rp) =>
         val (min, max) = v
         (math.max( min, rp.context.minConc ), math.min( max, rp.context.maxConc ))
      }
      val (minProcs, maxProcs) = if( toStop.nonEmpty ) {
         val num = rrand( math.min( minProcs0, maxProcs0 ), maxProcs0 )
         (rrand( minProcs0, num ), rrand( num, maxProcs0 ))
      } else (math.min( minProcs0, maxProcs0 ), maxProcs0)
      if( verbose ) println( "MIN = " + minProcs + " / MAX = " +maxProcs + " / CURRENT = " + newRunning.size )
      var keepGoing = true
      while( (newRunning.size > maxProcs) && keepGoing ) {
         val filtered = newRunning filter { rp => (now - rp.startTime) * 0.001 > rp.context.minDur }
         if( filtered.nonEmpty ) {
            val weightSum = newRunning.foldLeft( 0.0 ) { (sum, rp) => sum + rp.context.weight }
            val rp = wchoose( newRunning ) { rp => rp.context.weight / weightSum }
            if( verbose ) println( "STOPPING (CROWDED) " + rp )
            xfade( exprand( rp.context.minFade, rp.context.maxFade )) { rp.proc.dispose /* stop */}
            newRunning -= rp
         } else {
            keepGoing = false
         }
      }
      keepGoing = true
      while( (newRunning.size < minProcs) && keepGoing ) {
         val notRunning = (Material.all.toSet -- newRunning.map( _.context )).filter( c =>
            c.scaleStart <= scale && c.scaleStop >= scale )
         keepGoing = notRunning.nonEmpty
         if( keepGoing ) {
            val weightSum  = notRunning.foldLeft( 0.0 ) { (sum, c) => sum + c.weight }
            val c       = wchoose( notRunning ) { _.weight / weightSum }
            val f       = c.settings.createProcFactory( c.name )
            val death   = (exprand( c.minDur, c.maxDur ) * 1000).toLong + now
            val proc    = f.make
            c.settings.prepareForPlay( proc )
            val rp      = RunningProc( proc, c, now, death )
            if( verbose ) println( "STARTING (SPARSE) " + rp )
            xfade( exprand( rp.context.minFade, rp.context.maxFade )) {
               proc ~> CupolaNuages.fieldCollectors( rp.context.field )
               proc.play
            }
            newRunning += rp
         }
      }
      procsRunningRef.set( newRunning )
   }

   def stageChange( oldStage: Option[ Double ], newStage: Option[ Double ])( implicit tx: ProcTxn ) {
      newStage foreach { scale =>
         val newTime = System.currentTimeMillis * 0.001 // tx.time
//println( "AQUI " + oldStage + " / " + newStage )
         if( oldStage.isEmpty ) {
            lastTimeRef.set( newTime )
            startTimeRef.set( newTime )
         }
         val oldTime = lastTimeRef.swap( newTime )
         val dt = newTime - oldTime
         val oldScale = lastScaleRef.swap( scale )
         scaleAccumRef += dt * oldScale
      }
      validRef.set( newStage.isDefined )
   }

   case class RunningProc( proc: Proc, context: SoundContext, startTime: Long, deathTime: Long )
}