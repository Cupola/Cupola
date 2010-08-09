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
class ProcessManager {
   private val rnd            = new Random()
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
      println( "MEAN SCALE = " + scale )

      val procsRunning = procsRunningRef()
      val toStop = procsRunning filter { rp => rp.context.scaleStart > scale || rp.context.scaleStop < scale }
      var newRunning = procsRunning -- toStop
      toStop foreach { rp => xfade( exprand( 7, 21 )) {
println( "STOPPING OBSOLETE " + rp )
         rp.proc.dispose // stop
      }}
      val (minProcs, maxProcs) = newRunning.foldLeft( (1, Int.MaxValue) ) { (v, rp) =>
         val (min, max) = v
         (math.max( min, rp.context.minConc ), math.min( max, rp.context.maxConc ))
      }
println( "MIN = " + minProcs + " / MAX = " +maxProcs + " / CURRENT = " + newRunning.size )
      while( newRunning.size > maxProcs ) {
         val weightSum = newRunning.foldLeft( 0.0 ) { (sum, rp) => sum + rp.context.weight }
         val rp = wchoose( newRunning ) { rp => rp.context.weight / weightSum }
println( "STOPPING (CROWDED) " + rp )
         xfade( exprand( 7, 21 )) { rp.proc.dispose /* stop */}
         newRunning -= rp
      }
      var keepGoing = true
      while( (newRunning.size < minProcs) && keepGoing ) {
         val notRunning = (Material.all.toSet -- newRunning.map( _.context )).filter( c =>
            c.scaleStart <= scale && c.scaleStop >= scale )
         keepGoing = notRunning.nonEmpty
         if( keepGoing ) {
            val weightSum  = notRunning.foldLeft( 0.0 ) { (sum, c) => sum + c.weight }
            val c    = wchoose( notRunning ) { _.weight / weightSum }
            val f    = c.settings.createProcFactory( c.name )
            val proc = f.make
            val rp   = RunningProc( proc, c )
println( "STARTING (SPARSE) " + rp )
            xfade( exprand( 7, 21 )) { proc.play }
            newRunning += rp
         }
      }
      procsRunningRef.set( newRunning )
   }

   def wchoose[ T ]( seq: Traversable[ T ])( fun: T => Double ) : T = {
      val i    = rnd.nextDouble
      var sum  = 0.0
      seq find { e => sum += fun( e ); sum >= i } getOrElse seq.last   
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

//      if( oldStage._1 != newStage._1 ) {
//         val psOff = oldStage._1 match {
//            case Meditation  => CupolaNuages.meditProcs
//            case Equilibrium => CupolaNuages.equivProcs
//            case Chaos       => CupolaNuages.chaosProcs
//            case _           => Nil
//         }
//         psOff.foreach( p => xfade( exprand( 7, 21 )) { p.stop })
//         val psOn = newStage._1 match {
//            case Meditation  => CupolaNuages.meditProcs
//            case Equilibrium => CupolaNuages.equivProcs
//            case Chaos       => CupolaNuages.chaosProcs
//            case _           => Nil
//         }
//         psOn.foreach( p => xfade( exprand( 7, 21 )) {
//            p.anatomy match {
//               case ProcGen => p.control( "pos" ).v = rrand( 0, 1 )
//               case _ =>
//            }
//            p.play
//         })
//      }
   }

   private def exprand( lo: Double, hi: Double ) : Double = {
      lo * math.exp( math.log( hi / lo ) * rnd.nextDouble )
   }

   private def rrand( lo: Double, hi: Double ) : Double = {
      rnd.nextDouble() * (hi - lo) + lo
   }

   case class RunningProc( proc: Proc, context: SoundContext )
}