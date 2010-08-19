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

import Cupola._
import de.sciss.synth.proc._
import DSL._
import java.util.{TimerTask, Timer}
import collection.immutable.{ Set => ISet }

/**
 *    @version 0.12, 17-Aug-10
 */
object ProcessManager {
   import Util._
   
   var verbose = false

   private val stopAndDisposeListener = new Proc.Listener {
      def updated( u: Proc.Update ) {
         if( !u.state.fading && (!u.state.playing || u.state.bypassed) ) {
            // XXX workaround: CCSTM still has the previous txn visible,
            // hence we need to wait a bit longer :-(
//EventQueue.invokeLater { new Runnable { def run {
            if( verbose ) println( "" + new java.util.Date() + " FINAL-DISPOSE " + u.proc )
            disposeProc( u.proc ) // ProcTxn.atomic { implicit tx => }
//}}}
         }
      }
   }

   private def disposeProc( proc: Proc ) {
      ProcTxn.atomic { implicit t =>
         proc.anatomy match {
            case ProcFilter => disposeFilter( proc )
            case _ => disposeGenDiff( proc )
         }
      }
   }

   // XXX copied from Nuages. we should have this going into SoundProcesses directly somehow
   private def disposeFilter( proc: Proc )( implicit tx: ProcTxn ) {
      val in   = proc.audioInput( "in" )
      val out  = proc.audioOutput( "out" )
      val ines = in.edges.toSeq
      val outes= out.edges.toSeq
      if( ines.size > 1 ) println( "WARNING : Filter is connected to more than one input!" )
      if( verbose && outes.nonEmpty ) println( "" + new java.util.Date() + " " + out + " ~/> " + outes.map( _.in ))
      outes.foreach( oute => {
//         if( verbose ) println( "" + out + " ~/> " + oute.in )
         out ~/> oute.in
      })
      ines.headOption.foreach( ine => {
         if( verbose ) println( "" + new java.util.Date() + " " + ine.out + " ~> " + outes.map( _.in ))
         outes.foreach( oute => {
//            if( verbose ) println( "" + ine.out + " ~> " + oute.in )
            ine.out ~> oute.in
         })
      })
      // XXX tricky: this needs to be last, so that
      // the pred out's bus isn't set to physical out
      // (which is currently not undone by AudioBusImpl)
      if( verbose && ines.nonEmpty ) println( "" + new java.util.Date() + " " + ines.map( _.out ) + " ~/> " + in ) 
      ines.foreach( ine => {
         ine.out ~/> in
      })
      proc.dispose
   }

   // XXX copied from Nuages. we should have this going into SoundProcesses directly somehow
   private def disposeGenDiff( proc: Proc )( implicit tx: ProcTxn ) {
//      val toDispose = MSet.empty[ Proc ]
//      addToDisposal( toDispose, proc )
//      toDispose.foreach( p => {
val p = proc
         val ines = p.audioInputs.flatMap( _.edges ).toSeq // XXX
         val outes= p.audioOutputs.flatMap( _.edges ).toSeq // XXX
         outes.foreach( oute => oute.out ~/> oute.in )
         ines.foreach( ine => ine.out ~/> ine.in )
         p.dispose
//      })
   }
   
   private def stopAndDispose( rp: RunningProc )( implicit tx: ProcTxn ) {
      val p     = rp.proc
      val state = p.state
//println( "STOP-AND-DISPOSE " + p + " -> " + state + " / " + tx.transit )
      if( !state.fading && (!state.playing || state.bypassed || (tx.transit == Instant)) ) {
//println( ".......INSTANT" )
         p.dispose
      } else {
         p.addListener( stopAndDisposeListener )
         p.anatomy match {
            case ProcFilter => {
//println( ".......BYPASS" )
               p.bypass
            }
            case _ => {
//println( ".......STOP " + (new java.util.Date()) )
               p.stop
            }
         }
      }
   }

   abstract class GroupManager( name: String, contextSet: Set[ SoundContext ]) {
//      var verbose = true
      protected val procsRunningRef = Ref( Set.empty[ RunningProc ])

      protected val startTimeRef   = Ref( 0.0 )
      protected val lastTimeRef    = Ref( 0.0 )
      protected val lastScaleRef   = Ref( 0.0 )
      protected val scaleAccumRef  = Ref( 0.0 )

      protected val validRef = Ref( false )

      protected def insertAndPlay( newRunning: ISet[ RunningProc ], rp: RunningProc, fdt: Double )( implicit tx: ProcTxn ) : Unit

      def stageChange( oldStage: Option[ Double ], newStage: Option[ Double ])( implicit tx: ProcTxn ) {
         newStage foreach { scale =>
            val newTime = System.currentTimeMillis * 0.001 // tx.time
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

      def update {
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
//         if( verbose ) println( "MEAN SCALE = " + scale )

         val procsRunning = procsRunningRef()
         val now = System.currentTimeMillis
         val toStop = procsRunning filter { rp =>
            rp.deathTime <= now || rp.context.scaleStart > scale || rp.context.scaleStop < scale
         }
         var newRunning = procsRunning -- toStop
         toStop foreach { rp => xfade( exprand( rp.context.minFade, rp.context.maxFade )) {
            if( verbose ) println( "" + new java.util.Date() + " STOPPING OBSOLETE " + rp )
            stopAndDispose( rp )
   //         rp.proc.dispose // stop
         }}
         val (minProcs0, maxProcs0) = newRunning.foldLeft( (1, Int.MaxValue) ) { (v, rp) =>
            val (min, max) = v
            (math.max( min, rp.context.minConc ), math.min( max, rp.context.maxConc ))
         }
         val (minProcs, maxProcs) = if( toStop.nonEmpty ) {
            val num = rrand( math.min( minProcs0, maxProcs0 ), maxProcs0 )
            (rrand( minProcs0, num ), rrand( num, maxProcs0 ))
         } else (math.min( minProcs0, maxProcs0 ), maxProcs0)
   //val (minProcs, maxProcs) = (2, 2)
//         if( verbose ) println( "MIN = " + minProcs + " / MAX = " +maxProcs + " / CURRENT = " + newRunning.size )
         var keepGoing = true
         while( (newRunning.size > maxProcs) && keepGoing ) {
            val filtered = newRunning filter { rp => (now - rp.startTime) * 0.001 > rp.context.minDur }
            if( filtered.nonEmpty ) {
               val weightSum = newRunning.foldLeft( 0.0 ) { (sum, rp) => sum + rp.context.weight }
               val rp = wchoose( newRunning ) { rp => rp.context.weight / weightSum }
               if( verbose ) println( "" + new java.util.Date() + " STOPPING (CROWDED) " + rp )
               xfade( exprand( rp.context.minFade, rp.context.maxFade )) {
                  stopAndDispose( rp )
   //               rp.proc.dispose /* stop */
               }
               newRunning -= rp
            } else {
               keepGoing = false
            }
         }
         keepGoing = true
         while( (newRunning.size < minProcs) && keepGoing ) {
            val notRunning = (contextSet -- newRunning.map( _.context )).filter( c =>
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
               if( verbose ) println( "" + new java.util.Date() + " STARTING (SPARSE) " + rp )
               insertAndPlay( newRunning, rp, exprand( rp.context.minFade, rp.context.maxFade ))
               newRunning += rp
            }
         }
         procsRunningRef.set( newRunning )
      }
   }

   case class RunningProc( proc: Proc, context: SoundContext, startTime: Long, deathTime: Long )
}

class ProcessManager {
   import Util._
   import ProcessManager._
   
   val timer = new Timer()

   val inputManager = new GroupManager( "Input", Material.all.toSet ) {
      protected def insertAndPlay( newRunning: ISet[ RunningProc ], rp: RunningProc, fdt: Double )( implicit tx: ProcTxn ) {
         rp.proc ~> CupolaNuages.fieldCollectors( rp.context.field )
         xfade( fdt ) { rp.proc.play }
      }
   }
   val inputTask = {
      val res = new TimerTask {
         def run = inputManager.update
      }
      timer.schedule( res, 1993, 1993 ) // 300th prime
      res
   }

   val filterManager = {
      val res = new GroupManager( "Filter", Filters.all.toSet ) {
         protected def insertAndPlay( newRunning: ISet[ RunningProc ], rp: RunningProc, fdt: Double )( implicit tx: ProcTxn ) {
            val outProc = choose( newRunning.map( _.proc ) + CupolaNuages.fieldCollectors( rp.context.field ))
            val inProc  = outProc.audioOutput( "out" ).edges.head.targetVertex
            outProc ~|rp.proc|> inProc
            rp.proc.bypass
            rp.proc.play
            xfade( fdt ) { rp.proc.engage }
         }
      }
//      res.verbose = true
      res
   }

   val filterTask = {
      val res = new TimerTask {
         def run = filterManager.update
      }
      timer.schedule( res, 2131, 2131 ) // 320th prime
      res
   }

   def stageChange( oldStage: Option[ Double ], newStage: Option[ Double ])( implicit tx: ProcTxn ) {
      inputManager.stageChange( oldStage, newStage )
      filterManager.stageChange( oldStage, newStage )
   }
}