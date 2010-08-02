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
import de.sciss.synth.proc.{ DSL, ProcGen, ProcTxn}
import DSL._

/**
 *    @version 0.12, 02-Aug-10
 */
class ProcessManager {
   private val rnd = new Random()

   def stageChange( oldStage: (Level, Section), newStage: (Level, Section) )( implicit tx: ProcTxn ) {
      if( oldStage._1 != newStage._1 ) {
         val psOff = oldStage._1 match {
            case Meditation  => CupolaNuages.meditProcs
            case Equilibrium => CupolaNuages.equivProcs
            case Chaos       => CupolaNuages.chaosProcs
            case _           => Nil
         }
         psOff.foreach( p => xfade( exprand( 7, 21 )) { p.stop })
         val psOn = newStage._1 match {
            case Meditation  => CupolaNuages.meditProcs
            case Equilibrium => CupolaNuages.equivProcs
            case Chaos       => CupolaNuages.chaosProcs
            case _           => Nil
         }
         psOn.foreach( p => xfade( exprand( 7, 21 )) {
            p.anatomy match {
               case ProcGen => p.control( "pos" ).v = rrand( 0, 1 )
               case _ =>
            }
            p.play
         })
      }
   }

   private def exprand( lo: Double, hi: Double ) : Double = {
      lo * math.exp( math.log( hi / lo ) * rnd.nextDouble )
   }

   private def rrand( lo: Double, hi: Double ) : Double = {
      rnd.nextDouble() * (hi - lo) + lo
   }
}