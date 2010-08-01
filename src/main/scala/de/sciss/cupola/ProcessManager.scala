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
import de.sciss.synth.proc.{ DSL, ProcTxn }
import DSL._

/**
 *    @version 0.11, 01-Aug-10
 */
class ProcessManager {
   def stageChange( oldStage: (Level, Section), newStage: (Level, Section) )( implicit tx: ProcTxn ) {
      if( oldStage._1 != newStage._1 ) xfade( 10 ) {
         if( oldStage._1 == Meditation ) {
            CupolaNuages.meditProcs.foreach( _.stop )
         } else if( newStage._1 == Meditation ) {
            CupolaNuages.meditProcs.foreach( _.play )
         }
      }
   }
}