/*
 *  ClickControl.scala
 *  (Wolkenpumpe)
 *
 *  Copyright (c) 2008-2010 Hanns Holger Rutz. All rights reserved.
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

package de.sciss.nuages

import prefuse.controls.ControlAdapter
import java.awt.event.MouseEvent
import prefuse.Display
import javax.swing.{ListModel, ListSelectionModel}
import java.awt.geom.Point2D
import de.sciss.synth.proc.{Proc, ProcTxn, ProcFactory}

/**
 *    Simple interface to query currently selected
 *    proc factory and to feedback on-display positions
 *    for newly created procs.
 * 
 *    Methods are guaranteed to be called in the awt
 *    event thread. 
 */
trait ProcFactoryProvider {
   def factory: Option[ ProcFactory ]
   def setLocationHint( p: Proc, loc: Point2D )
}

class ClickControl( pfp: ProcFactoryProvider ) extends ControlAdapter {
   override def mousePressed( e: MouseEvent ) {
      if( e.getClickCount() != 2 ) return
      pfp.factory match {
         case Some( pf ) => {
            val d          = getDisplay( e )
            val displayPt  = d.getAbsoluteCoordinate( e.getPoint(), null )
            ProcTxn.atomic { implicit tx =>
               val p = pf.make
               tx.beforeCommit( _ => pfp.setLocationHint( p, displayPt ))
            }
         }
         case None =>
      }
   }

   @inline private def getDisplay( e: MouseEvent ) = e.getComponent().asInstanceOf[ Display ]
}