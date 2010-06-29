/**
 *  The code on which this class is based, is released under
 *  a BSD style license:
 *
 *  Copyright (c) 2004-2007 Regents of the University of California.
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions
 *  are met:
 *
 *  1. Redistributions of source code must retain the above copyright
 *  notice, this list of conditions and the following disclaimer.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *  notice, this list of conditions and the following disclaimer in the
 *  documentation and/or other materials provided with the distribution.
 *
 *  3.  Neither the name of the University nor the names of its contributors
 *  may be used to endorse or promote products derived from this software
 *  without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 *  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 *  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 *  ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 *  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 *  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 *  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 *  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 *  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 *  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 *  SUCH DAMAGE.
 */

package de.sciss.nuages

import prefuse.controls.ControlAdapter
import javax.swing.SwingUtilities
import prefuse.visual.{AggregateItem, VisualItem}
import java.awt.Cursor
import prefuse.Display
import java.awt.event.MouseEvent
import java.awt.geom.Point2D
/**
 *    Scala version of the AggregateDragControl by Jeffrey Heer
 *    (Prefuse Demos).
 */
object PrefuseAggregateDragControl {
   // recursive over aggregate items
   private def setFixed( vi: VisualItem, fixed: Boolean ) {
      vi match {
         case ai: AggregateItem => {
            val iter = ai.items()
            while( iter.hasNext() ) {
               val vi2 = iter.next.asInstanceOf[ VisualItem ]
               setFixed( vi2, fixed )
            }
         }
         case _ => vi.setFixed( fixed )
      }
   }

   // recursive over aggregate items
   private def move( vi: VisualItem, dx: Double, dy: Double ) {
      vi match {
         case ai: AggregateItem => {
            val iter = ai.items()
            while( iter.hasNext() ) {
               val vi2 = iter.next.asInstanceOf[ VisualItem ]
               move( vi2, dx, dy )
            }
         }
         case _ => {
            val x = vi.getX()
            val y = vi.getY()
            vi.setStartX( x )
            vi.setStartY( y )
            vi.setX( x + dx )
            vi.setY( y + dy )
            vi.setEndX( x + dx )
            vi.setEndY( y + dy )
         }
      }
   }
}

class PrefuseAggregateDragControl extends ControlAdapter {
   import PrefuseAggregateDragControl._

   private var activeItem: VisualItem = null
   private val down                   = new Point2D.Double()
   private val temp                   = new Point2D.Double()
   private var dragged                = false

   override def itemEntered( vi: VisualItem, e: MouseEvent ) {
       val d = e.getComponent().asInstanceOf[ Display ]
       d.setCursor( Cursor.getPredefinedCursor( Cursor.HAND_CURSOR ))
       activeItem = vi
       if( !vi.isInstanceOf[ AggregateItem ]) setFixed( vi, true )
   }

   override def itemExited( vi: VisualItem, e: MouseEvent ) {
      if( activeItem eq vi ) {
         activeItem = null
         setFixed( vi, false )
      }
      val d = e.getComponent().asInstanceOf[ Display ]
      d.setCursor( Cursor.getDefaultCursor() )
   }

   override def itemPressed( vi: VisualItem, e: MouseEvent ) {
      if( !SwingUtilities.isLeftMouseButton( e )) return
      dragged = false
      val d = e.getComponent().asInstanceOf[ Display ]
      d.getAbsoluteCoordinate( e.getPoint(), down )
      if( vi.isInstanceOf[ AggregateItem ]) setFixed( vi, true )
   }

   override def itemReleased( vi: VisualItem, e: MouseEvent ) {
      if( !dragged || !SwingUtilities.isLeftMouseButton( e )) return
      activeItem = null
      setFixed( vi, false )
      dragged = false
   }

   override def itemDragged( vi: VisualItem, e: MouseEvent ) {
      if( !SwingUtilities.isLeftMouseButton( e )) return
      dragged = true
      val d = e.getComponent().asInstanceOf[ Display ]
      d.getAbsoluteCoordinate( e.getPoint(), temp )
      val dx = temp.getX() - down.getX()
      val dy = temp.getY() - down.getY()
      move( vi, dx, dy )
      down.setLocation( temp )
   }
}