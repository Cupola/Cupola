package de.sciss.nuages

import prefuse.controls.ControlAdapter
import javax.swing.SwingUtilities
import prefuse.visual.{AggregateItem, VisualItem}
import java.awt.Cursor
import java.awt.event.MouseEvent
import java.awt.geom.Point2D
import prefuse.{Visualization, Display}

object DragControl {
   private val csrHand     = Cursor.getPredefinedCursor( Cursor.HAND_CURSOR )
   private val csrDefault  = Cursor.getDefaultCursor()
}

class DragControl( vis: Visualization ) extends ControlAdapter {
   import DragControl._
   import NuagesPanel._

   private var hoverItem: Option[ VisualItem ]  = None
   private var drag: Option[ Drag ]             = None

   private class Drag( /* val vi: VisualItem,*/ val lastPt: Point2D ) {
      var started = false
   }

   override def itemEntered( vi: VisualItem, e: MouseEvent ) {
      vis.getRenderer( vi ) match {
         case pr: NuagesProcRenderer => {
            val data = vi.get( COL_NUAGES ).asInstanceOf[ VisualData ]
            if( data != null ) {
               val d          = getDisplay( e )
               val displayPt  = d.getAbsoluteCoordinate( e.getPoint(), null )
               data.update( pr.getShape( vi ))
               data.itemEntered( vi, e, displayPt )
            }
         }
         case _ =>
      }
      e.getComponent().setCursor( csrHand )
      hoverItem = Some( vi )
      if( !vi.isInstanceOf[ AggregateItem ]) setFixed( vi, true )
   }

   @inline private def getDisplay( e: MouseEvent ) = e.getComponent().asInstanceOf[ Display ]

   override def itemExited( vi: VisualItem, e: MouseEvent ) {
      vis.getRenderer( vi ) match {
         case pr: NuagesProcRenderer => {
            val data = vi.get( COL_NUAGES ).asInstanceOf[ VisualData ]
            if( data != null ) {
               val d          = getDisplay( e )
               val displayPt  = d.getAbsoluteCoordinate( e.getPoint(), null )
               data.update( pr.getShape( vi ))
               data.itemExited( vi, e, displayPt )
            }
         }
         case _ =>
      }
      hoverItem = None
      setFixed( vi, false )
      e.getComponent().setCursor( csrDefault )
   }

   override def itemPressed( vi: VisualItem, e: MouseEvent ) {
      val d          = getDisplay( e )
      val displayPt  = d.getAbsoluteCoordinate( e.getPoint(), null )
      vis.getRenderer( vi ) match {
         case pr: NuagesProcRenderer => {
            val data       = vi.get( COL_NUAGES ).asInstanceOf[ VisualData ]
            if( data != null ) {
               data.update( pr.getShape( vi ))
               if( data.itemPressed( vi, e, displayPt )) return // consumed
            }
         }
         case _ =>
      }
      if( !SwingUtilities.isLeftMouseButton( e ) || e.isShiftDown() ) return
      val dr   = new Drag( displayPt )
      drag     = Some( dr )
      if( vi.isInstanceOf[ AggregateItem ]) setFixed( vi, true )
   }

   override def itemReleased( vi: VisualItem, e: MouseEvent ) {
      vis.getRenderer( vi ) match {
         case pr: NuagesProcRenderer => {
            val data = vi.get( COL_NUAGES ).asInstanceOf[ VisualData ]
            if( data != null ) {
               val d          = getDisplay( e )
               val displayPt  = d.getAbsoluteCoordinate( e.getPoint(), null )
               data.update( pr.getShape( vi ))
               data.itemReleased( vi, e, displayPt )
            }
         }
         case _ =>
      }
      drag.foreach( dr => {
         setFixed( vi, false )
         drag = None
      })
   }

   override def itemDragged( vi: VisualItem, e: MouseEvent ) {
      val d          = getDisplay( e )
      val newPt      = d.getAbsoluteCoordinate( e.getPoint(), null )
      vis.getRenderer( vi ) match {
         case pr: NuagesProcRenderer => {
            val data       = vi.get( COL_NUAGES ).asInstanceOf[ VisualData ]
            if( data != null ) {
               data.update( pr.getShape( vi ))
               data.itemDragged( vi, e, newPt )
            }
         }
         case _ =>
      }
      drag.foreach( dr => {
         if( !dr.started ) dr.started = true
         val dx = newPt.getX() - dr.lastPt.getX()
         val dy = newPt.getY() - dr.lastPt.getY()
         move( vi, dx, dy )
         dr.lastPt.setLocation( newPt )
      })
   }

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