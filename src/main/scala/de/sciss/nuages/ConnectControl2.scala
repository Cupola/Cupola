package de.sciss.nuages

import prefuse.controls.ControlAdapter
import prefuse.Display
import prefuse.visual.{NodeItem, VisualItem}
import java.awt.event.MouseEvent
import prefuse.util.display.PaintListener
import prefuse.util.ColorLib
import java.awt.{Color, BasicStroke, Paint, Graphics2D}
import java.awt.geom.{Line2D, Point2D}
import de.sciss.synth.proc.{ProcTxn, ProcParamAudioInput, ProcParamAudioOutput}

class ConnectControl2 extends ControlAdapter with PaintListener {
   control =>
   
   import NuagesPanel._

   private case class DragSource( vi: VisualItem, visual: VisualAudioOutput )
   private case class DragTarget( vi: VisualItem, visual: VisualAudioInput )
   private class Drag( val source: DragSource,
                       val targetLoc: Point2D, var target: Option[ DragTarget ])

   private var drag: Option[ Drag ] = None

   def prePaint( d: Display, g: Graphics2D ) {}

   def postPaint( d: Display, g: Graphics2D ) {
      drag.foreach( dr => {
         g.setColor( if( dr.target.isDefined ) Color.green else Color.red )
         val tgtX = dr.target.map( _.vi.getX() ).getOrElse( dr.targetLoc.getX() )
         val tgtY = dr.target.map( _.vi.getY() ).getOrElse( dr.targetLoc.getY() )
         val srcX = dr.source.vi.getX()
         val srcY = dr.source.vi.getY()
         val lin  = new Line2D.Double( srcX, srcY, tgtX, tgtY )
         val trns = d.getTransform
         val shp  = trns.createTransformedShape( lin )
         g.draw( shp )
      })
   }

   override def itemPressed( vi: VisualItem, e: MouseEvent ) {
//      if( !e.isControlDown() ) return
      if( !e.isShiftDown() ) return
      vi match {
         case ni: NodeItem => {
            val data = ni.get( COL_NUAGES ).asInstanceOf[ VisualData ]
            if( data == null ) return
            data match {
               case vBus: VisualAudioOutput => {
                  val d          = getDisplay( e )
                  val displayPt  = d.getAbsoluteCoordinate( e.getPoint(), null )
                  val dr = new Drag( DragSource( vi, vBus ), displayPt, None )
                  d.addPaintListener( control )
                  drag = Some( dr )
               }
               case _ =>
            }
         }
         case _ =>
      }
   }

   /**
    *    Bug in Prefuse: With Ctrl+Mouse we loose
    *    the item. So make sure we continue to track
    *    the motion and eventually kill the edge
    */
   override def mouseMoved( e: MouseEvent ) {
      checkDrag( e )
   }

   /**
    *    Bug in Prefuse: With Ctrl+Mouse we loose
    *    the item. So make sure we continue to track
    *    the motion and eventually kill the edge
    */
   override def mouseReleased( e: MouseEvent ) {
      checkRelease( e )
   }

   override def itemDragged( vi: VisualItem, e: MouseEvent ) {
      checkDrag( e )
   }

   private def checkDrag( e: MouseEvent ) {
      drag.foreach( dr => {
         val d          = getDisplay( e )
         val screenPt   = e.getPoint()
         d.getAbsoluteCoordinate( screenPt, dr.targetLoc )
         val vi         = d.findItem( screenPt )
         val tgt        = vi match {
            case ni: NodeItem => {
               if( vi != null ) {
                  val data = vi.get( COL_NUAGES ).asInstanceOf[ VisualData ]
                  if( data == null ) None
                  else data match {
                     case vBus: VisualAudioInput if( vBus.bus.proc != dr.source.visual.bus.proc ) =>
                        Some( DragTarget( vi, vBus ))
                     case _ => None
                  }
               } else None
            }
            case _ => None
         }
         if( tgt != dr.target ) {
            dr.target.foreach( _.vi.setFixed( false ))
            dr.target = tgt
            dr.target.foreach( _.vi.setFixed( true ))
         }
      })
   }

   override def itemReleased( vi: VisualItem, e: MouseEvent ) {
      checkRelease( e )
   }

   private def checkRelease( e: MouseEvent ) {
      drag.foreach( dr => {
//println( "REMOVE EDGE" )
//         g.removeEdge( edge )
         val d = getDisplay( e )
         d.removePaintListener( control )
         drag = None
         dr.target.foreach( tgt => {
            tgt.vi.setFixed( false )
            ProcTxn.atomic { implicit t => dr.source.visual.bus.~>( tgt.visual.bus )}
         })
      })
   }

   @inline private def getDisplay( e: MouseEvent ) = e.getComponent().asInstanceOf[ Display ]
}