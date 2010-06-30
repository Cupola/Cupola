package de.sciss.nuages

import prefuse.controls.ControlAdapter
import java.awt.event.MouseEvent
import prefuse.data.{Graph, Edge, Node => PNode}
import prefuse.visual.{NodeItem, VisualGraph, EdgeItem, VisualItem}
import prefuse.{Visualization, Display}
import prefuse.util.ColorLib
import java.awt.BasicStroke

class ConnectControl( g: Graph, targetHandle: PNode, targetVI: VisualItem, vis: Visualization, group: String )
extends ControlAdapter {
   import NuagesPanel._

   private var dragEdge: Option[ Edge ] = None

   override def itemPressed( vi: VisualItem, e: MouseEvent ) {
println( "itemPressed" )
//      if( !e.isControlDown() ) return
      if( !e.isShiftDown() ) return
      vi match {
         case ni: NodeItem => {
            val data = ni.get( COL_NUAGES ).asInstanceOf[ VisualData ]
            if( data == null ) return
            data match {
               case vBus: VisualBus => {
println( "ADD EDGE" )
                  val d          = getDisplay( e )
                  val displayPt  = d.getAbsoluteCoordinate( e.getPoint(), null )
                  targetVI.setStartX( displayPt.getX() )
                  targetVI.setStartY( displayPt.getY() )
                  targetVI.setX( displayPt.getX() )
                  targetVI.setY( displayPt.getY() )
                  targetVI.setEndX( displayPt.getX() )
                  targetVI.setEndY( displayPt.getY() )
                  val edge       = g.addEdge( vBus.pNode, targetHandle ) // .asInstanceOf[ EdgeItem ]
                  val viEdge     = vis.getVisualItem( group, edge )
                  viEdge.setStrokeColor( ColorLib.rgb( 127, 127, 255 ))
                  viEdge.setStroke( new BasicStroke( 2f ))
//                  vis.run( action )
//            e.setVisible( true )
                  dragEdge = Some( edge )
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
      if( dragEdge.isEmpty ) return
      val d          = getDisplay( e )
      val displayPt  = d.getAbsoluteCoordinate( e.getPoint(), null )
      targetVI.setStartX( displayPt.getX() )
      targetVI.setStartY( displayPt.getY() )
      targetVI.setX( displayPt.getX() )
      targetVI.setY( displayPt.getY() )
      targetVI.setEndX( displayPt.getX() )
      targetVI.setEndY( displayPt.getY() )
//      action
   }

   override def itemEntered( vi: VisualItem, e: MouseEvent ) {
println( "itemEntered" )
   }

   override def itemExited( vi: VisualItem, e: MouseEvent ) {
println( "itemExited" )
   }

   override def itemReleased( vi: VisualItem, e: MouseEvent ) {
      checkRelease( e )
   }

   private def checkRelease( e: MouseEvent ) {
      dragEdge.foreach( edge => {
println( "REMOVE EDGE" )
         g.removeEdge( edge )
         dragEdge = None
      })
   }

   @inline private def getDisplay( e: MouseEvent ) = e.getComponent().asInstanceOf[ Display ]
}