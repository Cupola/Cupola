package de.sciss.nuages

import java.awt.geom.Point2D
import java.awt.{Container, Color}
import javax.swing.{SwingUtilities, Icon}

abstract sealed class Port( icon: Icon ) extends Label( icon ) {
	setForeground( Color.white )
	setBackground( Color.black )

	def calcCenter( parent: Container ) : Point2D.Float
}

object Inlet {
   private val icon = new PortIcon( 17, 9, 0, 2 )
}
class Inlet extends Port( Inlet.icon ) {
	def calcCenter( parent: Container ) = {
		val inRect = SwingUtilities.convertRectangle( this.getParent(), this.getBounds(), parent )
		val tgtX	= inRect.x + inRect.width * 0.5f
		val tgtY	= inRect.y + 2 + inRect.height * 0.5f
		new Point2D.Float( tgtX, tgtY )
	}
}

object Outlet {
   private val icon = new PortIcon( 17, 9, 0, -6 )
}
class Outlet extends Port( Outlet.icon ) {
//   addMouseListener( new MouseAdapter() {
//      override def mousePressed( e: MouseEvent ) {
//         final Panel p = (Panel) SwingUtilities.getAncestorOfClass( Panel.class, Outlet.this );
//         if( p != null ) {
//            p.setDragOutlet( Outlet.this, e );
//         }
//      }
//   })

	def calcCenter( parent: Container ) = {
		val outRect	= SwingUtilities.convertRectangle( this.getParent(), this.getBounds(), parent )
		val srcX	= outRect.x + outRect.width * 0.5f
		val srcY	= outRect.y - 1 + outRect.height * 0.5f
		new Point2D.Float( srcX, srcY )
	}
}

object Sidelet {
   private val icon = new PortIcon( 11, 26, 2, 0 )
}
class Sidelet extends Port( Sidelet.icon ) {
	def calcCenter( parent: Container ) = {
		val inRect = SwingUtilities.convertRectangle( this.getParent(), this.getBounds(), parent )
		val tgtX	= inRect.x + inRect.width * 0.5f
		val tgtY	= inRect.y + inRect.height * 0.5f
		new Point2D.Float( tgtX, tgtY )
	}
}

