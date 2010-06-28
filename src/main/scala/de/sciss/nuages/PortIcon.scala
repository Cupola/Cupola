package de.sciss.nuages

import javax.swing.Icon
import java.awt._

object PortIcon {
   private val colrPort1   = new Color( 0xFF, 0xFF, 0xFF, 0x07F )
   private val colrPort2   = new Color( 0xFF, 0xFF, 0xFF, 0x04F )
   private val strkPort    = new BasicStroke( 3f )
}
class PortIcon( w: Int, h: Int, hShift: Int, vShift: Int ) extends Icon {
   import PortIcon._

	def getIconWidth() : Int = w
	def getIconHeight() : Int = h

	def paintIcon( c: Component, g: Graphics, x: Int, y: Int ) {
		val g2 = g.asInstanceOf[ Graphics2D ]
		val atOrig	= g2.getTransform()
		g2.setRenderingHint( RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON )
		g2.translate( 2.5, 0.5 )
		g2.setColor( colrPort1 )
		g2.setStroke( strkPort )
		g2.drawOval( x + 1 + hShift, y + 1 + vShift, 9, 9 )
		g2.setColor( colrPort2 )
		g2.fillOval( x + 1 + hShift, y + 1 + vShift, 9, 9 )
		g2.setTransform( atOrig )
	}
}
