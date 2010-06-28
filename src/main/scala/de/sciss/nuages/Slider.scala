package de.sciss.nuages

import javax.swing.{BorderFactory, SwingConstants}
import java.awt._

/**
 *    @version 0.12, 28-Jun-10
 */
object Slider {
   private val colrTrack	= new Color( 0x40, 0x40, 0xFF, 0xA0 )
   private val colrTrackM	= new Color( 0xFF, 0x40, 0x40, 0xA0 )
   private val colrDrag	   = Color.red
}
class Slider extends Label { // , MetaDataGenerator
   import Slider._
   import SwingConstants._

   private var orientationVar = HORIZONTAL
   private var valueVar       = 0f
   private var dragValue      = 0f
   private var dragShown      = false
   private var	mappedVar      = false
// private final MetaDataWrapper	mdw;
   private var centeredVar    = false
   private val ins   	      = new Insets( 0, 0, 0, 0 )

   // ---- constructor ----
	{
//		mdw = new MetaDataWrapper( this, new Object[] { "sli", -1 });
		setPreferredSize( new Dimension( 64, 24 ))
		setBorder( BorderFactory.createCompoundBorder(
		    BorderFactory.createMatteBorder( 1, 1, 1, 1, Color.white ),
		    BorderFactory.createEmptyBorder( 1, 1, 1, 1 )))
	}

//	def setID( Object id )
//	{
//		mdw.getPrepend()[ 1 ] = id;
//		setText( id.toString() );
//	}

   def centered : Boolean = centeredVar
	def centered_=( onOff: Boolean ) {
		if( onOff != centeredVar ) {
			centeredVar = onOff
			repaint()
		}
	}

//	public Object[] createMetaData( Point2D pt )
//	{
//		return mdw.createMetaData( pt );
//	}

   def orientation : Int = orientationVar
	def orientation_=( orient: Int ) {
		if( orient != orientationVar ) {
			orientationVar = orient
			repaint()
		}
	}

   def value : Float = valueVar
	def value_=( v: Float ) {
		if( v != valueVar ) {
			valueVar = v
			repaint()
		}
	}

//	public void setDragValue( float value )
//	{
//		if( value != dragValue ) {
//			dragValue = value;
//			if( dragShown ) repaint();
//		}
//	}
//
//	public void setDragShown( boolean onOff )
//	{
//		if( onOff != dragShown ) {
//			dragShown = onOff;
//			repaint();
//		}
//	}

   def mapped : Boolean = mappedVar
	def mapped_=( onOff: Boolean ) {
		if( onOff != mappedVar ) {
			mappedVar = onOff
			repaint()
		}
	}

	override def paintComponent( g: Graphics ) {
		super.paintComponent( g )

		val g2 = g.asInstanceOf[ Graphics2D ]

		getInsets( ins )

		val x = ins.left
		val y = ins.top
		val w = getWidth() - (ins.left + ins.right)
		val h = getHeight() - (ins.top + ins.bottom)

		val extenti = if( orientationVar == HORIZONTAL ) w else h
		val posi    = ((if( orientationVar == HORIZONTAL ) value else (1f - value)) * extenti + 0.5f).toInt
		val offi    = if( centeredVar ) extenti >> 1 else if( orientationVar == HORIZONTAL ) 0 else extenti
		val dposi   = if( dragShown ) {
         ((if( orientationVar == HORIZONTAL ) dragValue else (1f - dragValue)) * extenti + 0.5f).toInt
      } else 0

		g2.setColor( if( mapped ) colrTrackM else colrTrack )

		if( orientationVar == HORIZONTAL ) {
			g2.fillRect( x + math.min( posi, offi ), y, math.abs( posi - offi ), h )
			if( dragShown ) {
				g2.setColor( colrDrag )
				g2.drawLine( x + dposi, y, x + dposi, y + h - 1 )
			}
		} else {
			g2.fillRect( x, y + math.min( posi, offi ), w, math.abs( posi - offi ))
			if( dragShown ) {
				g2.setColor( colrDrag )
				g2.drawLine( x, y + dposi, x + w - 1, y + dposi )
			}
		}
	}
}
