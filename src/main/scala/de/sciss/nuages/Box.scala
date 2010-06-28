package de.sciss.nuages

import java.awt._
import javax.swing.border.{AbstractBorder, Border}
import javax.swing.{JPanel, BorderFactory, BoxLayout, JComponent}
import collection.immutable.{ IndexedSeq => IIdxSeq }

/**
 *    @version 0.14, 28-Jun-10
 */
object Box {
   val FLAG_PLAY	= 0x01
   val FLAG_REC	= 0x02
   val FLAG_CTRL	= 0x04

   private val colrPlay	   = new Color( 0x00, 0xA0, 0x00 )
   private val strkDashed	= new BasicStroke( 1f, BasicStroke.CAP_SQUARE,
                                              BasicStroke.JOIN_MITER, 10f,
                                              Array( 2f, 2f ), 0f )
}

class Box( name: String, flags: Int, numInlets: Int, numOutlets: Int )
extends JComponent { // with implements Disposable, MetaDataGenerator
   import Box._

	private val lab = new Label( name )
   private val buttonBox = new JPanel( new GridLayout( 1, 0, 0, 0 ))

//	private PeakMeter				meter		= null;
//	private final MetaDataWrapper	mdw;
	private val mainBox		= new JPanel( new BorderLayout( 0, 0 ))
	private val (inlets, inletBox) = if( numInlets > 0 ) {
      val res = new JPanel( null )
      res.setOpaque( false )
      res.setLayout( new BoxLayout( res, BoxLayout.X_AXIS ))
      res.add( javax.swing.Box.createHorizontalGlue() )
      val inlets: IIdxSeq[ Inlet ] = (0 until numInlets).map( i => {
         val inlet = new Inlet() // ( i )
         res.add( inlet )
         res.add( javax.swing.Box.createHorizontalGlue() )
         inlet
      })
      add( res, BorderLayout.NORTH )
      (inlets, res)
   } else (Vector.empty[ Inlet ], null)

	private val (outlets, outletBox) = if( numOutlets > 0 ) {
      val res = new JPanel( null )
      res.setOpaque( false )
      res.setLayout( new BoxLayout( res, BoxLayout.X_AXIS ))
      res.add( javax.swing.Box.createHorizontalGlue() )
      val outlets: IIdxSeq[ Outlet ] = (0 until numOutlets).map( i => {
         val outlet = new Outlet() // ( i )
         res.add( outlet )
         res.add( javax.swing.Box.createHorizontalGlue() )
         outlet
      })
      add( res, BorderLayout.SOUTH )
      (outlets, res)
   } else (Vector.empty[ Outlet ], null)

	private var sideletBox : JComponent	= null
	private val attrBox     = new JPanel( new GridLayout( 0, 1, 2, 2 ))
	private var attrMap	   = Map.empty[ String, Slider ]
	private val lbPlay      = if( (flags & FLAG_PLAY) != 0 ) {
		val res = new Label( "P" )
	   res.setBorder( BorderFactory.createCompoundBorder( BorderFactory.createEmptyBorder( 0, 1, 0, 1 ),
			            BorderFactory.createCompoundBorder( BorderFactory.createMatteBorder( 1, 1, 1, 1, Color.white ),
			            BorderFactory.createEmptyBorder( 0, 2, 0, 2 ))))
	   res.setOpaque( true )
      res.setForeground( Color.white )
      res.setBackground( Color.black )
      buttonBox.add( res )
      res
   } else null

	private val lbRec       = if( (flags & FLAG_REC) != 0 ) {
		val res = new Label( "R" );
	   res.setBorder( BorderFactory.createCompoundBorder( BorderFactory.createEmptyBorder( 0, 1, 0, 1 ),
			            BorderFactory.createCompoundBorder( BorderFactory.createMatteBorder( 1, 1, 1, 1, Color.white ),
			            BorderFactory.createEmptyBorder( 0, 2, 0, 2 ))))
		res.setOpaque( true )
      res.setForeground( Color.white )
      res.setBackground( Color.black )
      buttonBox.add( res )
      res
   } else null

	private val lbSolo	   = if( (flags & FLAG_CTRL) == 0 ) {
		val res = new Label( "S" );
		res.setBorder( BorderFactory.createCompoundBorder( BorderFactory.createEmptyBorder( 0, 1, 0, 1 ),
			            BorderFactory.createCompoundBorder( BorderFactory.createMatteBorder( 1, 1, 1, 1, Color.white ),
			            BorderFactory.createEmptyBorder( 0, 2, 0, 2 ))))
	   res.setOpaque( true )
      res.setForeground( Color.white )
      res.setBackground( Color.black )
      buttonBox.add( res )
      res
   } else null

	private val border         = new Border( (flags & FLAG_CTRL) != 0 )
	private var sidelets	      = Vector.empty[ Sidelet ]
	private val bdTop          = if( numInlets == 0 ) 0 else 13
   private val bdBottom       = if( numOutlets == 0 ) 0 else 13
	private var bdLeft		   = 0
	private val bdRight		   = 0

	private var numAttr		   = 0
	private var selected	      = false

	private var prefCenter: Point = null

	def this( name: String, numInlets: Int, numOutlets: Int ) {
		this( name, 0, numInlets, numOutlets )
	}

   // ---- constructor ----
	{
//		mdw		= new MetaDataWrapper( this, new Object[] { "box", -1 });

		setOpaque( false )
		setLayout( new BorderLayout() )
		lab.setForeground( Color.white )
		lab.setBorder( BorderFactory.createEmptyBorder( 0, 2, 0, 2 ))
		setForeground( Color.white )

		attrBox.setOpaque( false )
		mainBox.setOpaque( false )

		mainBox.add( lab, BorderLayout.CENTER )
		mainBox.add( attrBox, BorderLayout.SOUTH )
		mainBox.add( buttonBox, BorderLayout.EAST )
		add( mainBox, BorderLayout.CENTER )
	}

	def getVisibleBounds( r: Rectangle ) {
		r.x			= getX() + bdLeft
		r.y			= getY() + bdTop
		r.width		= getWidth() - (bdLeft + bdRight)
		r.height	   = getHeight() - (bdTop + bdBottom)
	}

	def getInlet( idx: Int ) : Inlet       = inlets( idx )
	def getOutlet( idx: Int ) : Outlet     = outlets( idx )
	def getSidelet( idx: Int ) : Sidelet   = sidelets( idx )

   def numAttrs : Int = attrMap.size

	def setPlay( onOff: Boolean ) {
		lbPlay.setForeground( if( onOff ) Color.white else Color.white )
		lbPlay.setBackground( if( onOff ) colrPlay else Color.black )
	}

	def setRec( onOff: Boolean ) {
		lbRec.setForeground( if( onOff ) Color.white else Color.white )
		lbRec.setBackground( if( onOff ) Color.red else Color.black )
	}

	def setSolo( onOff: Boolean ) {
		lbSolo.setForeground( if( onOff ) Color.black else Color.white )
		lbSolo.setBackground( if( onOff ) Color.yellow else Color.black )
	}

	def setSelected( onOff: Boolean ) {
		if( selected != onOff ) {
			selected = onOff
			lab.setForeground( if( onOff ) Color.red else Color.white )
		}
	}

	def isSelected : Boolean = selected

	def addComponent( c: JComponent ) {
		attrBox.add( c )
		attrBox.setSize( attrBox.getPreferredSize() )
	}

	def addAttr( name: String, centered: Boolean, value: Float, canMap: Boolean ) {
		val slid = new Slider
//		slid.setID( name )
		slid.centered  = centered
		slid.value     = value
		slid.setForeground( getForeground() )
		attrMap += name -> slid
		attrBox.add( slid )
		attrBox.setSize( attrBox.getPreferredSize() )

		if( canMap ) {
			if( sideletBox == null ) {
				bdLeft = 15
				sideletBox = new JPanel( null )
				sideletBox.setOpaque( false )
				sideletBox.setLayout( new BoxLayout( sideletBox, BoxLayout.Y_AXIS ))
//            sideletBox.add( javax.swing.Box.createVerticalStrut( 32 + (if( meter == null ) 0 else 6 )))
				sideletBox.add( javax.swing.Box.createVerticalStrut( 32 ))
				for( i <- (0 until numAttr) ) {
					sideletBox.add( javax.swing.Box.createVerticalStrut( 26 ))
				}
				add( sideletBox, BorderLayout.WEST )
			}
		}
		val s = new Sidelet() // ( if( canMap ) numAttr else -1 )
		sidelets :+= s
		if( canMap ) {
			sideletBox.add( s )
		} else if( sideletBox != null ) {
			sideletBox.add( javax.swing.Box.createVerticalStrut( 26 ))
		}

		numAttr += 1
	}

	def updateAttr( name: String, value: Float ) {
		attrMap.get( name ).foreach( _.value = value )
	}

	def setMapped( name: String, onOff: Boolean ) {
		attrMap.get( name ).foreach( _.mapped = onOff )
   }

//	public void startAttrDrag( String name, float value )
//	{
//		final Slider slid = attrMap.get( name );
//		if( slid != null ) {
//			slid.setDragValue( value );
//			slid.setDragShown( true );
//		} else {
//			SwingOSC.printException( new NullPointerException(), name );
//		}
//	}
//
//	public void updateAttrDrag( String name, float value )
//	{
//		final Slider slid = attrMap.get( name );
//		if( slid != null ) {
//			slid.setDragValue( value );
//		} else {
//			SwingOSC.printException( new NullPointerException(), name );
//		}
//	}
//
//	public void stopAttrDrag( String name )
//	{
//		final Slider slid = attrMap.get( name );
//		if( slid != null ) {
//			slid.setDragShown( false );
//		} else {
//			SwingOSC.printException( new NullPointerException(), name );
//		}
//	}

//	public void createPeakMeter()
//	{
//		if( meter == null ) {
//			meter = new PeakMeter();
//			meter.setRMSPainted( false );
//			mainBox.add( meter, BorderLayout.WEST );
//			setSize( getPreferredSize() );
//		}
//	}
//
//	public void setID( Object id )
//	{
//		mdw.getPrepend()[ 1 ] = id;
//	}
//
//	public PeakMeter getPeakMeter()
//	{
//		return meter;
//	}
//
//	public Object[] createMetaData( Point2D pt )
//	{
//		return mdw.createMetaData( pt );
//	}

	def dispose {
//		if( meter != null ) {
//			meter.dispose()
//			meter = null;
//		}
	}

//	def setPreferredCenter( x: Int, y: Int ) {
//		prefCenter = new Point( x, y );
////		setLocation( x - getWidth() / 2, y - getHeight() / 2 );
//	}

//	public Point getPreferredCenter()
//	{
//		return prefCenter;
//	}

//	public void translate( int x, int y )
//	{
//		setLocation( getX() + x, getY() + y );
//	}

	override def paintComponent( g: Graphics ) {
		super.paintComponent( g )
		border.paintBorder( this, g, bdLeft, bdTop, getWidth() - bdLeft, getHeight() - (bdBottom + bdTop) )
	}

	private class Border( dashed: Boolean ) extends AbstractBorder {
		override def getBorderInsets( c: Component ) : Insets = new Insets( 2, 2, 2, 2 )

		override def getBorderInsets( c: Component, in: Insets ) : Insets = {
			in.left   = 2
			in.top    = 2
			in.right  = 2
			in.bottom = 2
			in
		}

		override def isBorderOpaque() : Boolean = true

		override def paintBorder( c: Component, g: Graphics, x: Int, y: Int, w: Int, h: Int ) {
			val g2			= g.asInstanceOf[ Graphics2D ]
			val strkOrig	= g2.getStroke()
			g2.setColor( c.getForeground() )
			if( dashed ) g2.setStroke( strkDashed )
			g2.drawRect( x, y, w - 1, h - 1 )
			g2.drawRect( x + 1, y + 1, w - 3, h - 3 )

			if( dashed ) g2.setStroke( strkOrig )
		}
	}
}