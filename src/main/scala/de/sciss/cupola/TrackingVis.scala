package de.sciss.cupola

import java.awt._
import javax.swing._
import java.awt.event.{ActionEvent, ActionListener}
import java.io.File
import java.util.Date
import java.text.SimpleDateFormat
import de.sciss.osc.{OSCBundle, OSCMessage}

class TrackingVis extends JComponent {
   vis =>

   private var last = OSCTrackingMessage.empty
   private val f = new JFrame( "Tracking" )
   private val box = Box.createHorizontalBox()
   private val ggRec = button( "REC", actionRecord )
   private val ggStop = button( "STOP", actionStop )
   private val ggPlay = button( "PLAY", actionPlay )
   private val ggOpen = button( "OPEN", actionOpen )

   private var fileOption: Option[ File ] = None
   private var recorderOption : Option[ OSCRecorder ] = None
   private var playerOption: Option[ OSCPlayer ] = None
   private val sync  = new AnyRef
   private val df    = new SimpleDateFormat( "yyyyMMdd'_'HHmmss'.osc'" )

   // ---- constructor ----
   {
      val cp = f.getContentPane()
      vis.setPreferredSize( new Dimension( 400, 400 ))
      cp.add( vis, BorderLayout.CENTER )
      cp.add( box, BorderLayout.SOUTH )
      ggStop.setEnabled( false )
      ggPlay.setEnabled( false )
      f.pack()
      f.setLocation( Cupola.SCREEN_BOUNDS.height - getHeight - 10, Cupola.SCREEN_BOUNDS.width - getWidth - 10 )
      f.setVisible( true )
   }

   private def button( label: String, action: => Unit ) : AbstractButton = {
      val b = new JButton( label )
      b.addActionListener( new ActionListener {
         def actionPerformed( e: ActionEvent ) { action }
      })
      b.setFocusable( false )
      box.add( b )
      b
   }

   def actionPlay {
      sync.synchronized {
         actionStop
         fileOption foreach { file =>
            val player = new OSCPlayer( file, OSCTrackingCodec )
            player.action = _ match {
               case msg: OSCMessage => {
//println( "DOING : " + msg )
                  Cupola.simulate( msg )
               }
               case _ =>
            }
            playerOption = Some( player )
            Cupola.defeatTracking( true )
            player.start
            ggStop.setEnabled( true )
            ggPlay.setEnabled( false )
         }
      }
   }

   def actionStop {
      sync.synchronized {
         recorderOption.foreach( _.close )
         recorderOption = None
         playerOption foreach { player =>
            Cupola.defeatTracking( false )
            player.close
            playerOption = None
         }
         ggPlay.setEnabled( fileOption.isDefined )
         ggRec.setEnabled(  true )
         ggOpen.setEnabled( true )
         ggStop.setEnabled( false )
      }
   }

   def actionOpen {
      sync.synchronized {
         actionStop
         val dlg = new FileDialog( f, "Open OSC File for Playback" )
         dlg.setDirectory( Cupola.BASE_PATH + "osc" )
         dlg.setVisible( true )
         val path = dlg.getFile()
         val dir  = dlg.getDirectory()
         fileOption = if( path == null || dir == null ) {
            None
         } else {
            Some( new File( dir, path ))
         }
         ggPlay.setEnabled( fileOption.isDefined )
      }
   }

   def actionRecord {
      sync.synchronized {
         actionStop
         val file = new File( Cupola.BASE_PATH + "osc/" + df.format( new Date() ))
         fileOption = Some( file )
         recorderOption = Some( new OSCRecorder( file, OSCTrackingCodec ))
         ggPlay.setEnabled( false )
         ggRec.setEnabled(  false )
         ggOpen.setEnabled( false )
         ggStop.setEnabled( true )
      }
   }

   override def paintComponent( g: Graphics ) {
      val msg = last
      g.setColor( Color.black )
      val w = getWidth(); val h = getHeight()
      val g2 = g.asInstanceOf[ Graphics2D ]
      val atOrig = g2.getTransform()
      g.fillRect( 0, 0, w, h )
      g2.translate( 40, 25 )
      g.setColor( Color.green )
      g.drawOval( msg.eyeLX - 30, msg.eyeLY - 15, 60, 30 )
//      g.fillOval( msg.pupLX - 10, msg.pupLY - 10, 20, 20 )
      g.fillOval( msg.eyeLX + msg.pupLX - 10, msg.eyeLY + msg.pupLY - 10, 20, 20 )
      g.setColor( Color.red )
      g.drawOval( msg.eyeRX - 30, msg.eyeRY - 15, 60, 30 )
//      g.fillOval( msg.pupRX - 10, msg.pupRY - 10, 20, 20 )
      g.fillOval( msg.eyeRX + msg.pupRX - 10, msg.eyeRY + msg.pupRY - 10, 20, 20 )
      g.setColor( Color.blue )
      val ptx = msg.pointX.toInt
      val pty = msg.pointY.toInt
      g.drawLine( ptx, pty - 20, ptx - 20, pty + 20 )
      g.drawLine( ptx - 20, pty + 20, ptx + 20, pty + 20 )
      g.drawLine( ptx + 20, pty + 20, ptx, pty - 20 )
      g2.setTransform( atOrig )
   }

   def update( newMsg: OSCTrackingMessage ) {
      sync.synchronized {
         last = newMsg
         recorderOption.foreach( _.add( OSCBundle.millis( System.currentTimeMillis, newMsg )))
      }
      repaint( 50 )
   }
}