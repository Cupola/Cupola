package de.sciss.cupola

import Cupola._
import java.awt.{ BorderLayout, Color, Dimension, Graphics, GridLayout }
import java.awt.event.{ MouseAdapter, MouseEvent, WindowAdapter, WindowEvent }
import javax.swing.{ JComponent, JFrame, JLabel, JPanel, WindowConstants }
import actors.Actor
import collection.breakOut
import de.sciss.scalaosc.OSCMessage

class GUI {
   // ---- constructor ----
   {
      val f    = new JFrame( "Cupola" )
      val cp   = f.getContentPane
      f.setDefaultCloseOperation( WindowConstants.DO_NOTHING_ON_CLOSE )
      val levelPane = new JPanel( new GridLayout( Level.all.size, Section.all.size ))
      var selectedCell: Option[ Cell ] = None
      val map = Map( Level.all.flatMap( lvl => {
         Section.all.map( sec => {
            val gg = new Cell
            gg.addMouseListener( new MouseAdapter {
               override def mousePressed( e: MouseEvent ) {
//                  simulate( OSCMessage( "/cupola", "state", lvl.id, sec.id ))
               }
            })
            levelPane.add( gg )
            LevelChanged( lvl, sec ) -> gg
         })
      }): _* )

      // WARNING: actor { } is BROKEN!!! only use new Actor to create an actor!!!
      val a = new Actor {
         def act {
            Cupola ! AddListener
            Cupola ! QueryLevel
            loop {
               react {
               case msg: LevelChanged => guiRun {
                  selectedCell.foreach( _.deselect )
                  selectedCell = map.get( msg )
                  selectedCell.foreach( _.select )
               }
//               case Quit => {
//                  Cupola ! Quit
//               }
               case x => println( "GUI: Unknown actor message  : " + x)
            }}
         }
      }
      a.start
      
      f.addWindowListener( new WindowAdapter {
         override def windowClosing( e: WindowEvent ) {
//            println( "SENDING QUIT" )
            Cupola ! Quit
         }
      })
      cp.add( levelPane, BorderLayout.CENTER )
      f.pack
      f.setLocationRelativeTo( null )
      f.setVisible( true )
   }

   private class Cell extends JComponent {
      setPreferredSize( new Dimension( 64, 32 ))
      setOpaque( false )
      deselect

      def select {
         setBackground( Color.white )
      }

      def deselect {
         setBackground( Color.black )
      }

      override def paintComponent( g: Graphics ) {
         super.paintComponent( g )
         g.setColor( getBackground )
         g.fillRect( 1, 1, getWidth - 2, getHeight - 2 )
      }
   }
}