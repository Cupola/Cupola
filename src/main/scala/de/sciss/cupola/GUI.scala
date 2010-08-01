/*
 *  GUI.scala
 *  (Cupola)
 *
 *  Copyright (c) 2010 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 *
 *
 *  Changelog:
 */

package de.sciss.cupola

import Cupola._
import java.awt.{ BorderLayout, Color, Dimension, Graphics, GridLayout }
import java.awt.event.{ MouseAdapter, MouseEvent, WindowAdapter, WindowEvent }
import javax.swing.{ JComponent, JFrame, JLabel, JPanel, WindowConstants }
import actors.Actor
import collection.breakOut
import de.sciss.scalaosc.OSCMessage
import de.sciss.synth.proc.ProcTxn

/**
 *    @version 0.10, 01-Aug-10
 */
class GUI extends Cupola.Listener {
   gui =>

   private var valid = false
   private var selectedCell: Option[ Cell ] = None
   private val levelPane = new JPanel( new GridLayout( Level.all.size, Section.all.size ))
   private val map = Map[ (Level, Section), Cell ]( Level.all.flatMap( lvl => {
      Section.all.map( sec => {
         val gg = new Cell
         gg.addMouseListener( new MouseAdapter {
            override def mousePressed( e: MouseEvent ) {
               if( valid ) Cupola.simulate( OSCMessage( "/cupola", "state", lvl.id, sec.id ))
            }
         })
         levelPane.add( gg )
         (lvl, sec) -> gg
      })
   }): _* )

   // ---- constructor ----
   {
      val f    = new JFrame( "Cupola" )
      val cp   = f.getContentPane
      f.setDefaultCloseOperation( WindowConstants.DO_NOTHING_ON_CLOSE )

      ProcTxn.atomic { implicit tx => Cupola.addListener( gui )}

      f.addWindowListener( new WindowAdapter {
         override def windowClosing( e: WindowEvent ) {
//            println( "SENDING QUIT" )
            Cupola.quit
         }
      })
      cp.add( levelPane, BorderLayout.CENTER )
      f.setResizable( false )
      f.pack
      f.setLocation( 10, Cupola.SCREEN_BOUNDS.height - f.getHeight() - 10 )
      f.setVisible( true )
   }

   def updated( u: Cupola.Update ) {
      Cupola.guiRun {
         u.stage foreach { tup =>
            valid = true
            selectedCell.foreach( _.deselect )
            selectedCell = map.get( tup )
            selectedCell.foreach( _.select )
         }
      }
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