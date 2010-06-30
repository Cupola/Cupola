/*
 *  NuagesFrame.scala
 *  (Wolkenpumpe)
 *
 *  Copyright (c) 2008-2010 Hanns Holger Rutz. All rights reserved.
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

package de.sciss.nuages

import java.awt.{Color, Component, EventQueue, BorderLayout}
import javax.swing._
import de.sciss.synth.{Server, Model}
import event.{ListSelectionListener, ListSelectionEvent}
import java.awt.geom.Point2D
import de.sciss.synth.proc.{Proc, ProcFactory}

class NuagesFrame( server: Server ) extends JFrame {
   private val ggPanel  = new NuagesPanel( server )
   private val ggGens   = new JList( GensModel )

   // ---- constructor ----
   {
      // XXX should query current gen list
      // but then we need to figure out
      // a proper synchronization
      val font = Wolkenpumpe.condensedFont.deriveFont( 12 )
      Wolkenpumpe.addListener( GensModel.nuagesListener )

//      ggGens.setFont( font ) // doesn't do anything, porque?

      val cp = getContentPane
      cp.setBackground( Color.black )
      ggGens.setBackground( Color.black )
      GensRenderer.setFont( font )
      ggGens.setCellRenderer( GensRenderer )
      ggGens.setFixedCellWidth( 64 )
      ggGens.setSelectionMode( ListSelectionModel.SINGLE_SELECTION )
      ggGens.addListSelectionListener( new ListSelectionListener {
         def valueChanged( e: ListSelectionEvent ) {
            val idx = e.getFirstIndex()
            val pf = if( idx >= 0 && idx < GensModel.getSize() ) {
               Some( GensModel.getElementAt( idx ))
            } else {
               None
            }
            ggPanel.factory = pf
         }
      })

      val ggGensScroll = new JScrollPane( ggGens, ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS,
         ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER )
      cp.add( BorderLayout.EAST, ggGensScroll )
      cp.add( BorderLayout.CENTER, ggPanel )

      setDefaultCloseOperation( WindowConstants.DISPOSE_ON_CLOSE )
   }

   override def dispose {
      Wolkenpumpe.removeListener( GensModel.nuagesListener )
      ggPanel.dispose
      super.dispose
   }

   private def defer( thunk: => Unit ) {
      EventQueue.invokeLater( new Runnable { def run = thunk })
   }

   private object GensRenderer extends DefaultListCellRenderer {
      private val colrUnfocused = new Color( 0xC0, 0xC0, 0xC0 )

      override def getListCellRendererComponent( list: JList, value: AnyRef, index: Int,
         isSelected: Boolean, isFocused: Boolean ) : Component = {

         val obj = value match {
            case pf: ProcFactory => pf.name
            case x => x
         }
//         super.getListCellRendererComponent( list, obj, index, isSelected, isFocused )
         setText( obj.toString )
         setBackground( if( isSelected ) { if( isFocused ) Color.white else colrUnfocused } else Color.black )
         setForeground( if( isSelected ) Color.black else Color.white )
         this
      }
   }

   private object GensModel extends AbstractListModel with Ordering[ ProcFactory ] {
//      model =>

      private var coll = Vector.empty[ ProcFactory ]

      val nuagesListener : Model.Listener = {
         case Wolkenpumpe.GensRemoved( pfs @ _* ) => defer {
            val indices = pfs.map( Util.binarySearch( coll, _ )( GensModel )).filter( _ >= 0 )
            coll = coll.diff( pfs )
            val index0 = indices.min
            val index1 = indices.max
//            fireIntervalRemoved( model, index0, index1 )
            removed( index0, index1 ) // WARNING: IllegalAccessError with fireIntervalRemoved
         }
         case Wolkenpumpe.GensAdded( pfs @ _* ) => defer {
            val indices = pfs.map( pf => {
               val idx  = Util.binarySearch( coll, pf )( GensModel )
               val idx0 = if( idx < 0) (-idx - 1) else idx
               coll     = coll.patch( idx0, Vector( pf ), 0 )
               idx0
            })
            val index0 = indices.min
            val index1 = indices.max
//println( "fireIntervalAdded( " + model + ", " + index0 + ", " + index1 +" )" )
//            fireIntervalAdded( model, index0, index1 )
            added( index0, index1 ) // WARNING: IllegalAccessError with fireIntervalAdded
         }
      }

      private def removed( index0: Int, index1: Int ) {
         fireIntervalRemoved( GensModel, index0, index1 )
      }

      private def added( index0: Int, index1: Int ) {
         fireIntervalAdded( GensModel, index0, index1 )
      }

      // Ordering
      def compare( a: ProcFactory, b: ProcFactory ) = a.name.toUpperCase.compare( b.name.toUpperCase )

      // AbstractListModel
      def getSize : Int = coll.size
      def getElementAt( idx: Int ) : ProcFactory = coll( idx )
   }
}