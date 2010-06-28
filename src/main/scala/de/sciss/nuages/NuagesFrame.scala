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

import javax.swing.{AbstractListModel, JList, JFrame}
import de.sciss.synth.proc.ProcFactory
import de.sciss.synth.Model
import java.awt.{EventQueue, BorderLayout}

class NuagesFrame extends JFrame {
   private val gensModel = new GensModel

   // ---- constructor ----
   {
      // XXX should query current gen list
      // but then we need to figure out
      // a proper synchronization
      Wolkenpumpe.addListener( gensModel.nuagesListener )

      val cp = getContentPane
      val ggGens = new JList( gensModel )
      cp.add( BorderLayout.EAST, ggGens )
   }

   override def dispose {
      Wolkenpumpe.removeListener( gensModel.nuagesListener )
      super.dispose
   }

   private def defer( thunk: => Unit ) {
      EventQueue.invokeLater( new Runnable { def run = thunk })
   }

   private class GensModel extends AbstractListModel with Ordering[ ProcFactory ] {
      model =>

      private var coll = Vector.empty[ ProcFactory ]

      val nuagesListener : Model.Listener = {
         case Wolkenpumpe.GensRemoved( pfs @ _* ) => defer {
            val indices = pfs.map( Util.binarySearch( coll, _ )( model )).filter( _ >= 0 )
            coll = coll.diff( pfs )
            val index0 = indices.min
            val index1 = indices.max
//            fireIntervalRemoved( model, index0, index1 )
            removed( index0, index1 ) // WARNING: IllegalAccessError with fireIntervalRemoved
         }
         case Wolkenpumpe.GensAdded( pfs @ _* ) => defer {
            val indices = pfs.map( pf => {
               val idx  = Util.binarySearch( coll, pf )( model )
               val idx0 = if( idx < 0) (-idx - 1) else idx
               coll     = coll.patch( idx0, Vector( pf ), 0 )
               idx0
            })
            val index0 = indices.min
            val index1 = indices.max
println( "fireIntervalAdded( " + model + ", " + index0 + ", " + index1 +" )" )
//            fireIntervalAdded( model, index0, index1 )
            added( index0, index1 ) // WARNING: IllegalAccessError with fireIntervalAdded
         }
      }

      private def removed( index0: Int, index1: Int ) {
         fireIntervalRemoved( model, index0, index1 )
      }

      private def added( index0: Int, index1: Int ) {
         fireIntervalAdded( model, index0, index1 )
      }

      // Ordering
      def compare( a: ProcFactory, b: ProcFactory ) = a.name.toUpperCase.compare( b.name.toUpperCase )

      // AbstractListModel
      def getSize : Int = coll.size
      def getElementAt( idx: Int ) : AnyRef = coll( idx )
   }
}