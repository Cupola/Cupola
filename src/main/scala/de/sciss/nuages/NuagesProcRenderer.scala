/*
 *  NuagesProcRenderer.scala
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

import prefuse.render.Renderer
import prefuse.visual.VisualItem
import java.awt.geom.Point2D
import java.awt._
import javax.swing.{JLabel, JSlider, JButton, JPanel}

/**
 *    @version 0.10, 28-Jun-10
 */
class NuagesProcRenderer extends Renderer {
   val panel = new Box( "Proc", 0, 1, 1 )
   panel.addAttr( "Freq", false, 0.33f, true )
   panel.addAttr( "Amp", false, 0.1f, true )

//   panel.add( new JLabel( "GAGAISM" ))
//   panel.add( new JSlider(), BorderLayout.SOUTH )
   panel.revalidate()
   val ps = panel.getPreferredSize()
   panel.setSize( ps.width, ps.height )
   panel.getLayout().layoutContainer( panel )
println( "PS = " + ps )
//   panel.setSize( 100, 100 )

   def setBounds( vi: VisualItem ) {
//    val shape = getShape( vi )
      val ix   = vi.getX
      val iy   = vi.getY
      val w    = panel.getWidth
      val h    = panel.getHeight
      val x    = ix - w/2
      val y    = iy - h/2
      vi.setBounds( x, y, w, h )
   }

   def locatePoint( pt: Point2D, vi: VisualItem ) : Boolean = {
      vi.getBounds().contains( pt )
   }

   def render( g: Graphics2D, vi: VisualItem ) {
      val atOrig = g.getTransform
      val b = vi.getBounds()
      g.translate( b.getX, b.getY )
      panel.paint( g ) // XXX clip bounds?
      g.setTransform( atOrig )
   }

//   private def getShape( vi: VisualItem ) : Shape = {
//       val at = getTransform( vi )
//       val rawShape = getRawShape( vi )
//       if( at == null || rawShape == null ) rawShape else at.createTransformedShape( rawShape )
//   }
}