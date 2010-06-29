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

import prefuse.visual.VisualItem
import java.awt._
import geom._
import javax.swing._
import prefuse.util.ColorLib
import prefuse.render.{AbstractShapeRenderer, ShapeRenderer, Renderer}

/**
 *    @version 0.10, 29-Jun-10
 */
object NuagesProcRenderer {
//   private val COS45 = math.cos( math.Pi * 0.25 )
//   private val COS45H = math.cos( math.Pi * 0.25 ) * 0.5
   private val colrPlaying = new Color( 0x00, 0xC0, 0x00 )
   private val colrStopped = new Color( 0x80, 0x80, 0x80 )
   private val colrMapped  = new Color( 210, 60, 60 )
   private val colrManual  = new Color( 60, 60, 240 )
}

class NuagesProcRenderer( size: Int )
extends AbstractShapeRenderer {
   import NuagesProcRenderer._
   import NuagesPanel._

   private val ellipse  = new Ellipse2D.Float()
   private val margin   = size * 0.2 
   private val margin2  = margin * 2 

   private val innerE   = new Ellipse2D.Double()
   private val gp       = new GeneralPath()
   private val playArc  = {
      val res = new Arc2D.Double( Arc2D.PIE )
      res.setAngleStart( 135 )
      res.setAngleExtent( 90 )
      res
   }

   private val pContArc  = {
      val res = new Arc2D.Double( Arc2D.PIE )
      res.setAngleStart( -45 )
      res.setAngleExtent( 270 )
      res
   }
   private val pValArc  = {
      val res = new Arc2D.Double( Arc2D.PIE )
      res
   }

   protected def getRawShape( vi: VisualItem ) : Shape = {
      var x    = vi.getX()
      if( x.isNaN || x.isInfinity ) x = 0.0
      var y    = vi.getY()
      if( y.isNaN || y.isInfinity ) y = 0.0
      val diam = size * vi.getSize()
      if ( diam > 1 ) {
          x -= diam / 2
          y -= diam / 2
      }
      ellipse.setFrame( x, y, diam, diam )
      ellipse
   }

   override def render( g: Graphics2D, vi: VisualItem ) {
      val outline = getShape( vi )
      if( outline == null ) return
      val data = vi.get( COL_NUAGES )
      if( data == null ) return
      val r     = outline.getBounds2D
      g.setColor( ColorLib.getColor( vi.getFillColor ))
      g.fill( outline )
      innerE.setFrame( r.getX() + margin, r.getY() + margin,
         r.getWidth() - margin2, r.getHeight() - margin2 )
      gp.reset()
      gp.append( outline, false )
      data match {
         case vProc: VisualProc     => renderProc( g, vi, r, vProc )
         case vFloat: VisualFloat   => renderFloat( g, vi, r, vFloat )
         case vBus: VisualBus       => renderBus( g, vi, r, vBus )
      }
   }

   private def renderProc( g: Graphics2D, vi: VisualItem, r: Rectangle2D, vProc: VisualProc ) {
      playArc.setFrame( r )
      val playArea = new Area( playArc )
      playArea.subtract( new Area( innerE ))
      gp.append( playArea, false )

      g.setColor( if( vProc.playing ) colrPlaying else colrStopped )
      g.fill( playArea )
      g.setColor( ColorLib.getColor( vi.getStrokeColor ))
      g.draw( gp )

      val font = Wolkenpumpe.condensedFont.deriveFont( size * vi.getSize().toFloat * 0.33333f )
      drawName( g, vi, r, font, vProc.proc.name )
   }

   private def renderFloat( g: Graphics2D, vi: VisualItem, r: Rectangle2D, vFloat: VisualFloat ) {
      pContArc.setFrame( r )
      val angExtent = (vFloat.value * 270).toInt
      val angStart  = 225 - angExtent
      pValArc.setArc( r, angStart, angExtent, Arc2D.PIE )
      val containerArea = new Area( pContArc )
      val innerArea = new Area( innerE )
      containerArea.subtract( innerArea )
      gp.append( containerArea, false )
      val valueArea = new Area( pValArc )
      valueArea.subtract( innerArea )

      g.setColor( if( vFloat.mapped ) colrMapped else colrManual )
      g.fill( valueArea )
      g.setColor( ColorLib.getColor( vi.getStrokeColor ))
      g.draw( gp )

      val font = Wolkenpumpe.condensedFont.deriveFont( size * vi.getSize().toFloat * 0.33333f )
      drawName( g, vi, r, font, vFloat.param.name )
   }

   private def renderBus( g: Graphics2D, vi: VisualItem, r: Rectangle2D, vBus: VisualBus ) {
      val font = Wolkenpumpe.condensedFont.deriveFont( size * vi.getSize().toFloat * 0.5f )
      drawName( g, vi, r, font, vBus.param.name )
   }

   private def drawName( g: Graphics2D, vi: VisualItem, r: Rectangle2D, font: Font, name: String ) {
      val diam    = r.getWidth()
      val diam2   = diam - (margin + margin)
      val cx      = r.getCenterX()
      val cy      = r.getCenterY()
      g.setFont( font )
      val fm      = g.getFontMetrics()
      g.setColor( ColorLib.getColor( vi.getTextColor() ))
      g.drawString( name, (cx - (fm.stringWidth( name ) * 0.5)).toFloat,
                          (cy + ((fm.getAscent() - fm.getLeading()) * 0.5)).toFloat )
   }
}