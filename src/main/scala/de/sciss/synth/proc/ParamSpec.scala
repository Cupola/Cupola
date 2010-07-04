/*
 *  ParamSpec.scala
 *  (ScalaCollider-Proc)
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

package de.sciss.synth.proc

import de.sciss.synth._

/**
 *    @version 0.11, 04-Jul-10
 */
case class ParamSpec( lo: Double = 0.0, hi: Double = 1.0, warp: Warp = LinWarp, step: Double = 0.0 ) {
   def range = hi - lo
   def ratio = hi / lo
   def clip( value: Double ) : Double  = math.max( lo, math.min( hi, value ))
   def map( value: Double ) : Double   = warp.map( this, value )
   def unmap( value: Double ) : Double = warp.unmap( this, value )
   def map( value: GE ) : GE           = warp.map( this, value )
   def unmap( value: GE ) : GE         = warp.unmap( this, value )
}

trait Warp {
   /**
    *    From normalized range to spec
    */
   def map( spec: ParamSpec, value: Double ) : Double

   /**
    *    From spec to normalized range
    */
   def unmap( spec: ParamSpec, value: Double ) : Double
   def map( spec: ParamSpec, value: GE ) : GE
   def unmap( spec: ParamSpec, value: GE ) : GE
}

object LinWarp extends Warp {
   def map( spec: ParamSpec, value: Double ) : Double    = value * spec.range + spec.lo
   def unmap( spec: ParamSpec, value: Double ) : Double  = (value - spec.lo) / spec.range
   def map( spec: ParamSpec, value: GE ) : GE            = value * spec.range + spec.lo
   def unmap( spec: ParamSpec, value: GE ) : GE          = (value - spec.lo) / spec.range
}

object ExpWarp extends Warp {
   def map( spec: ParamSpec, value: Double ) : Double    = spec.ratio.pow( value ) * spec.lo
   def unmap( spec: ParamSpec, value: Double ) : Double  = (value / spec.lo).log / spec.ratio.log
   def map( spec: ParamSpec, value: GE ) : GE            = (spec.hi / spec.lo).pow( value ) * spec.lo
   def unmap( spec: ParamSpec, value: GE ) : GE          = (value / spec.lo).log / spec.ratio.log
}