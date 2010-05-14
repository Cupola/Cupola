package de.sciss.cupola

import de.sciss.synth.{ GE, SC }
import SC._

case class ParamSpec( lo: Float = 0f, hi: Float = 1f, warp: Warp = LinearWarp, step: Float = 0f ) {
   def range = hi - lo
   def map( value: Float ) : Float = warp.map( this, value )
   def unmap( value: Float ) : Float = warp.unmap( this, value )
   def map( value: GE ) : GE = warp.map( this, value )
   def unmap( value: GE ) : GE = warp.unmap( this, value )
}

trait Warp {
   /**
    *    From normalized range to spec
    */
   def map( spec: ParamSpec, value: Float ) : Float

   /**
    *    From spec to normalized range
    */
   def unmap( spec: ParamSpec, value: Float ) : Float
   def map( spec: ParamSpec, value: GE ) : GE
   def unmap( spec: ParamSpec, value: GE ) : GE
}

object LinearWarp extends Warp {
   def map( spec: ParamSpec, value: Float ) : Float   = value * spec.range + spec.lo
   def unmap( spec: ParamSpec, value: Float ) : Float = (value - spec.lo) / spec.range
   def map( spec: ParamSpec, value: GE ) : GE         = value * spec.range + spec.lo
   def unmap( spec: ParamSpec, value: GE ) : GE       = (value - spec.lo) / spec.range
}