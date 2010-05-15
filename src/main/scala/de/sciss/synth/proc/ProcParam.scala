package de.sciss.synth.proc

import de.sciss.synth.GE

trait ProcParam[ T ] {
//   type t = T
   def name : String
   def default : Option[ T ]

//   // ---- scope : Proc ----
//
//   def get : T = Proc.local.getParam[ T ]( name )
}

trait ProcParamFloat extends ProcParam[ Float ] {
//   def kr      : GE = name.kr( default.getOrElse( spec.lo ))
//   def mapKr   : GE = spec.map( name.kr( spec.unmap( default.getOrElse( spec.lo ))))

   def ir : GE = {
      ProcGraphBuilder.local.addParam( this )
      name.ir( default.getOrElse( 0f ))
   }

   def kr : GE = {
      ProcGraphBuilder.local.addParam( this )
      name.kr( default.getOrElse( 0f ))
   }

   def spec    : ParamSpec
}

trait ProcParamString extends ProcParam[ String ] {
}
