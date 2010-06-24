/*
 *  ProcParam.scala
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
import ugen.{Out, In}

/**
 *    @version 0.12, 24-Jun-10
 */
sealed trait ProcParam[ T ] {
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

//   def ir : GE
//   def kr : GE
   
   def ir : GE = {
      ProcGraphBuilder.local.includeParam( this )
      name.ir( default.getOrElse( 0f ))
   }

   def kr : GE = {
      ProcGraphBuilder.local.includeParam( this )
      name.kr( default.getOrElse( 0f ))
   }

   def spec : ParamSpec
}

trait ProcParamString extends ProcParam[ String ] {
}

sealed trait ProcParamAudioBus extends ProcParam[ RichBus ] {
   def index : GE = {
      ProcGraphBuilder.local.includeParam( this )
      name.kr( 0 ) // default.map( _._1 ).getOrElse( 0 )) // XXX
   }

   def numChannels : Int
}

trait ProcParamAudioInput extends ProcParamAudioBus {
   def ar : GE = {
// note: index already includes the parameter
//      ProcGraphBuilder.local.includeParam( this )
      In.ar( index, numChannels )
   }
}

trait ProcParamAudioOutput extends ProcParamAudioBus {
   def ar( sig: GE ) : GE = {
// note: index already includes the parameter
//      ProcGraphBuilder.local.includeParam( this )
      Out.ar( index, sig )
   }
}
