/*
 *  ParamImpl.scala
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

package de.sciss.synth.proc.impl

import de.sciss.synth._
import de.sciss.synth.proc._
import de.sciss.synth.ugen.{Out, In}

/**
 *    @version 0.11, 04-Jul-10
 */
class ParamControlImpl( val name: String, val spec: ParamSpec, val default: Double )
extends ProcParamControl {
   def kr : GE = {
      val p             = Proc.local
      val pb            = ProcGraphBuilder.local
      implicit val tx   = pb.tx
      val c             = p.control( name )
      pb.includeParam( this )
      c.mapping.map( m => {
         val outBus = m.output // .get
         require( outBus.rate == control )
         // stupidly we have two arguments...
         name.kr( default, List.fill( outBus.numChannels - 1 )( default ): _* )
      }).getOrElse( name.kr( default ))
   }
}

class ParamAudioImpl( val name: String, val spec: ParamSpec, val default: Double )
extends ProcParamAudio {
   def ar : GE = {
      val p             = Proc.local
      val pb            = ProcGraphBuilder.local
      implicit val tx   = pb.tx
      val c             = p.control( name )
      pb.includeParam( this )
      c.mapping.map( m => {
         val outBus = m.output // .get
         require( outBus.rate == audio )
         // stupidly we have two arguments...
         name.ar( default, List.fill( outBus.numChannels - 1 )( default ): _* )
      }).getOrElse( name.ar( default ))
   }
}

class ParamStringImpl( val name: String, val default: Option[ String ])
extends ProcParamString

class ParamAudioInputImpl( val name: String, val default: Option[ RichAudioBus ], physical: Boolean )
extends ProcParamAudioInput {

   def ar : GE = {
      val p    = Proc.local
      val pb   = ProcGraphBuilder.local
      implicit val tx   = pb.tx
      val ain = p.audioInput( name )
      val b = ain.bus.getOrElse({
         if( ain.synthetic ) {
            val res = RichBus.audio( p.server, 1 )
            ain.bus = Some( res )
            res
         } else if( physical ) {
            val res = RichBus.soundIn( p.server, 1 )
            ain.bus = Some( res )
            res
         } else pError( name ) // throw e
      })
      pb.includeParam( this ) // important: must be after ain.bus_= NO NOT TRUE - DOES NOT MATTER

      In.ar( name.kr, b.numChannels )
   }

   private def pError( name: String ) = throw new ProcParamUnspecifiedException( name )
}

class ParamAudioOutputImpl( val name: String, val default: Option[ RichAudioBus ], physical: Boolean )
extends ProcParamAudioOutput {

   def ar( sig: GE ) : GE = {
      val numChannels   = sig.numOutputs
      val p             = Proc.local
      val pb            = ProcGraphBuilder.local
      implicit val tx   = pb.tx
      val aout = p.audioOutput( name )
      val b = aout.bus.getOrElse({
         if( aout.synthetic ) {
            val res = RichBus.audio( p.server, numChannels )
            aout.bus = Some( res )
            res
         } else if( physical ) {
            val res = RichBus.soundOut( p.server, numChannels )
            aout.bus = Some( res )
            res
         } else pError( name ) // throw e
      })
      pb.includeParam( this ) // important: must be after aout.bus_= NO NOT TRUE DOES NOT MATTER
      val sig2: GE = if( b.numChannels == numChannels ) {
         sig
      } else {
         println( "WARNING: Coercing output signal from " + numChannels + " into " + b.numChannels + " channels" )
         val chans   = sig.outputs
         List.tabulate( b.numChannels )( ch => chans( ch % numChannels ))
      }
      Out.ar( name.kr, sig2 )
   }

   private def pError( name: String ) = throw new ProcParamUnspecifiedException( name )
}
