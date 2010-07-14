/*
 *  DSL.scala
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
import de.sciss.scalaosc.OSCBundle
import collection.immutable.{ IndexedSeq => IIdxSeq }
import reflect.ClassManifest

/**
 *    @version 0.12, 14-Jul-10
 */
object DSL {
   private val cmGE     = ClassManifest.fromClass( classOf[ GE ])
   private val cmUnit   = ClassManifest.Unit

   // ---- scope : outside ----

   /**
    *    Generates a sound generating process factory
    *    with the given name and described through
    *    the given code block.
    */
   def gen( name: String )( thunk: => Unit )( implicit tx: ProcTxn ) : ProcFactory = {
      val res = ProcFactoryBuilder.gen( name )( thunk )
      ProcDemiurg.addFactory( res )
      res
   }

   /**
    *    Generates a sound filtering (transformation) process factory
    *    with the given name and described through
    *    the given code block.
    */
   def filter( name: String )( thunk: => Unit )( implicit tx: ProcTxn ) : ProcFactory = {
      val res = ProcFactoryBuilder.filter( name )( thunk )
      ProcDemiurg.addFactory( res )
      res
   }

   /**
    *    Generates a sound diffusing process factory
    *    with the given name and described through
    *    the given code block.
    */
   def diff( name: String )( thunk: => Unit )( implicit tx: ProcTxn ) : ProcFactory = {
      val res = ProcFactoryBuilder.diff( name )( thunk )
      ProcDemiurg.addFactory( res )
      res
   }

   /**
    *    Performs a code block where all
    *    transitions are considered instantly.
    */
   def instant[ T ]( thunk: => T )( implicit tx: ProcTxn ) = {
      tx.withTransition( Instant )( thunk )
   }

   /**
    *    Performs a code block with transitional
    *    semantics taken from a crossfade of
    *    the given duration (in seconds).
    */
   def xfade[ T ]( dur: Double )( thunk: => T )( implicit tx: ProcTxn ) = {
      val trns = XFade( tx.time, dur )
      tx.withTransition( trns )( thunk )
   }

   /**
    *    Performs a code block with transitional
    *    semantics taken from gliding for
    *    the given duration (in seconds).
    */
   def glide[ T ]( dur: Double )( thunk: => T )( implicit tx: ProcTxn ) = {
      val trns = Glide( tx.time, dur ) 
      tx.withTransition( trns )( thunk )
   }

   // ---- scope : gen (ProcFactoryBuilder) ----

   def pControl( name: String, spec: ParamSpec = ParamSpec(), default: Double ) : ProcParamControl =
      ProcFactoryBuilder.local.pControl( name, spec, default )
   def pAudio( name: String, spec: ParamSpec = ParamSpec(), default: Double ) : ProcParamAudio =
      ProcFactoryBuilder.local.pAudio( name, spec, default )
   def pString( name: String, default: Option[ String ] = None ) : ProcParamString =
      ProcFactoryBuilder.local.pString( name, default )
   def pAudioIn( name: String, default: Option[ RichAudioBus ] = None ) : ProcParamAudioInput =
      ProcFactoryBuilder.local.pAudioIn( name, default )
   def pAudioOut( name: String, default: Option[ RichAudioBus ] = None ) : ProcParamAudioOutput =
      ProcFactoryBuilder.local.pAudioOut( name, default )

   def graph( thunk: => GE ) : ProcGraph = {
      val b = ProcFactoryBuilder.local
      b.anatomy match {
         case ProcGen    => b.graphOut( () => thunk )
         case ProcFilter => b.graphInOut( in => thunk )
         case ProcDiff   => b.graph( () => thunk )
      }
   }

   def graph( fun: GE => GE ) : ProcGraph = {
      val b = ProcFactoryBuilder.local
      b.anatomy match {
         case ProcGen    => error( "Generators do not have a default input" )
         case ProcFilter => b.graphInOut( fun )
         case ProcDiff   => b.graphIn( fun )
      }
   }

//   def synth[ T ]( thunk: => T )( implicit m: ClassManifest[ T ]) : ProcGraph = {
//      val pf = ProcFactoryBuilder.local
//      if( m <:< cmGE ) {
//         val funC: Function0[ GE ] = () => thunk.asInstanceOf[ GE ]
//         pf.synthOutput( funC )
//      } else if( m <:< cmUnit ) {
//         val funC: Function0[ Unit ] = () => thunk
//         pf.synth( funC )
//      } else error( "Unsupported graph return type" )
//   }
//
//   def filter[ T ]( fun: GE => T )( implicit m: ClassManifest[ T ]) : ProcGraph = {
//      val pf = ProcFactoryBuilder.local
//      if( m <:< cmGE ) {
//         val funC: Function1[ GE, GE ] = fun.asInstanceOf[ GE => GE ]
//         pf.filterOutput( funC )
//      } else if( m <:< cmUnit ) {
//         val funC: Function1[ GE, Unit ] = fun.asInstanceOf[ GE => Unit ]
//         pf.filter( funC )
//      } else error( "Unsupported graph return type" )
//   }

   def bufCue( name: String, path: String ) : ProcBuffer =
      ProcFactoryBuilder.local.bufCue( name, path )
   def bufCue( name: String, p: ProcParamString ) : ProcBuffer =
      ProcFactoryBuilder.local.bufCue( name, p )

   // ---- scope : graph (ProcGraphBuilder) ----
   
//   implicit def paramNumToGE( p: ProcParamNum ) : GE = p.embed
//   implicit def bufferToGE( b: ProcBuffer ) : GE = b.embed

   implicit def procToAudioInput( p: Proc ) : ProcAudioInput   = p.audioInput( "in" )
   implicit def procToAudioOutput( p: Proc ) : ProcAudioOutput = p.audioOutput( "out" )
   implicit def procToAudioInOut( p: Proc ) : (ProcAudioInput, ProcAudioOutput) =
      p.audioInput( "in" ) -> p.audioOutput( "out" )
}

trait ProcBuffer {
   def name : String
//   def kr : GE // = name.kr

   // ---- scope : graph (ProcGraphBuilder) ----
   
   def id : GE
   def numChannels : Int
}

sealed abstract class ProcAnatomy
case object ProcGen    extends ProcAnatomy
case object ProcFilter extends ProcAnatomy
case object ProcDiff   extends ProcAnatomy

trait ProcSpec {
   def name : String
   def anatomy : ProcAnatomy
   def params : IIdxSeq[ ProcParam ]   // XXX change naming
   def param( name: String ) : ProcParam
}

trait ProcFactory extends ProcSpec {
   def make( implicit tx: ProcTxn ) : Proc
}

//trait ProcGenFactory extends ProcFactory {
//   def make( implicit tx: ProcTxn ) : ProcGen
//}
//
//trait ProcFilterFactory extends ProcFactory {
//   def make( implicit tx: ProcTxn ) : ProcFilter
//}
//
//trait ProcDiffFactory extends ProcFactory {
//   def make( implicit tx: ProcTxn ) : ProcDiff
//}
