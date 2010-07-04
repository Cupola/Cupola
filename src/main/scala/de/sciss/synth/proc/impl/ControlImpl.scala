/*
 *  ControlImpl.scala
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

import de.sciss.synth.proc._
import de.sciss.synth._
import ugen._

class ControlImpl( val proc: Impl, param: ProcParamFloat, val _rate: Rate )
extends ProcControl {
   ctrl =>

//   private var valueRef = Ref( default )
   private var valueRef = Ref( ControlValue.instant( default ))

//      private var valueRef = Ref.withObserver( default ) { (oldV, newV) =>
//         if( oldV != newV ) proc.dispatchControlChange( ctrl, newV )
//      }
   private val mappingRef = Ref[ Option[ ProcControlMapping ]]( None )
//      private val mappingRef = Ref.withObserver[ Option[ ProcControlMapping ]]( None ) { (oldM, newM) =>
//         if( oldM != newM ) proc.dispatchMappingChange( ctrl, newM )
//      }

   // XXX this should eventually fuse with mappingRef !!!
   private val glidingRef = Ref[ Option[ ControlGliding ]]( None )

   def rate    = Some( _rate )
   def default = param.default // .getOrElse( 0f )
   def spec    = param.spec
   def name    = param.name

/*
old trns new trns resolution
------------------------------------------
instant  instant  instant  if( oldValue != newValue)
instant  glide    glide    if( oldValue != newValue)
instant  xfade    xfade    if( oldValue != newValue)

glide    instant  stop glide + instant
glide    glide    stop glide + glide
glide    xfade    xfade

xfade    instant  instant  if( oldValue != newValue)
xfade    glide    glide    if( oldValue != newValue)
xfade    xfade    xfade
 */

   def cv( implicit tx: ProcTxn ) = valueRef()

   def value( implicit tx: ProcTxn ) : Double = valueRef().current

   def value_=( newValue: Double )( implicit tx: ProcTxn ) {
      if( isMapped ) { // how to handle this the best? error?
         valueRef.set( ControlValue.instant( newValue )) // we just ignore the transition and remember the value
         return
      }

      val oldCV   = valueRef()
      val oldCG   = glidingRef()
      tx.transit match {
         case Instant   => {
            if( oldCG.isDefined || oldCV.target != newValue ) {
               glidingRef.set( None )
               oldCG.foreach( _.stop )
               val newCV = if( oldCG.isDefined ) {
                  ControlValue.instant( newValue )
               } else {
                  oldCV.copy( target = newValue )
               }
               valueRef.set( newCV )
               proc.controlChanged( ctrl, newCV )
            }
         }
         case glide: Glide => {
            if( oldCG.isDefined || oldCV.target != newValue ) {
               val newCV = ControlValue( oldCV.current, newValue, glide )
               val newCG = _rate match {
                  case `control` => new ControlKGliding( ctrl, newCV )
                  case `audio`   => new ControlAGliding( ctrl, newCV )
                  case _ => error( "Cannot glide rate " + _rate )
               }
               glidingRef.set( Some( newCG ))
               oldCG.foreach( _.stop )
               valueRef.set( newCV )
               proc.controlChanged( ctrl, newCV )
            }
         }
         case fade: XFade => error( "NOT YET IMPLEMENTED" )
      }
   }

   def mapping( implicit tx: ProcTxn ) : Option[ ProcControlMapping ] = mappingRef()

   def map( aout: ProcAudioOutput )( implicit tx: ProcTxn ) : ProcControlAMapping = {
//         require( aout.proc.server == proc.server ) // that should be in ~> hopefully
      require( mapping.isEmpty, "Already mapped" )
      val m = _rate match {
         case `control` => new ControlKBusMapping( aout, ctrl )
         case `audio` => new ControlABusMapping( aout, ctrl )
         case _ => error( "Cannot map rate " + _rate )
      }
      m.init
      val mo = Some( m )
      mappingRef.set( mo )
      proc.controlMapped( ctrl, mo )
//         m.connect
//         this
      m
   }

   def isMapable = true
   def canMap( aout: ProcAudioOutput )( implicit tx: ProcTxn ) : Boolean = !isMapped
}

trait ControlMapping {
   protected val synth   = Ref[ Option[ RichSynth ]]( None )

   // ---- abstract ----
   def target: ControlImpl
   def play( implicit tx: ProcTxn ) : Unit
   protected def addOutputConsumers( implicit tx: ProcTxn ) : Unit
   protected def removeOutputConsumers( implicit tx: ProcTxn ) : Unit
   protected def targetNode( implicit tx: ProcTxn ): RichNode

   def proc    = target.proc
   def name    = target.name + "#map"

   def stop( implicit tx: ProcTxn ) {
      // we set synth to None first, so
      // the removeReader calls don't produce
      // unnecessary n_set messages
      synth.swap( None ).foreach( _.free( true ))
      removeOutputConsumers
   }
}

trait ControlGliding extends ControlMapping {
   def cv: ControlValue

   def play( implicit tx: ProcTxn ) {
      val g       = graph
      val rsd     = RichSynthDef( target.proc.server, g )
      val dur     = cv.transit.asInstanceOf[ Glide ].dur // XXX not so pretty
      val rs      = rsd.play( target.proc.playGroup,
         List( "$start" -> cv.source, "$stop" -> cv.target, "$dur" -> dur ), addBefore )

      val oldSynth = synth.swap( Some( rs ))
      addOutputConsumers   // requires that synth has been assigned!
      oldSynth.foreach( _.free( true ))
   }

   protected def graph : SynthGraph
}

class ControlKGliding( val target: ControlImpl, val cv: ControlValue )
extends ControlGliding with ControlToKMapping {
   def output( implicit tx: ProcTxn ) = {
      outputRef().getOrElse({
         val res = RichBus.control( proc.server, 1 ) // XXX numChannels
         outputRef.set( Some( res ))
         res
      })
   }

   protected def targetNode( implicit tx: ProcTxn ) = target.proc.group // XXX anyway wrong!

   protected def graph = SynthGraph {
      val line    = Line.kr( "$start".ir, "$stop".ir, "$dur".ir, freeSelf )
      val out     = target.spec.map( line )
      // XXX multichannel expansion
      Out.kr( "$out".kr, out )
   }
}

class ControlAGliding( val target: ControlImpl, val cv: ControlValue )
extends ControlGliding with ControlToAMapping {
   def output( implicit tx: ProcTxn ) = {
      outputRef().getOrElse({
         val res = RichBus.audio( proc.server, 1 ) // XXX numChannels
         outputRef.set( Some( res ))
         res
      })
   }

   protected def targetNode( implicit tx: ProcTxn ) = target.proc.group // XXX anyway wrong!

   protected def graph = SynthGraph {
      val line    = Line.ar( "$start".ir, "$stop".ir, "$dur".ir, freeSelf )
      val out     = target.spec.map( line )
      // XXX multichannel expansion
      Out.ar( "$out".kr, out )
   }
}

abstract class ControlBusMapping extends AbstractAudioInputImpl with ControlMapping with ProcControlAMapping {
   def source: ProcAudioOutput
//      def connect( implicit tx: ProcTxn ) { source ~> this } // XXX NO WE DON'T NEED TO ENFORCE TOPOLOGY !!!

   lazy val edge = ProcEdge( source, this )

//      protected val edges = Ref( Set.empty[ ProcEdge ])

//      def input : ProcAudioInput = this

   def init( implicit tx: ProcTxn ) {
      addEdge( edge )
   }

   /**
    *    That means the mapping source bus changed.
    *    If numChannels changes we need to rebuild.
    *    Otherwise the mapping synth's "in" param
    *    needs update.
    */
   def busChanged( bus: AudioBus )( implicit tx: ProcTxn ) {
      // XXX check numChannels
      synth().foreach( _.set( true, "in" -> bus.index ))
   }

   def play( implicit tx: ProcTxn ) {
      val inBus   = bus.get.busOption.get
      val g       = graph( inBus )
      val rsd     = RichSynthDef( inBus.server, g )
      val rs      = rsd.play( source.proc.playGroup, List( "$in" -> inBus.index ), addAfter )

      val oldSynth = synth.swap( Some( rs ))
      addOutputConsumers   // requires that synth has been assigned!
      oldSynth.foreach( _.free( true ))
   }

   protected def graph( inBus: AudioBus ) : SynthGraph
}

trait ControlToKMapping extends ControlMapping {
   protected var outputRef = Ref[ Option[ RichControlBus ]]( None )

   // ---- abstract ----
   def output( implicit tx: ProcTxn ) : RichControlBus

   private val outputReader = new RichControlBus.User {
      def busChanged( bus: ControlBus )( implicit tx: ProcTxn ) {
         targetNode.mapn( true, target.name -> bus )
      }
   }

   private val outputWriter = new RichControlBus.User {
      def busChanged( bus: ControlBus )( implicit tx: ProcTxn ) {
         synth().foreach( rs => {
            rs.set( true, "$out" -> bus.index )
         })
      }
   }

   protected def addOutputConsumers( implicit tx: ProcTxn ) {
      output.addReader( outputReader )
      output.addWriter( outputWriter )
   }

   protected def removeOutputConsumers( implicit tx: ProcTxn ) {
      output.removeReader( outputReader )
      output.removeWriter( outputWriter )
   }
}

trait ControlToAMapping extends ControlMapping {
   protected var outputRef = Ref[ Option[ RichAudioBus ]]( None )

   // ---- abstract ----
   def output( implicit tx: ProcTxn ) : RichAudioBus

   private val outputWriter = new RichAudioBus.User {
      def busChanged( bus: AudioBus )( implicit tx: ProcTxn ) {
         synth().foreach( rs => {
            rs.set( true, "$out" -> bus.index )
         })
      }
   }

   private val outputReader = new RichAudioBus.User {
      def busChanged( bus: AudioBus )( implicit tx: ProcTxn ) {
         targetNode.mapan( true, target.name -> bus )
      }
   }

   protected def addOutputConsumers( implicit tx: ProcTxn ) {
      output.addReader( outputReader )
      output.addWriter( outputWriter )
   }

   protected def removeOutputConsumers( implicit tx: ProcTxn ) {
      output.removeReader( outputReader )
      output.removeWriter( outputWriter )
   }
}

class ControlKBusMapping( val source: ProcAudioOutput, val target: ControlImpl )
extends ControlBusMapping with ControlToKMapping {
   override def toString = "aIn(" + proc.name + " @ " + name + ")"

   def output( implicit tx: ProcTxn ) = {
      outputRef().getOrElse({
         val inBus   = bus.get.busOption.get
         val res     = RichBus.control( inBus.server, inBus.numChannels )
         outputRef.set( Some( res ))
         res
      })
   }

   protected def targetNode( implicit tx: ProcTxn ) = target.proc.group // XXX anyway wrong!

   protected def graph( inBus: AudioBus ) = SynthGraph {
      val in      = A2K.kr( In.ar( "$in".kr, inBus.numChannels ))
      val clipped = Clip.kr( in, -1, 1 )
      val out     = target.spec.map( clipped.madd( 0.5f, 0.5f ))
      Out.kr( "$out".kr, out )
   }
}

class ControlABusMapping( val source: ProcAudioOutput, val target: ControlImpl )
extends ControlBusMapping with ControlToAMapping {
   override def toString = "aIn(" + proc.name + " @ " + name + ")"

   def output( implicit tx: ProcTxn ) = {
      outputRef().getOrElse({
         val inBus   = bus.get.busOption.get
         val res     = RichBus.audio( inBus.server, inBus.numChannels )
         outputRef.set( Some( res ))
         res
      })
   }

   protected def targetNode( implicit tx: ProcTxn ) = target.proc.group // XXX anyway wrong!

   protected def graph( inBus: AudioBus ) = SynthGraph {
      val in      = In.ar( "$in".kr, inBus.numChannels )
      val clipped = Clip.ar( in, -1, 1 )
      val out     = target.spec.map( clipped.madd( 0.5f, 0.5f ))
      Out.ar( "$out".kr, out )
   }
}