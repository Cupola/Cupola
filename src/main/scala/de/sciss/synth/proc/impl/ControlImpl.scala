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

/**
 *    @version 0.12, 09-Jul-10
 */
class ControlImpl( val proc: ProcImpl, param: ProcParamFloat, val _rate: Rate )
extends ProcControl {
   ctrl =>

//   private var valueRef = Ref( default )
   private var valueRef = Ref( ControlValue.instant( default ))

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

   def v( implicit tx: ProcTxn ) : Double = valueRef().current

   def v_=( newValue: Double )( implicit tx: ProcTxn ) {
      val oldCV   = valueRef()
      val transit = tx.transit
      val newCVO  = oldCV.mapping match {
         case None => {
            transit match {
               case Instant => {
                  if( oldCV.target != newValue ) {
                     Some( oldCV.copy( target = newValue ))
                  } else None
               }
               case fade: XFade => {
                  if( oldCV.target != newValue ) {
                     proc.sendToBack( fade )
                     Some( oldCV.copy( target = newValue ))
                  } else None
               }
               case glide: Glide => {
                  val current = oldCV.target
                  if( current != newValue ) {
                     val startNorm  = spec.unmap( current )
                     val targetNorm = spec.unmap( newValue )
                     val newCG = _rate match {
                        case `control` => new ControlKGliding( ctrl, startNorm, targetNorm, glide )
                        case `audio`   => new ControlAGliding( ctrl, startNorm, targetNorm, glide )
                        case _ => error( "Cannot glide rate " + _rate )
                     }
                     Some( ControlValue( newValue, Some( newCG )))
                  } else None
               }
            }
         }
         case Some( _: ControlBusMapping ) => {
            if( oldCV.target != newValue ) {
               // just remember the value, but don't do anything
               // ; we could also undo the mapping hereby?
               Some( oldCV.copy( target = newValue ))
            } else None
         }
         case Some( oldCG: ControlGliding ) => {
            transit match {
               case Instant => {
                  val current = oldCG.currentValue
                  oldCG.stop
                  Some( ControlValue.instant( newValue ))
               }
               case fade: XFade => {
                  error( "NOT YET IMPLEMENTED" ) // XXX missing: we need to stop the gliss just for the new synth!
//                  proc.sendToBack( fade )
//                  Some( ControlValue.instant( newValue ))
               }
               case glide: Glide => {
                  val startNorm  = spec.unmap( oldCG.currentValue )
                  val targetNorm = spec.unmap( newValue )
                  oldCG.stop
                  val newCG = _rate match {
                     case `control` => new ControlKGliding( ctrl, startNorm, targetNorm, glide )
                     case `audio`   => new ControlAGliding( ctrl, startNorm, targetNorm, glide )
                     case _ => error( "Cannot glide rate " + _rate )
                  }
                  Some( ControlValue( newValue, Some( newCG )))
               }
            }
         }
      }
      newCVO.foreach( newCV => {
         valueRef.set( newCV )
         proc.controlChanged( ctrl, newCV )
      })
   }

   private[proc] def glidingDone( implicit tx: ProcTxn ) { v = cv.target } 

   def map( aout: ProcAudioOutput )( implicit tx: ProcTxn ) : ControlABusMapping = {
      val oldCV = valueRef()
      oldCV.mapping match {
         case None => // Ok
         case Some( cg: ControlGliding ) => {
            v = cg.currentValue // stops the thing
         }
         case Some( m: ControlBusMapping ) => error( "Already mapped" )
      }
      val m = _rate match {
         case `control` => new ControlABusToKMapping( aout, ctrl )
         case `audio` => new ControlABusToAMapping( aout, ctrl )
         case _ => error( "Cannot map rate " + _rate )
      }
      m.init
      val newCV = ControlValue( oldCV.current, Some( m ))
      valueRef.set( newCV )
//      proc.controlMapped( ctrl, newCV )
      proc.controlChanged( ctrl, newCV )
      m
   }

   def isMapable = true
   def canMap( aout: ProcAudioOutput )( implicit tx: ProcTxn ) : Boolean = !isMapped
}

trait ControlMappingImpl /* extends ControlMapping*/ {
   protected val synth   = Ref[ Option[ RichSynth ]]( None )

   // ---- abstract ----
   def target: ControlImpl
   def play( implicit tx: ProcTxn ) : Unit
   protected def addOutputConsumers( implicit tx: ProcTxn ) : Unit
   protected def removeOutputConsumers( implicit tx: ProcTxn ) : Unit
//   protected def targetNode( implicit tx: ProcTxn ): RichNode

   def proc    = target.proc
   def name    = target.name + "#map"

   def stop( implicit tx: ProcTxn ) {
      // we set synth to None first, so
      // the removeReader calls don't produce
      // unnecessary n_set messages
      synth.swap( None ).foreach( _.free( true ))
      removeOutputConsumers
   }

//   def isPlaying( implicit tx: ProcTxn ) : Boolean = synth().map( _.isOnline.get ).getOrElse( false )
}

trait ControlGlidingImpl
extends ControlGliding with ControlMappingImpl {
//   def cv: ControlValue

   def isPlaying( implicit tx: ProcTxn ) : Boolean = synth().map( _.isOnline.get ).getOrElse( false )
   
   def play( implicit tx: ProcTxn ) {
      val g       = graph
      val rsd     = RichSynthDef( target.proc.server, g )
//      val dur     = cv.transit.asInstanceOf[ Glide ].dur // XXX not so pretty
      val spec    = target.spec
//      val startN  = spec.unmap( /*spec.clip(*/ startValue /*)*/)
//      val targetN = spec.unmap( targetValue )
      val rs      = rsd.play( target.proc.preGroup,
         List( "$start" -> startNorm, "$stop" -> targetNorm, "$dur" -> glide.dur ))

      val oldSynth = synth.swap( Some( rs ))
      addOutputConsumers   // requires that synth has been assigned!
      oldSynth.foreach( _.free( true ))

      rs.synth.onEnd { ProcTxn.atomic { tx0 =>
         synth()( tx0 ).foreach( rs2 => if( rs == rs2 ) {
            synth.set( None )( tx0 )
            target.glidingDone( tx0 )  // invokes stop and hence removeOutputConsumers!
         })
      }}
   }

   protected def graph : SynthGraph
}

class ControlKGliding( val target: ControlImpl, val startNorm: Double, val targetNorm: Double, val glide: Glide )
extends ControlGlidingImpl with ControlToKMapping {
   def output( implicit tx: ProcTxn ) = {
      outputRef().getOrElse({
         val res = RichBus.control( proc.server, 1 ) // XXX numChannels
         outputRef.set( Some( res ))
         res
      })
   }

//   protected def targetNode( implicit tx: ProcTxn ) = target.proc.group // XXX anyway wrong!

   protected def graph = SynthGraph {
      val line    = Line.kr( "$start".ir, "$stop".ir, "$dur".ir, freeSelf )
      val out     = target.spec.map( line )
      // XXX multichannel expansion
      Out.kr( "$out".kr, out )
   }
}

class ControlAGliding( val target: ControlImpl, val startNorm: Double, val targetNorm: Double, val glide: Glide )
extends ControlGlidingImpl with ControlToAMapping {
   def output( implicit tx: ProcTxn ) = {
      outputRef().getOrElse({
         val res = RichBus.audio( proc.server, 1 ) // XXX numChannels
         outputRef.set( Some( res ))
         res
      })
   }

//   protected def targetNode( implicit tx: ProcTxn ) = target.proc.group // XXX anyway wrong!

   protected def graph = SynthGraph {
      val line    = Line.ar( "$start".ir, "$stop".ir, "$dur".ir, freeSelf )
      val out     = target.spec.map( line )
      // XXX multichannel expansion
      Out.ar( "$out".kr, out )
   }
}

abstract class ControlABusMappingImpl
extends AbstractAudioInputImpl with ControlMappingImpl with ControlABusMapping {
   def source: ProcAudioOutput
//      def connect( implicit tx: ProcTxn ) { source ~> this } // XXX NO WE DON'T NEED TO ENFORCE TOPOLOGY !!!

   lazy val edge = ProcEdge( source, this )

//      protected val edges = Ref( Set.empty[ ProcEdge ])

//      def input : ProcAudioInput = this

   def init( implicit tx: ProcTxn ) {
      addEdge( edge )
   }

   def sendToBack( xfade: XFade, backGroup: RichGroup )( implicit tx: ProcTxn ) {
// XXX what now?
//      death.node.onEnd {
//
//      }
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
      val rs      = rsd.play( source.proc.postGroup, List( "$in" -> inBus.index ), addToTail )

      val oldSynth = synth.swap( Some( rs ))
      addOutputConsumers   // requires that synth has been assigned!
      oldSynth.foreach( _.free( true ))
   }

   protected def graph( inBus: AudioBus ) : SynthGraph
}

trait ControlToKMapping extends ControlMappingImpl {
   protected var outputRef = Ref[ Option[ RichControlBus ]]( None )

   // ---- abstract ----
   def output( implicit tx: ProcTxn ) : RichControlBus

   private val outputReader = new RichControlBus.User {
      def busChanged( bus: ControlBus )( implicit tx: ProcTxn ) {
         target.proc.anchorNode.mapn( true, target.name -> bus )
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

trait ControlToAMapping extends ControlMappingImpl {
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
         target.proc.anchorNode.mapan( true, target.name -> bus )
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

class ControlABusToKMapping( val source: ProcAudioOutput, val target: ControlImpl )
extends ControlABusMappingImpl with ControlToKMapping {
   override def toString = "aIn(" + proc.name + " @ " + name + ")"

   def output( implicit tx: ProcTxn ) = {
      outputRef().getOrElse({
         val inBus   = bus.get.busOption.get
         val res     = RichBus.control( inBus.server, inBus.numChannels )
         outputRef.set( Some( res ))
         res
      })
   }

//   protected def targetNode( implicit tx: ProcTxn ) = target.proc.group // XXX anyway wrong!

   protected def graph( inBus: AudioBus ) = SynthGraph {
      val in      = A2K.kr( In.ar( "$in".kr, inBus.numChannels ))
      val clipped = Clip.kr( in, -1, 1 )
      val out     = target.spec.map( clipped.madd( 0.5f, 0.5f ))
      Out.kr( "$out".kr, out )
   }
}

class ControlABusToAMapping( val source: ProcAudioOutput, val target: ControlImpl )
extends ControlABusMappingImpl with ControlToAMapping {
   override def toString = "aIn(" + proc.name + " @ " + name + ")"

   def output( implicit tx: ProcTxn ) = {
      outputRef().getOrElse({
         val inBus   = bus.get.busOption.get
         val res     = RichBus.audio( inBus.server, inBus.numChannels )
         outputRef.set( Some( res ))
         res
      })
   }

//   protected def targetNode( implicit tx: ProcTxn ) = target.proc.group // XXX anyway wrong!

   protected def graph( inBus: AudioBus ) = SynthGraph {
      val in      = In.ar( "$in".kr, inBus.numChannels )
      val clipped = Clip.ar( in, -1, 1 )
      val out     = target.spec.map( clipped.madd( 0.5f, 0.5f ))
      Out.ar( "$out".kr, out )
   }
}