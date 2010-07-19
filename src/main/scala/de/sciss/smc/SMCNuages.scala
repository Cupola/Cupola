/*
 *  SMCNuages.scala
 *  (Cupola)
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

package de.sciss.smc

import de.sciss.synth.{ EnvSeg => S, _ }
import de.sciss.synth.ugen._
import de.sciss.synth.proc._
import de.sciss.nuages.NuagesFrame
import com.jhlabs.jnitablet.{TabletProximityEvent, TabletEvent, TabletListener, TabletWrapper}
import java.util.TimerTask
import java.awt.event.MouseEvent
import collection.breakOut

/**
 *    @version 0.11, 19-Jul-10
 */
object SMCNuages extends TabletListener {
   import DSL._

   val USE_TABLET       = true
   val DEBUG_PROXIMITY  = false

   var freesoundFile : Option[ String ] = None
   var f : NuagesFrame = null

   def init( s: Server, f: NuagesFrame ) = ProcTxn.atomic { implicit tx =>
      // -------------- GENERATORS --------------

      // NuagesULoop
      // NuagesUMic

      gen( "free" ) {
         val pspeed  = pAudio( "speed", ParamSpec( 0.1f, 10, ExpWarp ), 1 )
         val ploop   = pControl( "loop", ParamSpec( 0, 1, LinWarp, 1 ), 0 )
          graph {
             val b       = bufCue( freesoundFile.getOrElse( error( "No freesound file selected" )))
             val numCh  = b.numChannels
             val sig    = VDiskIn.ar( numCh, b.id, pspeed.ar * BufRateScale.ir( b.id ), loop = ploop.kr ).outputs.take(2)
             if( numCh == 1 ) List( sig( 0 ), sig( 0 )) else sig
          }
      }

      // -------------- FILTERS --------------

      def mix( in: GE, flt: GE, mix: ProcParamAudio ) : GE = LinXFade2.ar( in, flt, mix.ar * 2 - 1 )
      def pMix = pAudio( "mix", ParamSpec( 0, 1 ), 1 ) 

      // NuagesUHilbert
      // NuagesUMagBelow

      filter( "achil") {
         val pspeed  = pAudio( "speed", ParamSpec( 0.125, 2.3511, ExpWarp ), 0.5 )
         val pmix    = pMix

         graph { in =>
            val speed	   = Lag.ar( pspeed.ar, 0.1 )
            val numFrames  = sampleRate.toInt
            val numChannels= in.numOutputs
            val buf        = bufEmpty( numFrames, numChannels )
            val bufID      = buf.id
            val writeRate  = BufRateScale.kr( bufID )
            val readRate   = writeRate * speed
            val readPhasor = Phasor.ar( 0, readRate, 0, numFrames )
            val read			= BufRd.ar( numChannels, bufID, readPhasor, 0, 4 )
            val writePhasor= Phasor.ar( 0, writeRate, 0, numFrames )
            val old			= BufRd.ar( numChannels, bufID, writePhasor, 0, 1 )
            val wet0 		= SinOsc.ar( 0, ((readPhasor - writePhasor).abs / numFrames * math.Pi) )
            val dry			= 1 - wet0.squared
            val wet			= 1 - (1 - wet0).squared
            BufWr.ar( (old * dry) + (in * wet), bufID, writePhasor )
            mix( in, read, pmix )
         }
      }

      filter( "a-gate" ) {
         val pamt = pAudio( "amt", ParamSpec( 0, 1 ), 1 )
         val pmix = pMix
         graph { in =>
            val amount = Lag.ar( pamt.ar, 0.1 )
            val flt = Compander.ar( in, in, Amplitude.ar( in * (1 - amount ) * 5 ), 20, 1, 0.01, 0.001 )
            mix( in, flt, pmix )
         }
      }

      filter( "a-hilb" ) {
         val pmix = pMix
         graph { in =>
            var flt: GE = List.fill( in.numOutputs )( 0.0 )
            in.outputs foreach { ch =>
               val hlb  = Hilbert.ar( DelayN.ar( ch, 0.01, 0.01 ))
               val hlb2 = Hilbert.ar( Normalizer.ar( ch, dur = 0.02 ))
               flt     += (hlb \ 0) * (hlb2 \ 0) - (hlb \ 1 * hlb2 \ 1)
            }
            mix( in, flt, pmix )
         }
      }

      filter( "filt" ) {
         val pfreq   = pAudio( "freq", ParamSpec( -1, 1 ), 0.54 )
         val pmix    = pMix
         
         graph { in =>
            val normFreq	= pfreq.ar
            val lowFreqN	= Lag.ar( Clip.ar( normFreq, -1, 0 ))
            val highFreqN	= Lag.ar( Clip.ar( normFreq,  0, 1 ))
            val lowFreq		= LinExp.ar( lowFreqN, -1, 0, 30, 20000 )
            val highFreq	= LinExp.ar( highFreqN, 0, 1, 30, 20000 )
            val lowMix		= Clip.ar( lowFreqN * -10.0, 0, 1 )
            val highMix		= Clip.ar( highFreqN * 10.0, 0, 1 )
            val dryMix		= 1 - (lowMix + highMix)
            val lpf			= LPF.ar( in, lowFreq ) * lowMix
            val hpf			= HPF.ar( in, highFreq ) * highMix
            val dry			= in * dryMix
            val flt			= dry + lpf + hpf
            mix( in, flt, pmix )
         }
      }

      filter( "frgmnt" ) {
   		val pspeed  = pAudio(   "speed", ParamSpec( 0.125, 2.3511, ExpWarp ), 1 )
		   val pgrain  = pControl( "grain", ParamSpec( 0, 1 ), 0.5 )
		   val pfeed   = pAudio(   "fb",    ParamSpec( 0, 1 ), 0 )
         val pmix    = pMix

         graph { in =>
            val bufDur        = 4.0
            val numFrames     = (bufDur * sampleRate).toInt
            val numChannels   = in.numOutputs
            val buf           = bufEmpty( numFrames, numChannels )
            val bufID         = buf.id

            val feedBack	   = Lag.ar( pfeed.ar, 0.1 )
            val grain	      = pgrain.kr // Lag.kr( grainAttr.kr, 0.1 )
            val maxDur	      = LinExp.kr( grain, 0, 0.5, 0.01, 1.0 )
            val minDur	      = LinExp.kr( grain, 0.5, 1, 0.01, 1.0 )
            val fade		      = LinExp.kr( grain, 0, 1, 0.25, 4 )
            val rec		      = (1 - feedBack).sqrt
            val pre		      = feedBack.sqrt
            val trig		      = LocalIn.kr( 1 )
            val white	      = TRand.kr( 0, 1, trig )
            val dur		      = LinExp.kr( white, 0, 1, minDur, maxDur )
            val off0		      = numFrames * white
            val off		      = off0 - (off0 % 1.0)
            val gate		      = trig
            val lFade	      = Latch.kr( fade, trig )
            val fadeIn	      = lFade * 0.05
            val fadeOut	      = lFade * 0.15
            val env 		      = EnvGen.ar( Env.linen( fadeIn, dur, fadeOut, 1, sinShape ), gate, doneAction = 0 )
            val recLevel0     = env.sqrt
            val preLevel0     = (1 - env).sqrt
            val recLevel      = recLevel0 * rec
            val preLevel      = preLevel0 * (1 - pre) + pre
            val run           = recLevel > 0
            RecordBuf.ar( in, bufID, off, recLevel, preLevel, run, 1 )
            LocalOut.kr( Impulse.kr( 1.0 / (dur + fadeIn + fadeOut ).max( 0.01 )))

      	   val speed      = pspeed.ar
			   val play		   = PlayBuf.ar( numChannels, bufID, speed, loop = 1 )
            mix( in, play, pmix )
      	}
      }

      filter( "gain" ) {
         val pgain   = pAudio( "gain", ParamSpec( -30, 30 ), 0 )
         val pmix = pMix
         graph { in =>
            val amp  = pgain.ar.dbamp
            val flt  = in * amp 
            mix( in, flt, pmix )
         }
      }

      filter( "gendy" ) {
         val pamt = pAudio( "amt", ParamSpec( 0, 1 ), 1 )
         val pmix = pMix
         graph { in =>
            val amt     = Lag.ar( pamt.ar, 0.1 )
            val minFreq	= amt * 69 + 12;
            val scale	= amt * 13 + 0.146;
            val gendy   = Gendy1.ar( 2, 3, 1, 1,
                     minFreq = minFreq, maxFreq = minFreq * 8,
                     ampScale = scale, durScale = scale,
                     initCPs = 7, kNum = 7 ) * in
            val flt	   = Compander.ar( gendy, gendy, 0.7, 1, 0.1, 0.001, 0.02 )
            mix( in, flt, pmix )
         }
      }

// XXX currently crashes scsynth
/*
      filter( "m-above" ) {
         val pthresh = pControl( "thresh", ParamSpec( 1.0e-3, 1.0e-1, ExpWarp ), 1.0e-2 )
         val pmix = pMix
         graph { in =>
            val numChannels   = in.numOutputs
            val thresh		   = pthresh.kr
            val env			   = Env( 0.0, List( S( 0.2, 0.0, stepShape ), S( 0.2, 1.0, linShape )))
            val ramp			   = EnvGen.kr( env )
            val volume		   = LinLin.kr( thresh, 1.0e-3, 1.0e-1, 32, 4 )
            val bufIDs        = List.fill( numChannels )( bufEmpty( 1024 ).id )
            val chain1 		   = FFT( bufIDs, HPZ1.ar( in ))
            val chain2        = PV_MagAbove( chain1, thresh )
            val flt			   = LPZ1.ar( volume * IFFT( chain2 )) * ramp

            // account for initial dly
            val env2          = Env( 0.0, List( S( BufDur.kr( bufIDs ) * 2, 0.0, stepShape ), S( 0.2, 1, linShape )))
            val wet			   = EnvGen.kr( env2 )
            val sig			   = (in * (1 - wet).sqrt) + (flt * wet)
            mix( in, flt, pmix )
         }
      }
*/
      filter( "pitch" ) {
         val ptrans  = pControl( "shift", ParamSpec( 0.125, 4, ExpWarp ), 1 )
         val ptime   = pControl( "time",  ParamSpec( 0.01, 1, ExpWarp ), 0.1 )
         val ppitch  = pControl( "pitch", ParamSpec( 0.01, 1, ExpWarp ), 0.1 )
         val pmix    = pMix
         graph { in =>
            val grainSize  = 0.5f
            val pitch	   = ptrans.kr
            val timeDisp	= ptime.kr
            val pitchDisp	= ppitch.kr
            val flt		   = PitchShift.ar( in, grainSize, pitch, pitchDisp, timeDisp * grainSize )
            mix( in, flt, pmix )
         }
      }

      filter( "pow" ) {
         val pamt = pAudio( "amt", ParamSpec( 0, 1 ), 0.5 )
         val pmix = pMix
         graph { in =>
            val amt  = pamt.ar
            val amtM = 1 - amt
            val exp  = amtM * 0.5 + 0.5
            val flt0 = in.abs.pow( exp ) * in.signum
            val amp0 = Amplitude.ar( flt0 )
            val amp  = amtM + (amp0 * amt)
//            val flt  = LeakDC.ar( flt0 ) * amp
            val flt  = flt0 * amp
            mix( in, flt, pmix )
         }
      }

// XXX this has problems with UDP max datagram size
/*
      filter( "renoise" ) {
         val pcolor  = pAudio( "color", ParamSpec( 0, 1 ), 0 )
         val pmix    = pMix
         val step	   = 0.5
         val freqF   = math.pow( 2, step )
         val freqs	= Array.iterate( 32.0, 40 )( _ * freqF ).filter( _ <= 16000 )
         graph { in =>
            val color         = Lag.ar( pcolor.ar, 0.1 )
            val numChannels   = in.numOutputs
            val noise	      = WhiteNoise.ar( numChannels )
            val sig           = freqs.foldLeft[ GE ]( 0 ){ (sum, freq) =>
               val filt       = BPF.ar( in, freq, step )
               val freq2      = ZeroCrossing.ar( filt )
               val w0         = Amplitude.ar( filt )
               val w2         = w0 * color
               val w1         = w0 * (1 - color)
               sum + BPF.ar( (noise * w1) + (LFPulse.ar( freq2 ) * w2), freq, step )
            }
            val amp           = step.reciprocal  // compensate for Q
            val flt           = sig * amp
            mix( in, flt, pmix )
         }
      }
*/
   
// XXX THIS CURRENTLY CRASHES SCSYNTH
/*
      filter( "verb" ) {
         val pextent = pControl( "size", ParamSpec( 0, 1 ), 0.5 ) // XXX pScalar
         val pcolor  = pControl( "color", ParamSpec( 0, 1 ), 0.5 )
         val pmix    = pMix
         graph { in =>
            val extent     = pextent.kr
            val color	   = Lag.kr( pcolor.kr, 0.1 )
            val i_trig     = Impulse.kr( 0 )
            val i_roomSize	= Latch.kr( LinExp.kr( extent, 0, 1, 1, 100 ), i_trig ) // XXX bit stupid
            val i_revTime  = Latch.kr( LinExp.kr( extent, 0, 1, 0.3, 20 ), i_trig )
            val spread	   = 15
            val numChannels= in.numOutputs
            val ins        = in.outputs
            val verbs      = (ins :+ ins.last).grouped( 2 ).toSeq.flatMap( pair =>
               (GVerb.ar( Mix( pair ), i_roomSize, i_revTime, color, color, spread, 0, 1, 0.7, i_roomSize ) * 0.3).outputs
            )
// !! BUG IN SCALA 2.8.0 : CLASSCASTEXCEPTION
// weird stuff goin on with UGenIn seqs...
            val flt: GE     = Vector( verbs.take( numChannels ): _* ) // drops last one if necessary
            mix( in, flt, pmix )
         }
      }
*/

      filter( "zero" ) {
         val pwidth	= pAudio( "width", ParamSpec( 0, 1 ), 0.5 )
         val pdiv 	= pAudio( "div",   ParamSpec( 1, 10, LinWarp, 1 ), 1 )
         val plag	   = pAudio( "lag",   ParamSpec( 0.001, 0.1, ExpWarp ), 0.01 )
         val pmix    = pMix
         graph { in =>
            val freq		= ZeroCrossing.ar( in ).max( 20 )
            val width0  = Lag.ar( pwidth.ar, 0.1 )
            val amp		= width0.sqrt
            val width	= width0.reciprocal
            val div		= Lag.ar( pdiv.ar, 0.1 )
            val lagTime	= plag.ar
            val pulse   = Lag.ar( LFPulse.ar( freq / div, 0, width ) * amp, lagTime )
            val flt		= in * pulse
            mix( in, flt, pmix )
         }
      }

      // -------------- DIFFUSIONS --------------

      diff( "out" ) {
          val pamp  = pAudio( "amp", ParamSpec( 0.01, 10, ExpWarp ), 1 )
          val pout  = pAudioOut( "out", SMC.config.masterBus.map( RichBus.wrap( _ )))

          graph { in => pout.ar( in * pamp.ar )}
      }

      // tablet
      this.f = f
      if( USE_TABLET ) {
//         new java.util.Timer().schedule( new TimerTask {
//            def run {
               val inst = TabletWrapper.getInstance
               inst.addTabletListener( SMCNuages )
//               inst.removeTabletListener( this )
//               inst.addTabletListener( this )
      println(" TABLET ")
//            }
//         }, 5000 )
      }
   }

//	 ---------------- TabletListener interface ----------------

   private var wasInstant = false

   def tabletEvent( e: TabletEvent ) {
      if( !f.isActive() ) return
//      println( e.getTiltY )

      if( (e.getButtonMask() & 0x02) != 0 ) {
         if( e.getID() != MouseEvent.MOUSE_RELEASED ) {
            f.transition.setTransition( 2, e.getTiltY() * -0.5 + 0.5 )
         }
      } else {
         if( !wasInstant ) {
            f.transition.setTransition( 0, 0 )
            wasInstant = true
         }
      }


//      switch( e.getID() ) {
//      case MouseEvent.MOUSE_DRAGGED:
//         // ignore messages that originate from drags that started outside the view
//         if( !pressed ) return;
//         break;
//      case MouseEvent.MOUSE_MOVED:
//         // ignore messages that originate from moves that left the view
//         if( !inside ) return;
//         break;
//      case MouseEvent.MOUSE_PRESSED:
//         // ignore messages that originate from clicking outside the view
//         if( !inside ) return;
//         break;
//      case MouseEvent.MOUSE_RELEASED:
//         // ignore messages that originate from clicking outside the view
//         if( !pressed ) return;
//         break;
//      default:
//         break;
//      }

//      for( Iterator iter = listeners.iterator(); iter.hasNext(); ) {
//         ((TabletListener) iter.next()).tabletEvent( e );
//      }

//		println( "TabletEvent" )
//		System.out.println( "  id                         " + e.getID() );
//		System.out.println( "  x                          " + e.getX() );
//		System.out.println( "  y                          " + e.getY() );
//		System.out.println( "  absoluteY                  " + e.getAbsoluteY() );
//		System.out.println( "  absoluteX                  " + e.getAbsoluteX() );
//		System.out.println( "  absoluteZ                  " + e.getAbsoluteZ() );
//		System.out.println( "  buttonMask                 " + e.getButtonMask() );
//		System.out.println( "  pressure                   " + e.getPressure() );
//		System.out.println( "  rotation                   " + e.getRotation() );
//		System.out.println( "  tiltX                      " + e.getTiltX() );
//		System.out.println( "  tiltY                      " + e.getTiltY() );
//		System.out.println( "  tangentialPressure         " + e.getTangentialPressure() );
//		System.out.println( "  vendorDefined1             " + e.getVendorDefined1() );
//		System.out.println( "  vendorDefined2             " + e.getVendorDefined2() );
//		System.out.println( "  vendorDefined3             " + e.getVendorDefined3() );
//		System.out.println();
   }

   def tabletProximity( e: TabletProximityEvent ) {
//      if( e.isEnteringProximity() ) {
//         if( inside ) {
//            lastEnterEvent	= null;
//            dispatch( e );
//            dispatchExit	= true;
//         } else {
//            lastEnterEvent	= e;
//         }
//      } else {
//         if( dispatchExit ) {
//            dispatchExit	= false;
//            dispatch( e );
//         }
//      }

      if( DEBUG_PROXIMITY ) {
         println( "TabletProximityEvent" )
         println( "  capabilityMask             " + e.getCapabilityMask )
         println( "  deviceID                   " + e.getDeviceID )
         println( "  enteringProximity          " + e.isEnteringProximity )
         println( "  pointingDeviceID           " + e.getPointingDeviceID )
         println( "  pointingDeviceSerialNumber " + e.getPointingDeviceSerialNumber )
         println( "  pointingDeviceType         " + e.getPointingDeviceType )
         println( "  systemTabletID             " + e.getSystemTabletID )
         println( "  tabletID                   " + e.getTabletID )
         println( "  uniqueID                   " + e.getUniqueID )
         println( "  vendorID                   " + e.getVendorID )
         println( "  vendorPointingDeviceType   " + e.getVendorPointingDeviceType )
         println()
      }
   }
}