package de.sciss.smc

import de.sciss.synth._
import de.sciss.synth.ugen._
import de.sciss.synth.proc._
import de.sciss.nuages.NuagesFrame
import com.jhlabs.jnitablet.{TabletProximityEvent, TabletEvent, TabletListener, TabletWrapper}
import java.util.TimerTask
import java.awt.event.MouseEvent

/**
 * Created by IntelliJ IDEA.
 * User: rutz
 * Date: 17.07.2010
 * Time: 23:53:38
 * To change this template use File | Settings | File Templates.
 */

object SMCNuages extends TabletListener {
   import DSL._

   val USE_TABLET       = true
   val DEBUG_PROXIMITY  = false

   var freesoundFile : Option[ String ] = None
   var f : NuagesFrame = null

   def init( s: Server, f: NuagesFrame ) = ProcTxn.atomic { implicit tx =>
      // -------------- GENERATORS --------------

      gen( "free" ) {
         val pspeed  = pAudio( "speed", ParamSpec( 0.1f, 10, ExpWarp ), 1 )
         val ploop   = pControl( "loop", ParamSpec( 0, 1, LinWarp, 1 ), 0 )
         val b       = bufCue( "disk", {
//            println( "FREESOUNDFILE" )
            freesoundFile.getOrElse( error( "No freesound file selected" ))
         })

          graph {
             val numCh = b.numChannels
             val sig   = VDiskIn.ar( numCh, b.id, pspeed.ar * BufRateScale.ir( b.id ), loop = ploop.kr ).outputs.take(2)
             if( numCh == 1 ) List( sig( 0 ), sig( 0 )) else sig
          }
      }

      // -------------- FILTERS --------------

      def mix( in: GE, flt: GE, mix: ProcParamAudio ) : GE = LinXFade2.ar( in, flt, mix.ar * 2 - 1 )
      def pMix = pAudio( "mix", ParamSpec( 0, 1 ), 1 ) 

      // NuagesUAchilles
      // NuagesUFragment

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
         val pfreq = pAudio( "freq", ParamSpec( -1, 1 ), 0.54 )
         val pmix = pMix
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