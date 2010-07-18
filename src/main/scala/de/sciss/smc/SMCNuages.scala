package de.sciss.smc

import de.sciss.synth._
import de.sciss.synth.ugen._
import de.sciss.synth.proc._

/**
 * Created by IntelliJ IDEA.
 * User: rutz
 * Date: 17.07.2010
 * Time: 23:53:38
 * To change this template use File | Settings | File Templates.
 */

object SMCNuages {
   import DSL._

   var freesoundFile : Option[ String ] = None

   def init( s: Server ) = ProcTxn.atomic { implicit tx =>
      gen( "Free" ) {
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

      diff( "Out" ) {
          val pamp  = pAudio( "amp", ParamSpec( 0.01, 10, ExpWarp ), 1 )
          val pout  = pAudioOut( "out", Some( RichBus.soundOut( s, 2 )))

          graph { in => pout.ar( in * pamp.ar )}
      }
   }
}