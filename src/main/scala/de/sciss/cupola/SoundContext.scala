package de.sciss.cupola

import collection.{ Set => ISet }
import de.sciss.synth._
import de.sciss.synth.ugen._
import de.sciss.synth.proc._
import Util._

case class SoundContext( name: String, settings: SoundSettings,
                         scaleStart: Double, scaleStop: Double, weight: Double,
                         minConc: Int, maxConc: Int,
                         minDur: Double, maxDur: Double,
                         minFade: Double, maxFade: Double,
                         field: Field,
                         mutex: ISet[ String ])

sealed abstract class SoundSettings {
//   def toXML : Node
   def createProcFactory( name: String )( implicit tx: ProcTxn ) : ProcFactory
   def prepareForPlay( proc: Proc )( implicit tx: ProcTxn ) : Unit
}

case class TapeSoundSettings( file: String, gain: Double, speed: Double )
extends SoundSettings {
   def createProcFactory( name: String )( implicit tx: ProcTxn ) : ProcFactory = {
      import DSL._
      ProcDemiurg.factories.find( _.name == name ) getOrElse gen( name ) {
         val pspeed  = pControl( "speed", ParamSpec( 0.1f, 10, ExpWarp ), speed )
         val pamp    = pControl( "amp",   ParamSpec( 0.1f, 10, ExpWarp ), gain.dbamp )
         val ppos    = pScalar(  "pos",   ParamSpec( 0, 1 ), 0 )
         graph {
            val fullPath   = Cupola.BASE_PATH + "audio_work/material/" + file
            val afSpec     = audioFileSpec( fullPath )
            val startPos   = ppos.v
            val startFrame = (startPos * afSpec.numFrames).toLong
            val buf        = bufCue( fullPath, startFrame )
            val bufID      = buf.id
            val speed      = pspeed.kr * BufRateScale.ir( bufID )
            val d          = VDiskIn.ar( afSpec.numChannels, bufID, speed, loop = 1 )
//               val frame   = d.reply
//               (frame.carry( pspeed.v * b.sampleRate ) / b.numFrames) ~> ppos
            val liveFrame  = Integrator.ar( K2A.ar( speed ))
            val livePos    = ((liveFrame / BufFrames.ir( bufID )) + startPos) % 1.0f
//               livePos ~> ppos
            d * pamp.kr
         }
      }
   }

   def prepareForPlay( proc: Proc )( implicit tx: ProcTxn ) {
      proc.control( "pos" ).v = rand( 0.95 )
   }
}
