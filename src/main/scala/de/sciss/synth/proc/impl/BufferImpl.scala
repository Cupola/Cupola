package de.sciss.synth.proc.impl

import de.sciss.synth.proc._
import de.sciss.synth._
import de.sciss.synth.io.AudioFile

// XXX either BufferCueImpl or put cueing information in Graph instead
class BufferImpl( val name: String, path: ProcTxn => String ) extends ProcBuffer {
   def controlName   = "buf$" + name

   def create( server: Server )( implicit tx: ProcTxn ) : RichBuffer = {
      val b = Buffer( server )
      val rb = RichBuffer( b )
      rb.alloc( 32768, numChannels )
      rb.cue( path( tx ))
      rb
   }

   def disposeWith( rb: RichBuffer, rs: RichSynth ) {
      rs.synth.onEnd { rb.server ! rb.buf.closeMsg( rb.buf.freeMsg )} // XXX update RichBuffer fields !
   }

   def numChannels : Int = {
      try { // XXX should call includeBuffer ?
         val spec = AudioFile.readSpec( path( ProcGraphBuilder.local.tx ))
         spec.numChannels
      } catch {
         case e => e.printStackTrace()
         1  // XXX what should we do? --> FAIL
      }
   }

   def id : GE = {
      ProcGraphBuilder.local.includeBuffer( this )
      controlName.kr
   }
}
