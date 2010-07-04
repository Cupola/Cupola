package de.sciss.synth.proc.impl

import de.sciss.synth.AudioBus
import de.sciss.synth.proc.{RichGroup, ProcTxn, ProcRunning, RichSynth}

class RunningGraphImpl( rs: RichSynth ) extends ProcRunning {
   import ProcRunning._

   rs.synth.onEnd {
      dispatch( Stopped )
   }

   def stop( implicit tx: ProcTxn ) = {
      rs.free()
   }

   def setString( name: String, value: String )( implicit tx: ProcTxn ) { error( "not yet supported" )}

   def setFloat( name: String, value: Float )( implicit tx: ProcTxn ) {
      rs.set( true, name -> value )
   }

   def busChanged( name: String, bus: AudioBus )( implicit tx: ProcTxn ) {
      // XXX check numChannels
      rs.set( true, name -> bus.index )
   }

   def setGroup( g: RichGroup )( implicit tx: ProcTxn ) {
      rs.moveToHead( true, g )
   }
}

