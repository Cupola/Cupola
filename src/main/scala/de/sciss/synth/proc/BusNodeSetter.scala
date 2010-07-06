package de.sciss.synth.proc

import de.sciss.synth.AudioBus

trait AudioBusNodeSetter {
   def add( implicit tx: ProcTxn )    : AudioBusNodeSetter
   def remove( implicit tx: ProcTxn ) : AudioBusNodeSetter
}
object AudioBusNodeSetter {
   def reader( name: String, rb: RichAudioBus, rn: RichNode ) : AudioBusNodeSetter = new ReaderImpl( name, rb, rn )
   def writer( name: String, rb: RichAudioBus, rn: RichNode ) : AudioBusNodeSetter = new WriterImpl( name, rb, rn )

   private abstract class Impl( name: String, rn: RichNode )
   extends RichAudioBus.User with AudioBusNodeSetter {
      def busChanged( b: AudioBus )( implicit tx: ProcTxn ) {
         rn.set( true, name -> b.index )
      }
   }

   private class ReaderImpl( _name: String, rb: RichAudioBus, _rn: RichNode ) extends Impl( _name, _rn ) {
      def add( implicit tx: ProcTxn )    = { rb.addReader(    this ); this }
      def remove( implicit tx: ProcTxn ) = { rb.removeReader( this ); this }
   }

   private class WriterImpl( _name: String, rb: RichAudioBus, _rn: RichNode ) extends Impl( _name, _rn ) {
      def add( implicit tx: ProcTxn )    = { rb.addWriter(    this ); this }
      def remove( implicit tx: ProcTxn ) = { rb.removeWriter( this ); this }
   }
}