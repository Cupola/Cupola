package de.sciss.synth.proc

trait TxnPlayer {
   def play( implicit tx: ProcTxn ) : Unit
   def stop( implicit tx: ProcTxn ) : Unit
   def isPlaying( implicit tx: ProcTxn ) : Boolean
}