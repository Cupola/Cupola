package de.sciss.synth.proc

/**
 *    @version 0.10, 03-Jun-10
 */
trait ProcAudioOutput {
   def ~>  ( in: ProcAudioInput ) : ProcAudioInput
   def ~/> ( in: ProcAudioInput ) : ProcAudioOutput
   def ~|  ( insert: (ProcAudioInput, ProcAudioOutput) ) : ProcAudioInsertion
   def proc : Proc
}

trait ProcAudioInput {
   def proc : Proc
}

trait ProcAudioInsertion {
   def |> ( in: ProcAudioInput ) : ProcAudioInput
}