package de.sciss.synth.proc

import de.sciss.synth.SynthDef

case class RichSynthDef( synthDef: SynthDef, state: RichObject.State ) extends RichObject