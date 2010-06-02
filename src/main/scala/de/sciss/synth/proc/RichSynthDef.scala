package de.sciss.synth.proc

import de.sciss.synth.SynthDef

object RichSynthDef {
   sealed abstract class State
   case class Pending( syncID: Int ) extends State
   case object Online  extends State
}
case class RichSynthDef( synthDef: SynthDef, state: RichSynthDef.State )