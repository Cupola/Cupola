package de.sciss.synth.proc

import de.sciss.synth.Model

object Stoppable {
   case object Stopped
}

trait Stoppable extends Model {
   def stop : Unit
}