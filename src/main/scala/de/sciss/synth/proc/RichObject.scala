package de.sciss.synth.proc

object RichObject {
   sealed abstract class State
   case class Pending( syncID: Int ) extends State
   case object Online  extends State
}

trait RichObject { def state: RichObject.State }