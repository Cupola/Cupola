package de.sciss.synth.proc

object ControlValue {
   def instant( value: Double ) = ControlValue( value, value, Instant )
}

case class ControlValue( source: Double, target: Double, transit: Transition ) {
   def current( implicit txn: ProcTxn ) : Double = {
      val w = transit.position( txn.time )
      source * (w - 1) + target * w
   }

   def instant = ControlValue( target, target, Instant )

   /**
    *    Transaction-less coarse approximation
    *    of the current value.  Useful for a GUI
    *    which does not want to mess around with
    *    txn.
    */
   def currentApprox : Double = {
      val w = transit.positionApprox
      source * (w - 1) + target * w
   }
}