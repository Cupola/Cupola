package de.sciss.synth.proc

/**
 *    @version 0.10, 03-Jul-10
 */
object Transition {
   private val currentRef = TxnLocal[ Transition ]( Instant )
   def current( implicit tx: ProcTxn ) : Transition = currentRef()
   def use[ T ]( trns: Transition )( thunk: => T )( implicit tx: ProcTxn ) : T = {
      val old = currentRef.swap( trns )
      try {
         thunk
      } finally {
         currentRef.swap( old )
      }
   }
}

sealed abstract class Transition {
   import Transition._

   def apply[ T ]( thunk: => T )( implicit tx: ProcTxn ) : T = {
      use( this )( thunk )
    }
}

case object Instant extends Transition
case class xfade( secs: Double ) extends Transition
case class glide( secs: Double )