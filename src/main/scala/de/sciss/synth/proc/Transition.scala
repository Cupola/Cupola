package de.sciss.synth.proc

/**
 *    @version 0.10, 03-Jul-10
 */
//object Transition {
//   private val currentRef = TxnLocal[ Transition ]( Instant )
//   def current( implicit tx: ProcTxn ) : Transition = currentRef()
//   def use[ T ]( trns: Transition )( thunk: => T )( implicit tx: ProcTxn ) : T = {
//      val old = currentRef.swap( trns )
//      try {
//         thunk
//      } finally {
//         currentRef.swap( old )
//      }
//   }
//}

sealed abstract class Transition {
//   import Transition._
//
//   def apply[ T ]( thunk: => T )( implicit tx: ProcTxn ) : T = {
//      use( this )( thunk )
//    }

   def position( time: Double ) : Double
   def positionApprox : Double
}

case object Instant extends Transition {
   def position( time: Double ) = 1.0
   def positionApprox = 1.0
}

sealed abstract class DurationalTransition extends Transition {
   private val sys = System.currentTimeMillis

   def start: Double
   def dur: Double

   def position( time: Double ) = if( dur > 0.0 ) math.max( 0.0, math.min( 1.0, (time - start) / dur )) else 1.0
   def positionApprox = position( (System.currentTimeMillis - sys) * 0.001 / dur )
}

case class XFade( start: Double, dur: Double ) extends DurationalTransition
case class Glide( start: Double, dur: Double ) extends DurationalTransition
