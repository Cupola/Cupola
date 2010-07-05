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

   def position( time: Double ) = {
//      val res =
      if( dur > 0.0 ) math.max( 0.0, math.min( 1.0, (time - start) / dur )) else 1.0
//      println( "time = " + time + "; start = " + start + "; off = " + (time-start) + "; dur = " + dur + "; res = " + res )
//      res
   }

   def positionApprox = {
      if( dur > 0.0 ) math.max( 0.0, math.min( 1.0, (System.currentTimeMillis - sys) * 0.001 / dur )) else 1.0
   }
}

case class XFade( start: Double, dur: Double ) extends DurationalTransition {
   private var markSet = TxnLocal( Set.empty[ AnyRef ])

   def markSendToBack( obj: AnyRef )( implicit tx: ProcTxn ) : Boolean = {
      val s       = markSet()
      val isNew   = !s.contains( obj )
      if( isNew ) {
         markSet.set( s + obj )
      }
      isNew
   }
}

case class Glide( start: Double, dur: Double ) extends DurationalTransition
