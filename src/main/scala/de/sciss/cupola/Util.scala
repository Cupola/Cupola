package de.sciss.cupola

import util.Random

object Util {
   private val rnd = new Random()

   def exprand( lo: Double, hi: Double ) : Double = {
      lo * math.exp( math.log( hi / lo ) * rnd.nextDouble )
   }

   def rrand( lo: Double, hi: Double ) : Double = {
      rnd.nextDouble() * (hi - lo) + lo
   }

   def rrand( lo: Int, hi: Int ) : Int = {
      if( lo <= hi ) {
         rnd.nextInt( hi - lo + 1 ) + lo
      } else {
         rnd.nextInt( lo - hi + 1 ) + hi
      }
   }

   def rand( d: Double ) : Double = rnd.nextDouble() * d

   def wchoose[ T ]( seq: Traversable[ T ])( fun: T => Double ) : T = {
      val i    = rnd.nextDouble
      var sum  = 0.0
      seq find { e => sum += fun( e ); sum >= i } getOrElse seq.last
   }
}