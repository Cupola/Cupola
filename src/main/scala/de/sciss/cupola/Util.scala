/*
 *  Util.scala
 *  (Cupola)
 *
 *  Copyright (c) 2010 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 *
 *
 *  Changelog:
 */

package de.sciss.cupola

import util.Random

/**
 * @version 0.11, 17-Aug-10
 */
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

   def choose[ T ]( seq: Traversable[ T ]) : T = {
      val idxSeq = seq.toIndexedSeq
      idxSeq( rnd.nextInt( idxSeq.size ))
   }

   def wchoose[ T ]( seq: Traversable[ T ])( fun: T => Double ) : T = {
      val i    = rnd.nextDouble
      var sum  = 0.0
      seq find { e => sum += fun( e ); sum >= i } getOrElse seq.last
   }
}