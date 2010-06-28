/*
 *  Wolkenpumpe.scala
 *  (Wolkenpumpe)
 *
 *  Copyright (c) 2008-2010 Hanns Holger Rutz. All rights reserved.
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

package de.sciss.nuages

import de.sciss.synth.proc.{ Ref, ProcFactory, ProcTxn}
import de.sciss.synth.Model

object Wolkenpumpe extends Model {
   // --- dispatched from the model ----
   case class GensRemoved( pfs: ProcFactory* )
   case class GensAdded(   pfs: ProcFactory* )

   val gens = Ref.withObserver( Set.empty[ ProcFactory ]) { (oldSet, newSet) =>
      val removed = oldSet.diff( newSet )
      val added   = newSet.diff( oldSet )
      if( removed.nonEmpty ) dispatch( GensRemoved( removed.toSeq: _* ))
      if( added.nonEmpty )   dispatch( GensAdded(   added.toSeq:   _* ))
   }

   def add( pf: ProcFactory )( implicit tx: ProcTxn ) {
      gens.transform( _ + pf )
   }

   def remove( pf: ProcFactory )( implicit tx: ProcTxn ) {
      gens.transform( _ - pf )
   }
}