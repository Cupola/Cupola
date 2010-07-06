/*
 *  ProcGraph.scala
 *  (ScalaCollider-Proc)
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

package de.sciss.synth.proc

import de.sciss.synth.AddAction

/**
 *    @version 0.12, 01-Jul-10
 */
// XXX THIS SHOULD EXTEND TxnPlayer AND WE KILL OFF ProcRunning
trait ProcEntry {
//   def play( target: RichGroup )( implicit tx: ProcTxn ) : ProcRunning
   def play( implicit tx: ProcTxn ) : ProcRunning
}

trait ProcGraph extends ProcEntry {
}

object ProcGraphBuilder extends ThreadLocalObject[ ProcGraphBuilder ] {
}

trait ProcGraphBuilder {
   def tx: ProcTxn
   def includeParam( p: ProcParam ) : Unit
   def includeBuffer( b: ProcBuffer ) : Unit
}
