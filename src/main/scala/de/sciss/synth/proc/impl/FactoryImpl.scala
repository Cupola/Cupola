/*
 *  FactoryImpl.scala
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

package de.sciss.synth.proc.impl

import de.sciss.synth.proc._
import de.sciss.synth.Server
import collection.immutable.{ IndexedSeq => IIdxSeq }

class FactoryImpl( val name: String, val entry: ProcEntry,
                           val paramMap: Map[ String, ProcParam ],
                           val paramSeq: IIdxSeq[ ProcParam ],
                           val pAudioIns: IIdxSeq[ ProcParamAudioInput ],
                           val pAudioOuts: IIdxSeq[ ProcParamAudioOutput ])
extends ProcFactory {
   def make( implicit tx: ProcTxn ) : Proc = {
      val res = new Impl( this, Server.default, name )
      ProcDemiurg.addVertex( res )
      res
   }

   override def toString = "gen(" + name + ")"
}
