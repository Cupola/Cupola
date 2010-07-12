/*
 *  ProcRunning.scala
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

import de.sciss.synth.{ AudioBus, Group, Model }
import collection.immutable.{ Seq => ISeq }

/**
 *    @version 0.11, 12-Jul-10
 */
//object ProcRunning {
//   case object Stopped
//}

trait ProcRunning /* extends Model*/ {
   def stop( implicit tx: ProcTxn ) : Unit
   def setFloat( name: String, value: Float )( implicit tx: ProcTxn ) : Unit
   def setString( name: String, value: String )( implicit tx: ProcTxn ) : Unit
//   def setAudioBus( name: String, value: RichBus )( implicit tx: ProcTxn ) : Unit

   def busChanged( name: String, bus: Option[ RichAudioBus ])( implicit tx: ProcTxn ) : Unit

   def setGroup( group: RichGroup )( implicit tx: ProcTxn ) : Unit

//   def controlAudioMapChanged( name: String, index: Int )( implicit tx: ProcTxn ) : Unit // XXX ugly

   def anchorNode( implicit tx: ProcTxn ) : RichNode

//   def accessories: ISeq[ TxnPlayer ]
}