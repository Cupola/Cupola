/*
 *  Ref.scala
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

import edu.stanford.ppl.ccstm.{ Ref => CRef }

class Ref[ @specialized T ] private( c: CRef[ T ]) {
    def apply()( implicit tx: ProcTxn ) : T  = c.apply()( tx.ccstm )
    def set( v: T )( implicit tx: ProcTxn ) : Unit = c.set( v )( tx.ccstm )
    def swap( v: T )( implicit tx: ProcTxn ) : T = c.swap( v )( tx.ccstm )   
    def transform( f: T => T )( implicit tx: ProcTxn ) : Unit = c.transform( f )( tx.ccstm )
    def transformIfDefined( pf: PartialFunction[ T, T ])( implicit tx: ProcTxn ) : Boolean = c.transformIfDefined( pf )( tx.ccstm )
}

object Ref {
   def apply[ @specialized T ]( initialValue: T )( implicit m: ClassManifest[ T ]) : Ref[ T ] =
      new Ref( CRef( initialValue ))
}