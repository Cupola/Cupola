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

import edu.stanford.ppl.ccstm.{Txn, Ref => CRef}

/**
 *    @version 0.11, 24-Jun-10
 */
trait Ref[ @specialized T ] {
    def apply()( implicit tx: ProcTxn ) : T
    def set( v: T )( implicit tx: ProcTxn ) : Unit
    def swap( v: T )( implicit tx: ProcTxn ) : T
    def transform( f: T => T )( implicit tx: ProcTxn ) : Unit
    def transformIfDefined( pf: PartialFunction[ T, T ])( implicit tx: ProcTxn ) : Boolean

    def +=( rhs: T )( implicit tx: ProcTxn, num: Numeric[ T ]): Unit
}

object Ref {
   def make[ @specialized T ]( implicit m: ClassManifest[ T ]) : Ref[ T ] =
      new Impl( CRef.make[ T ])

   def apply[ @specialized T ]( initialValue: T )( implicit m: ClassManifest[ T ]) : Ref[ T ] =
      new Impl( CRef( initialValue ))

// SCALA BUG : CANNOT OVERLOAD
//   def apply[ @specialized T ]( initialValue: T, cleanUp: PartialFunction[ T, Unit ])
//                              ( implicit m: ClassManifest[ T ]) : Ref[ T ] =

   def withCheck[ @specialized T ]( initialValue: T )( cleanUp: PartialFunction[ T, Unit ])
                              ( implicit m: ClassManifest[ T ]) : Ref[ T ] =
      new CleanUpImpl( CRef( initialValue ), cleanUp )

   private class Impl[ T ]( c: CRef[ T ]) extends Ref[ T ] {
      def apply()( implicit tx: ProcTxn ) : T  = c.apply()( tx.ccstm )
      def set( v: T )( implicit tx: ProcTxn ) : Unit = c.set( v )( tx.ccstm )
      def swap( v: T )( implicit tx: ProcTxn ) : T = c.swap( v )( tx.ccstm )
      def transform( f: T => T )( implicit tx: ProcTxn ) : Unit = c.transform( f )( tx.ccstm )
      def transformIfDefined( pf: PartialFunction[ T, T ])( implicit tx: ProcTxn ) : Boolean =
         c.transformIfDefined( pf )( tx.ccstm )

      def +=( rhs: T )( implicit tx: ProcTxn, num: Numeric[ T ]): Unit = c.+=( rhs )( tx.ccstm, num )   

      override def toString = c.toString
   }

   private class CleanUpImpl[ T ]( _c: CRef[ T ], fun: PartialFunction[ T, Unit ])
   extends Impl( _c ) {
      private val touched = CRef( false )

      private def touch( tx: ProcTxn ) {
         implicit val t = tx.ccstm
         if( !touched.swap( true )) t.beforeCommit( _ => {
            touched.set( false )  // make sure it is reset for the next transaction
            val aqui = _c()
            if( fun.isDefinedAt( aqui ) ) {
                 t.addWriteResource( new Txn.WriteResource {
                     def prepare( t: Txn )         = true
                     def performCommit( t: Txn )   = fun( aqui )
                     def performRollback( t: Txn ) = ()
                 }, Int.MaxValue )
             }
         }, Int.MaxValue )
      }

      override def set( v: T )( implicit tx: ProcTxn ) : Unit = { touch( tx ); super.set( v )}
      override def swap( v: T )( implicit tx: ProcTxn ) : T = { touch( tx ); super.swap( v )}
      override def transform( f: T => T )( implicit tx: ProcTxn ) : Unit = { touch( tx ); super.transform( f )}
      override def transformIfDefined( pf: PartialFunction[ T, T ])( implicit tx: ProcTxn ) : Boolean = {
         touch( tx ); super.transformIfDefined( pf )
      }
   }
}
