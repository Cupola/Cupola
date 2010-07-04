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

import edu.stanford.ppl.ccstm.{ TxnLocal => CTxnLocal, Txn, Ref => CRef}

/**
 *    ScalaCollider-Proc's Ref wraps CCSTM's in order to isolate the rest of the
 *    code from the need to import ccstm packages. It also changes the method
 *    naming slightly, and most importantly adds refined Refs through the
 *    companion object's <code>withCheck</code> and <code>withObserver</code>
 *    methods.
 *
 *    @version 0.11, 27-Jun-10
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

   /**
    *    Creates a Ref whose value is checked before commit using the provided
    *    <code>PartialFunction</code>. If the function is defined at the particular
    *    value (the new value as changed during the transaction), the function
    *    is evaluated after the commit. Therefore clean-up behaviour such as
    *    freeing resources and other side-effect actions can take place.
    */
   def withCheck[ @specialized T ]( initialValue: T )( cleanUp: PartialFunction[ T, Unit ])
                              ( implicit m: ClassManifest[ T ]) : Ref[ T ] =
      new CleanUpImpl( CRef( initialValue ), cleanUp )

   /**
    *    Creates a Ref along with a <code>Function</code> which will be called after
    *    the commit if the Ref was touched during the transaction. The function is
    *    called in any case, even if as a result of the manipulations the actually
    *    stored value of the reference did not change. The function is called with
    *    the old value of the reference (as read before the first manipulation
    *    within the transaction) as first argument, and the new value (as read before
    *    commit) as second argument. The function may then broadcast a notification
    *    to observers of the object, informing them about the changes. A simple
    *    <code>==</code> comparision between old and new value may be used to
    *    determine if the value of the reference effectively changed during the
    *    transaction. 
    */
   def withObserver[ @specialized T ]( initialValue: T )( observer: Function2[ T, T, Unit ])
                              ( implicit m: ClassManifest[ T ]) : Ref[ T ] =
      new ObserverImpl( CRef( initialValue ), observer )

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

   private abstract class TouchImpl[ T ]( _c: CRef[ T ]) extends Impl[ T ]( _c ) {
      me: Impl[ T ] =>

      private val touchedRef = TxnLocal( false )

      private def touch( tx: ProcTxn ) {
         if( !touchedRef.swap( true )( tx )) {
            touched( tx )
         }
      }

      protected def touched( tx: ProcTxn ) : Unit

      override def set( v: T )( implicit tx: ProcTxn ) : Unit = { touch( tx ); super.set( v )}
      override def swap( v: T )( implicit tx: ProcTxn ) : T = { touch( tx ); super.swap( v )}
      override def transform( f: T => T )( implicit tx: ProcTxn ) : Unit = { touch( tx ); super.transform( f )}
      override def transformIfDefined( pf: PartialFunction[ T, T ])( implicit tx: ProcTxn ) : Boolean = {
         touch( tx ); super.transformIfDefined( pf )
      }
   }

   private class CleanUpImpl[ T ]( _c: CRef[ T ], fun: PartialFunction[ T, Unit ])
   extends TouchImpl( _c ) {
      protected def touched( tx: ProcTxn ) {
         val t = tx.ccstm
         t.beforeCommit( implicit t0 => {
            val newValue = _c()
            if( fun.isDefinedAt( newValue )) t0.afterCommit { _ => fun( newValue )}
         }, Int.MaxValue )
      }
   }

   private class ObserverImpl[ T ]( _c: CRef[ T ], fun: Function2[ T, T, Unit ])
   extends TouchImpl( _c ) {
      protected def touched( tx: ProcTxn ) {
         val t = tx.ccstm
         val oldValue = _c()( t )
         t.beforeCommit( implicit t0 => {
            val newValue = _c()
            t0.afterCommit { _ => fun( oldValue, newValue )}
         }, Int.MaxValue )
      }
   }
}

/**
 *    Note: specialization is commented out due to a bug in scala 2.8.0.RC6 (ticket #3636)
 */
trait TxnLocal[ /*@specialized*/ T ] {
   def apply()( implicit tx: ProcTxn ) : T
   def set( v: T )( implicit tx: ProcTxn ) : Unit
   def swap( v: T )( implicit tx: ProcTxn ) : T
   def transform( f: T => T )( implicit tx: ProcTxn ) : Unit
}

object TxnLocal {
   def apply[ /*@specialized*/ T ] : TxnLocal[ T ] = new Impl( new CTxnLocal[ T ])
   def apply[ /*@specialized*/ T ]( initValue: => T ) : TxnLocal[ T ] = new Impl( new CTxnLocal[ T ] {
      override def initialValue( tx: Txn ): T = initValue
   })

   private class Impl[ T ]( c: CTxnLocal[ T ]) extends TxnLocal[ T ] {
      def apply()( implicit tx: ProcTxn ) : T = c.get( tx.ccstm )
      def set( v: T )( implicit tx: ProcTxn ) : Unit = c.set( v )( tx.ccstm )
      def swap( v: T )( implicit tx: ProcTxn ) : T = {
         // currently not implemented in CTxnLocal
         val oldV = apply
         set( v )
         oldV
      }
      def transform( f: T => T )( implicit tx: ProcTxn ) {
         set( f( apply ))
      }
   }
}