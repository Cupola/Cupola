/*
 *  Proc.scala
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

import edu.stanford.ppl.ccstm.{ Ref => CRef, Txn }
import collection.immutable.{ Queue => IQueue }

/**
 *    A publisher model in the observer pattern, employing
 *    transactional semantics. The approach is that of an
 *    update-sheet which comprises of all the modifications
 *    happening during a transaction. Subscribed listeners
 *    receive this so-called partial update-sheet after the
 *    successful commit of the transaction. Listeners added
 *    during a transaction instead receive a so-called
 *    full update-sheet which means that for collection-
 *    based models all elements of the collection are
 *    published, and for parameter based models that the
 *    current state of each modeled parameter is included.
 *
 *    @version 0.10, 02-Jul-10 
 */
object TxnModel {
   trait Listener[ T ] {
      def updated( u: T )
   }
}

trait TxnModel[ T ] {
   import TxnModel._

   type L = Listener[ T ]

   protected val updateRef    = TxnLocal( emptyUpdate )
   private val touched        = TxnLocal( false )
   private val newListeners   = TxnLocal( IQueue.empty[ L ])
   private val listeners      = Ref( IQueue.empty[ L ])

   protected def emptyUpdate : T  // no way to get a ProcTxn ; hence leave away
   protected def fullUpdate( implicit tx: ProcTxn ) : T
   
   def addListener( l: L )( implicit tx: ProcTxn ) {
      touch
      newListeners.transform( _ enqueue l )
   }

   def removeListener( l: L )( implicit tx: ProcTxn ) {
      listeners.transform(    _.filterNot( _ == l ))
      newListeners.transform( _.filterNot( _ == l ))
   }

   protected def touch( implicit tx: ProcTxn ) {
      if( !touched.swap( true )) {
         tx.beforeCommit( tx0 => { // dispatch preparation
            val parList    = listeners()( tx0 )
            val parUpd : T = if( parList.nonEmpty ) updateRef()( tx0 ) else null.asInstanceOf[ T ]
            val fullList   = newListeners()( tx0 )
            val fullUpd    = if( fullList.nonEmpty ) fullUpdate( tx0 ) else null.asInstanceOf[ T ]
            if( fullList.nonEmpty ) listeners.transform( _ ++ fullList )( tx0 )
            if( parUpd != null || fullUpd != null ) tx0.afterCommit { tx1 =>
               parList.foreach(  _.updated( parUpd  ))
               fullList.foreach( _.updated( fullUpd ))
            }
         }, Int.MaxValue )
      }
   }
}