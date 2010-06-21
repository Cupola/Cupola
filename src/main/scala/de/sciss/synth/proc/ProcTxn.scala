/*
 *  ProcTxn.scala
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

import de.sciss.synth._
import de.sciss.scalaosc.{ OSCBundle, OSCMessage, OSCPacket }
import actors.{ DaemonActor, Future, Futures, TIMEOUT }
import edu.stanford.ppl.ccstm.{ STM, Txn }
import collection.immutable.{ IntMap, Queue => IQueue, SortedMap => ISortedMap }
import collection.mutable.{ HashMap, Queue => MQueue, Set => MSet }
import collection.{ breakOut, SortedMap }
import osc._

/**
 *    @version 0.11, 21-Jun-10
 */
trait ProcTxn {
   import ProcTxn._

   def add( msg: OSCMessage with OSCSend, change: Option[ (FilterMode, RichState, Boolean) ], audible: Boolean,
            dependancies: Map[ RichState, Boolean ] = Map.empty ) : Unit

   private[ proc ] def ccstm : Txn
}

object ProcTxn {
   sealed abstract class FilterMode
   case object Always extends FilterMode
   case object IfChanges extends FilterMode
   case object RequiresChange extends FilterMode

   var verbose = false

   def atomic[ Z ]( block: ProcTxn => Z ) : Z = STM.atomic { implicit t =>
      val tx = new Impl
      t.addWriteResource( tx, Int.MaxValue )
      block( tx )
   }

   private class Impl( implicit txn: Txn )
   extends ProcTxn with Txn.WriteResource {
      tx =>

      private var serverData  = Map.empty[ Server, ServerData ]

      private var waitIDs  = Map.empty[ Server, Int ]

      private class ServerData( val server: Server ) {
         var firstMsgs        = IQueue.empty[ OSCMessage ]
         var secondMsgs       = IQueue.empty[ OSCMessage ]
         var firstAbortFuns   = IQueue.empty[ Function0[ Unit ]]
         var secondAbortMsgs  = IQueue.empty[ OSCMessage ]
         var waitID           = -1
         var secondSent       = false
      }

      private[ proc ] def ccstm : Txn = txn

      // ---- WriteResource implementation ----

      def prepare( t: Txn ) : Boolean = syn.synchronized { t.status.mightCommit && {
         if( verbose ) println( "TXN PREPARE" )
         val (clumps, maxSync) = establishDependancies
val server = Server.default // XXX vergación
         clumps.foreach( tup => {
            val (idx, msgs) = tup
            if( idx <= maxSync ) {
               val syncMsg    = server.syncMsg
               val syncID     = syncMsg.id
               val bndl       = OSCBundle( msgs.enqueue( syncMsg ): _* )  
               val fut        = server !! (bndl, { case OSCSyncedMessage( syncID ) => true })
               // XXX should use heuristic for timeouts
               Futures.awaitAll( 10000L, fut ) match {
                  case List( Some( true )) =>
                  case _ => error( "Timeout" )
               }
            } else {
               server ! OSCBundle( msgs: _* ) // XXX eventually audible could have a bundle time
//               true
            }
         })
         true
      }}

      def performRollback( t: Txn ) {
         if( verbose ) println( "TXN ROLLBACK" )
         val datas = serverData.values
         datas.foreach( data => {
            import data._
            if( secondSent && secondAbortMsgs.nonEmpty ) {
               server ! OSCBundle( secondAbortMsgs: _* )
            }
         })
         datas.foreach( data => {
            data.firstAbortFuns.foreach( fun => try { fun() } catch { case e => e.printStackTrace() })
         })
      }

      def performCommit( t: Txn ) {
         if( verbose ) println( "TXN COMMIT" )
         val datas = serverData.values
         datas.foreach( data => {
            import data._
            if( !secondSent && secondMsgs.nonEmpty ) {
               server ! OSCBundle( secondMsgs: _* )
            }
         })
      }

      def waitFor( server: Server, ids: Int* ) {
         val data = getServerData( server )
         data.waitID = math.max( data.waitID, ids.max )
      }

      private def getServerData( server: Server ) =
         serverData.getOrElse( server, {
            val data = new ServerData( server )
            serverData += server -> data
            data
         })

      private val syn      = new AnyRef
      private var entries  = IQueue.empty[ Entry ]
      private var entryMap = Map.empty[ (RichState, Boolean), Entry ]
      private var stateMap = Map.empty[ RichState, Boolean ]

      def add( msg: OSCMessage with OSCSend, change: Option[ (FilterMode, RichState, Boolean) ], audible: Boolean,
               dependancies: Map[ RichState, Boolean ]) : Unit = syn.synchronized {

         if( verbose ) println( "TXN ADD : " + (msg, change, audible, dependancies) )

         def processDeps {
            dependancies.foreach( tup => {
               val (state, value) = tup
               if( !stateMap.contains( state )) {
                  stateMap += state -> state.get( tx )
               }
            })
            val entry = Entry( msg, change, audible, dependancies )
            entries = entries.enqueue( entry )
            change.foreach( tup => {
               val (_, state, value) = tup
               entryMap += (state, value) -> entry
            })
         }

         change.map( tup => {
            val (mode, state, value) = tup
            val changed = state.get( tx ) != value
            require( changed || (mode != RequiresChange) )
            if( changed || (mode == Always) ) {
               // it is important that processDeps is
               // executed before state.set as the object
               // might depend on a current state of its own
               processDeps
               if( changed ) state.set( value )( tx )
            }
         }).getOrElse( processDeps )
      }

      private def establishDependancies : (IntMap[ IQueue[ OSCMessage ]], Int) = {
         var topo = Topology.empty[ Entry, EntryEdge ]

         var clumpEdges = Map.empty[ Entry, Set[ Entry ]]

         entries.foreach( targetEntry => {
            topo = topo.addVertex( targetEntry )
            targetEntry.dependancies.foreach( dep => {
               entryMap.get( dep ).map( sourceEntry => {
                  val edge = EntryEdge( sourceEntry, targetEntry )
                  topo.addEdge( edge ) match {
                     case Some( (newTopo, _, _) ) => {
                        topo = newTopo
                        // clumping occurs when a synchronous message depends on
                        // an asynchronous message
                        if( !sourceEntry.msg.isSynchronous && targetEntry.msg.isSynchronous ) {
                           clumpEdges += targetEntry -> (clumpEdges.getOrElse( targetEntry, Set.empty ) + sourceEntry)
                        }
                     }
                     case None => {
                        error( "Unsatisfied dependancy " + edge )
                     }
                  }
               }).getOrElse({
                  val (state, value) = dep
                  if( stateMap.get( state ) != Some( value )) {
                     error( "Unsatisfied dependancy " + dep )
                  }
               })
            })
         })

         // clumping
         var clumpIdx   = 0
         var clumpMap   = Map.empty[ Entry, Int ]
         var clumps     = IntMap.empty[ IQueue[ OSCMessage ]]
         val audibleIdx = Int.MaxValue
         topo.vertices.foreach( targetEntry => {
            if( targetEntry.audible ) {
               clumps += audibleIdx -> (clumps.getOrElse( audibleIdx, IQueue.empty ) enqueue targetEntry.msg)
               clumpMap += targetEntry -> audibleIdx
            } else {
               val depIdx = clumpEdges.get( targetEntry ).map( set => {
                  set.map( clumpMap.getOrElse( _, error( "Unsatisfied dependancy " + targetEntry ))).max
               }).getOrElse( -1 )
               if( depIdx > clumpIdx ) error( "Unsatisfied dependancy " + targetEntry )
               if( depIdx == clumpIdx ) clumpIdx += 1
               clumps += clumpIdx -> (clumps.getOrElse( clumpIdx, IQueue.empty ) enqueue targetEntry.msg)
               clumpMap += targetEntry -> clumpIdx
            }
         })

         if( verbose ) clumps.foreach( tup => {
            val (idx, msgs) = tup
            println( "clump #" + idx + " : " + msgs.toList )
         })

         (clumps, if( clumps.contains( audibleIdx )) clumpIdx else clumpIdx - 1)
      }

      private case class Entry( msg: OSCMessage with OSCSend, change: Option[ (FilterMode, RichState, Boolean) ],
                                audible: Boolean, dependancies: Map[ RichState, Boolean ])

      private case class EntryEdge( sourceVertex: Entry, targetVertex: Entry ) extends Topology.Edge[ Entry ] 
   }
}