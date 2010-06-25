/*
 *  BusManagement.scala
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

import de.sciss.synth.{Server, Bus}
import collection.immutable.{ SortedMap => ISortedMap, SortedSet => ISortedSet }

/**
 *    @version 0.10, 25-Jun-10
 */
trait RichBus {
   import RichBus._

   def server : Server
   def numChannels : Int
   def addReader( u: User )( implicit tx: ProcTxn ) : Unit
   def addWriter( u: User )( implicit tx: ProcTxn ) : Unit
   def removeReader( u: User )( implicit tx: ProcTxn ) : Unit
   def removeWriter( u: User )( implicit tx: ProcTxn ) : Unit
}

object RichBus {
   def alloc( server: Server, numChannels: Int ) : RichBus = Impl( server, numChannels )

   def soundIn( server: Server, numChannels: Int ) : RichBus = {
      val o = server.options
      require( numChannels <= o.inputBusChannels )
      HardwareImpl( server, o.outputBusChannels, numChannels )
   }

   def soundOut( server: Server, numChannels: Int ) : RichBus = {
      val o = server.options
      require( numChannels <= o.outputBusChannels )
      HardwareImpl( server, 0, numChannels )
   }

   trait User {
      def busChanged( index: Int, numChannels: Int )( implicit tx: ProcTxn ) : Unit
   }

   private class BusHolder( bus: Bus ) {
      private val useCount = Ref.withCheck( 0 ) { case 0 => bus.free }

      def alloc( implicit tx: ProcTxn ) { useCount += 1 }

      def free( implicit tx: ProcTxn ) {
         val cnt = useCount() - 1
         require( cnt >= 0 )
         useCount.set( cnt )
         if( cnt == 0 ) remove
      }

      def index : Int = bus.index

      protected def remove( implicit tx: ProcTxn ) {}
   }

//   private object BusHolderOrdering extends Ordering[ BusHolder ] {
//
//   }

   private type BusHolderMap = Map[ Server, ISortedMap[ Int, BusHolder ]]

   private class RichBusHolder( _bus: Bus, mapRef: Ref[ BusHolderMap ])
   extends BusHolder( _bus ) {
      def add( implicit tx: ProcTxn ) {
         mapRef.transform( map => map +
            (_bus.server -> (map.getOrElse( _bus.server, ISortedMap.empty[ Int, BusHolder ]) + (_bus.numChannels -> this))) )
      }

      override protected def remove( implicit tx: ProcTxn ) {
         mapRef.transform( map => {
            val newMap = map( _bus.server ) - _bus.numChannels
            if( newMap.isEmpty ) {
               map - _bus.server
            } else {
               map + (_bus.server -> newMap)
            }
         })
      }
   }

   private val readOnlyBuses  = Ref( Map.empty[ Server, ISortedMap[ Int, BusHolder ]])
   private val writeOnlyBuses = Ref( Map.empty[ Server, ISortedMap[ Int, BusHolder ]])

   private def allocReadOnlyBus( server: Server, numChannels: Int )( implicit tx: ProcTxn ) : BusHolder =
      allocRichBus( server, numChannels, readOnlyBuses )

   private def allocWriteOnlyBus( server: Server, numChannels: Int )( implicit tx: ProcTxn ) : BusHolder =
      allocRichBus( server, numChannels, writeOnlyBuses )

   private def allocRichBus( server: Server, numChannels: Int, mapRef: Ref[ BusHolderMap ])
                             ( implicit tx: ProcTxn ) : BusHolder = {
      val chanMapO = mapRef().get( server )
      val bus: BusHolder = chanMapO.flatMap( _.from( numChannels ).headOption.map( _._2 )).getOrElse({
         val res = new RichBusHolder( Bus.audio( server, numChannels ), mapRef )
         res.add
         res
      })
      bus.alloc
      bus
   }

   private def allocBus( server: Server, numChannels: Int )( implicit tx: ProcTxn ) : BusHolder = {
      val bus = new BusHolder( Bus.audio( server, numChannels ))
      bus.alloc
      bus
   }

   private case class HardwareImpl( server: Server, index: Int, numChannels: Int )
   extends RichBus {
      def addReader( u: User )( implicit tx: ProcTxn ) {
         u.busChanged( index, numChannels  )
      }

      def removeReader( u: User )( implicit tx: ProcTxn ) {}

      def addWriter( u: User )( implicit tx: ProcTxn ) {
         u.busChanged( index, numChannels )
      }

      def removeWriter( u: User )( implicit tx: ProcTxn ) {}
   }

   private case class Impl( server: Server, numChannels: Int ) extends RichBus {
//      private val bus: Ref[ Option[ Bus ]] = Ref( None )
//      // read count is stored in lower, write count in higher bits
//      // initRead | (initWrite << 16)
//      private val useCount = Ref( 0 ) { case 0 => bus.free }
      private val bus      = Ref.make[ BusHolder ]
      private val readers  = Ref( Set.empty[ User ])
      private val writers  = Ref( Set.empty[ User ])

      def addReader( u: User )( implicit tx: ProcTxn ) {
         val r       = readers()
         val newBus  = if( r.isEmpty ) {
            val w = writers()
            if( w.isEmpty ) { // no bus yet, create an empty shared one
               val res = allocReadOnlyBus( server, numChannels )
               bus.set( res )
               res
            } else { // dispose old dummy bus, create new bus
               val res     = allocBus( server, numChannels )
               val idx     = res.index
               r.foreach( _.busChanged( idx, numChannels ))
               w.foreach( _.busChanged( idx, numChannels ))
               val oldBus  = bus.swap( res )
               oldBus.free
               res
            }
         } else { // re-use existing bus
            val res = bus()
            res.alloc
            res
         }
         readers.transform( _ + u )
         // always perform this on the newly added
         // reader no matter if the bus is new:
         u.busChanged( newBus.index, numChannels )
      }

      def addWriter( u: User )( implicit tx: ProcTxn ) {
         val w       = writers()
         val newBus  = if( w.isEmpty ) {
            val r = readers()
            if( r.isEmpty ) { // no bus yet, create an empty shared one
               val res = allocWriteOnlyBus( server, numChannels )
               bus.set( res )
               res
            } else { // dispose old dummy bus, create new bus
               val res     = allocBus( server, numChannels )
               val idx     = res.index
               val oldBus  = bus.swap( res )
               r.foreach( _.busChanged( idx, numChannels ))
               w.foreach( _.busChanged( idx, numChannels ))
               oldBus.free
               res
            }
         } else { // re-use existing bus
            val res = bus()
            res.alloc
            res
         }
         writers.transform( _ + u )
         // always perform this on the newly added
         // reader no matter if the bus is new:
         u.busChanged( newBus.index, numChannels )
      }

      def removeReader( u: User )( implicit tx: ProcTxn ) {
         readers.transform( _ - u )
         val r       = readers()
         val oldBus  = bus()
         if( r.isEmpty ) {
            val w = writers()
            if( w.nonEmpty ) { // they can all go to write only
               val res  = allocWriteOnlyBus( server, numChannels )
               val idx  = res.index
               bus.set( res )
               w.foreach( _.busChanged( idx, numChannels ))
            }
         }
         oldBus.free
      }

      def removeWriter( u: User )( implicit tx: ProcTxn ) {
         writers.transform( _ - u )
         val w       = writers()
         val oldBus  = bus()
         if( w.isEmpty ) {
            val r = readers()
            if( r.nonEmpty ) { // they can all go to write only
               val res  = allocReadOnlyBus( server, numChannels )
               val idx  = res.index
               bus.set( res )
               r.foreach( _.busChanged( idx, numChannels ))
            }
         }
         oldBus.free
      }
   }

//   private class Impl( bus: Bus ) extends RichBus {
//      // read count is stored in lower, write count in higher bits
//      // initRead | (initWrite << 16)
//      private val useCount = Ref( 0 ) { case 0 => bus.free }
//      def index            = bus.index          // XXX maybe check if still valid
//      def numChannels      = bus.numChannels    // XXX maybe check if still valid
//
//      def removeReader( implicit tx: ProcTxn ) : RichBus = {
//         useCount.transform( cnt => {
//            val cnt        = useCount()
//            val readCnt    = (cnt & 0xFFFF) - 1
//            require( readCnt >= 0 )
//            readCnt | (cnt & 0xFFFF0000)
//         })
//      }
//
//      def addReader( implicit tx: ProcTxn ) : RichBus = {
//         val cnt = useCount()
//         require( cnt > 0 )
//         useCount += 1
//      }
//   }
}