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

import collection.immutable.{ SortedMap => ISortedMap, SortedSet => ISortedSet }
import de.sciss.synth._

/**
 *    @version 0.10, 01-Jul-10
 */
sealed trait RichBus {
   import RichBus._

   def server : Server
   def numChannels : Int
//   def addReader( u: User )( implicit tx: ProcTxn ) : Unit
//   def addWriter( u: User )( implicit tx: ProcTxn ) : Unit
//   def removeReader( u: User )( implicit tx: ProcTxn ) : Unit
//   def removeWriter( u: User )( implicit tx: ProcTxn ) : Unit

   def busOption( implicit tx: ProcTxn ): Option[ Bus ]
   def rate : Rate
}

object RichAudioBus {
   trait User {
      def busChanged( bus: AudioBus )( implicit tx: ProcTxn ) : Unit
   }
}

trait RichAudioBus extends RichBus with AudioRated {
   import RichAudioBus._

   def busOption( implicit tx: ProcTxn ): Option[ AudioBus ]

   def addReader( u: User )( implicit tx: ProcTxn ) : Unit
   def addWriter( u: User )( implicit tx: ProcTxn ) : Unit
   def removeReader( u: User )( implicit tx: ProcTxn ) : Unit
   def removeWriter( u: User )( implicit tx: ProcTxn ) : Unit
}

object RichControlBus {
   trait User {
      def busChanged( bus: ControlBus )( implicit tx: ProcTxn ) : Unit
   }
}

trait RichControlBus extends RichBus with ControlRated {
   import RichControlBus._

   def busOption( implicit tx: ProcTxn ): Option[ ControlBus ]

   def addReader( u: User )( implicit tx: ProcTxn ) : Unit
   def addWriter( u: User )( implicit tx: ProcTxn ) : Unit
   def removeReader( u: User )( implicit tx: ProcTxn ) : Unit
   def removeWriter( u: User )( implicit tx: ProcTxn ) : Unit
}

object RichBus {
   def audio( server: Server, numChannels: Int ) : RichAudioBus = AudioImpl( server, numChannels )
   def control( server: Server, numChannels: Int ) : RichControlBus = ControlImpl( server, numChannels )

   def soundIn( server: Server, numChannels: Int ) : RichAudioBus = {
      val o = server.options
      require( numChannels <= o.inputBusChannels )
      HardwareImpl( new AudioBus( server, o.outputBusChannels, numChannels ))
   }

   def soundOut( server: Server, numChannels: Int ) : RichAudioBus = {
      val o = server.options
      require( numChannels <= o.outputBusChannels )
      HardwareImpl( new AudioBus( server, 0, numChannels ))
   }

//   trait User {
//      def busChanged( bus: AudioBus )( implicit tx: ProcTxn ) : Unit
//   }

   private class BusHolder[ T <: Bus ]( val bus: T ) {
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

   private type AudioBusHolder   = BusHolder[ AudioBus ]
   private type ControlBusHolder = BusHolder[ ControlBus ]

//   private object BusHolderOrdering extends Ordering[ BusHolder ] {
//
//   }

   private type ABusHolderMap = Map[ Server, ISortedMap[ Int, AudioBusHolder ]]
//   private type KBusHolderMap = Map[ Server, ISortedMap[ Int, ControlBusHolder ]]

   private class RichAudioBusHolder( _bus: AudioBus, mapRef: Ref[ ABusHolderMap ])
   extends AudioBusHolder( _bus ) {
      def add( implicit tx: ProcTxn ) {
         mapRef.transform( map => map +
            (_bus.server -> (map.getOrElse( _bus.server, ISortedMap.empty[ Int, AudioBusHolder ]) + (_bus.numChannels -> this))) )
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

   private val readOnlyBuses  = Ref( Map.empty[ Server, ISortedMap[ Int, AudioBusHolder ]])
   private val writeOnlyBuses = Ref( Map.empty[ Server, ISortedMap[ Int, AudioBusHolder ]])

   private def allocReadOnlyBus( server: Server, numChannels: Int )( implicit tx: ProcTxn ) : AudioBusHolder =
      allocRichAudioBus( server, numChannels, readOnlyBuses )

   private def allocWriteOnlyBus( server: Server, numChannels: Int )( implicit tx: ProcTxn ) : AudioBusHolder =
      allocRichAudioBus( server, numChannels, writeOnlyBuses )

   private def allocRichAudioBus( server: Server, numChannels: Int, mapRef: Ref[ Map[ Server, ISortedMap[ Int, AudioBusHolder ]]])
                             ( implicit tx: ProcTxn ) : AudioBusHolder = {
      val chanMapO = mapRef().get( server )
      val bus: AudioBusHolder = chanMapO.flatMap( _.from( numChannels ).headOption.map( _._2 )).getOrElse({
         val res = new RichAudioBusHolder( Bus.audio( server, numChannels ), mapRef )
         res.add
         res
      })
      bus.alloc
      bus
   }

   private def allocAudioBus( server: Server, numChannels: Int )( implicit tx: ProcTxn ) : AudioBusHolder = {
      val bus = new BusHolder( Bus.audio( server, numChannels ))
      bus.alloc
      bus
   }

   private def allocControlBus( server: Server, numChannels: Int )( implicit tx: ProcTxn ) : ControlBusHolder = {
      val bus = new BusHolder( Bus.control( server, numChannels ))
      bus.alloc
      bus
   }

   private case class HardwareImpl( bus: AudioBus )
   extends RichAudioBus {
      import RichAudioBus._

      def server        = bus.server
      def numChannels   = bus.numChannels

      def busOption( implicit tx: ProcTxn ) = Some( bus )

      def addReader( u: User )( implicit tx: ProcTxn ) {
         u.busChanged( bus  )
      }

      def removeReader( u: User )( implicit tx: ProcTxn ) {}

      def addWriter( u: User )( implicit tx: ProcTxn ) {
         u.busChanged( bus )
      }

      def removeWriter( u: User )( implicit tx: ProcTxn ) {}
   }

   private case class AudioImpl( server: Server, numChannels: Int ) extends RichAudioBus {
      import RichAudioBus._

      private val bus      = Ref.make[ AudioBusHolder ]
      private val readers  = Ref( Set.empty[ User ])
      private val writers  = Ref( Set.empty[ User ])

      def busOption( implicit tx: ProcTxn ) = {
         val bh = bus()
         if( bh != null ) Some( bh.bus ) else None
      }

      def addReader( u: User )( implicit tx: ProcTxn ) {
         val r       = readers()
         val newBus  = if( r.isEmpty ) {
            val w = writers()
            if( w.isEmpty ) { // no bus yet, create an empty shared one
               val bh = allocReadOnlyBus( server, numChannels )
               bus.set( bh )
               new AudioBus( server, bh.index, numChannels )
            } else { // dispose old dummy bus, create new bus
               val bh     = allocAudioBus( server, numChannels )
//               val idx     = bh.index
//               r.foreach( _.busChanged( idx, numChannels ))
//               w.foreach( _.busChanged( idx, numChannels ))
               val res     = new AudioBus( server, bh.index, numChannels )
               r.foreach( _.busChanged( res ))
               w.foreach( _.busChanged( res ))
               val oldBus  = bus.swap( bh )
               oldBus.free
               res
            }
         } else { // re-use existing bus
            val bh = bus()
            bh.alloc
            new AudioBus( server, bh.index, numChannels )
         }
         readers.transform( _ + u )
         // always perform this on the newly added
         // reader no matter if the bus is new:
         u.busChanged( newBus )
      }

      def addWriter( u: User )( implicit tx: ProcTxn ) {
         val w       = writers()
         val newBus  = if( w.isEmpty ) {
            val r = readers()
            if( r.isEmpty ) { // no bus yet, create an empty shared one
               val bh = allocWriteOnlyBus( server, numChannels )
               bus.set( bh )
               new AudioBus( server, bh.index, numChannels )
            } else { // dispose old dummy bus, create new bus
               val bh      = allocAudioBus( server, numChannels )
//               val idx     = bh.index
               val oldBus  = bus.swap( bh )
//               r.foreach( _.busChanged( idx, numChannels ))
               val res = new AudioBus( server, bh.index, numChannels )
//               w.foreach( _.busChanged( idx, numChannels ))
               r.foreach( _.busChanged( res ))
               w.foreach( _.busChanged( res ))
               oldBus.free
               res
            }
         } else { // re-use existing bus
            val bh = bus()
            bh.alloc
            new AudioBus( server, bh.index, numChannels )
         }
         writers.transform( _ + u )
         // always perform this on the newly added
         // reader no matter if the bus is new:
         u.busChanged( newBus )
      }

      def removeReader( u: User )( implicit tx: ProcTxn ) {
         readers.transform( _ - u )
         val r       = readers()
         val oldBus  = bus()
         if( r.isEmpty ) {
            val w = writers()
            if( w.nonEmpty ) { // they can all go to write only
               val bh = allocWriteOnlyBus( server, numChannels )
//               val idx  = bh.index
               bus.set( bh )
               val res = new AudioBus( server, bh.index, numChannels )
//               w.foreach( _.busChanged( idx, numChannels ))
               w.foreach( _.busChanged( res ))
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
               val bh = allocReadOnlyBus( server, numChannels )
//               val idx  = bh.index
               bus.set( bh )
               val res = new AudioBus( server, bh.index, numChannels )
               r.foreach( _.busChanged( res ))
            }
         }
         oldBus.free
      }
   }

   private case class ControlImpl( server: Server, numChannels: Int ) extends RichControlBus {
      import RichControlBus._

      private val bus    = Ref.make[ ControlBusHolder ]
      private val users  = Ref( Set.empty[ User ])

      def busOption( implicit tx: ProcTxn ) = {
         val bh = bus()
         if( bh != null ) Some( bh.bus ) else None
      }

      def addReader( u: User )( implicit tx: ProcTxn ) { add( u )}
      def addWriter( u: User )( implicit tx: ProcTxn ) { add( u )}

      private def add( u: User )( implicit tx: ProcTxn ) {
         val g = users()
         val bh  = if( g.isEmpty ) {
            val res = allocControlBus( server, numChannels )
            bus.set( res )
            res
         } else { // re-use existing bus
            val res = bus()
            res.alloc
            res
         }
         val newBus = new ControlBus( server, bh.index, numChannels )
         users.transform( _ + u )
         // always perform this on the newly added
         // reader no matter if the bus is new:
         u.busChanged( newBus )
      }
      
      def removeReader( u: User )( implicit tx: ProcTxn ) { remove( u )}
      def removeWriter( u: User )( implicit tx: ProcTxn ) { remove( u )}

      private def remove( u: User )( implicit tx: ProcTxn ) {
         users.transform( _ - u )
         bus().free
      }
   }
}