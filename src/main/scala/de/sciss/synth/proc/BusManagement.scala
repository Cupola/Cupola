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
 *    @version 0.11, 05-Jul-10
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
   /**
    *    A consumer reading or writing from an audio bus.
    *    Since a RichAudioBus is a meta structure, the
    *    underlying audio bus may change due to optimization.
    *    In this case the consumer is asked to update its
    *    data. Also initial bus allocation is lazy, therefore
    *    when adding the user as reader or writer, the
    *    bus implementation will push its initial allocation
    *    information to the user.    
    */
   trait User {
      def busChanged( bus: AudioBus )( implicit tx: ProcTxn ) : Unit
   }
}

trait RichAudioBus extends RichBus with AudioRated {
   import RichAudioBus._

   def busOption( implicit tx: ProcTxn ): Option[ AudioBus ]

   /**
    *    Adds a reading consumer to the bus. Note that
    *    the readers are kept in a Set and this method doesn't
    *    currently check whether the set already contains
    *    the reader. Adding the same reader more than once
    *    will cause malbehaviour.
    *
    *    As a consequence, the user's busChanged method is
    *    invoked with the current bus. The current bus may
    *    change due to the addition. In this case, busChanged
    *    is called on all other currently registered users.
    */
   def addReader( u: User )( implicit tx: ProcTxn ) : Unit

   /**
    *    Adds a writing consumer to the bus. Note that
    *    the writers are kept in a Set and this method doesn't
    *    currently check whether the set already contains
    *    the writer. Adding the same writer more than once
    *    will cause malbehaviour.
    *
    *    As a consequence, the user's busChanged method is
    *    invoked with the current bus. The current bus may
    *    change due to the addition. In this case, busChanged
    *    is called on all other currently registered users.
    */
   def addWriter( u: User )( implicit tx: ProcTxn ) : Unit

   /**
    *    Removes a reading consumer from the bus. It is
    *    safe to call this method, passing in a user which
    *    has already been previously removed.
    *
    *    The current bus may change due to the removal.
    *    In this case, busChanged is called on all
    *    remaining registered users.
    */
   def removeReader( u: User )( implicit tx: ProcTxn ) : Unit

   /**
    *    Removes a writing consumer from the bus. It is
    *    safe to call this method, passing in a user which
    *    has already been previously removed.
    *
    *    The current bus may change due to the removal.
    *    In this case, busChanged is called on all
    *    remaining registered users.
    */
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

   /**
    *    Adds a reading consumer to the bus. Note that
    *    the readers are kept in a Set and this method doesn't
    *    currently check whether the set already contains
    *    the reader. Adding the same reader more than once
    *    will cause malbehaviour.
    *
    *    As a consequence, the user's busChanged method is
    *    invoked with the current bus.
    */
   def addReader( u: User )( implicit tx: ProcTxn ) : Unit

   /**
    *    Adds a writing consumer to the bus. Note that
    *    the writers are kept in a Set and this method doesn't
    *    currently check whether the set already contains
    *    the writer. Adding the same writer more than once
    *    will cause malbehaviour.
    *
    *    As a consequence, the user's busChanged method is
    *    invoked with the current bus.
    */
   def addWriter( u: User )( implicit tx: ProcTxn ) : Unit

   /**
    *    Removes a reading consumer from the bus. It is
    *    safe to call this method, passing in a user which
    *    has already been previously removed.
    */
   def removeReader( u: User )( implicit tx: ProcTxn ) : Unit

   /**
    *    Removes a writing consumer from the bus. It is
    *    safe to call this method, passing in a user which
    *    has already been previously removed.
    */
   def removeWriter( u: User )( implicit tx: ProcTxn ) : Unit
}

object RichBus {
   /**
    *    Constructs a new audio bus proxy for use in a shared environment, where
    *    there can be situations of semi-orphaned buses (only one reader or
    *    only one writer left).
    */
   def audio( server: Server, numChannels: Int ) : RichAudioBus = AudioImpl( server, numChannels )
   def control( server: Server, numChannels: Int ) : RichControlBus = ControlImpl( server, numChannels )
   /**
    *    Constructs a new audio bus proxy for use in a short-term temporary fashion.
    *    The implementation does not maintain dummy and empty buses for the case that
    *    there is only one reader or only one writer. As a consequence, it should not
    *    be used in such a scenario, as precious bus indices will be occupied. On the
    *    other hand, this method is useful for internal temporary buses, because when
    *    both a reader and a writer release the resource, there are no spurious
    *    bus re-assignments causing further busChanged notifications (which would go
    *    to concurrently freed nodes).
    */
   def tmpAudio( server: Server, numChannels: Int ) : RichAudioBus = TempAudioImpl( server, numChannels )

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

   private abstract class AbstractAudioImpl extends RichAudioBus {
      protected val bus = Ref.make[ AudioBusHolder ]

      def busOption( implicit tx: ProcTxn ) = {
         val bh = bus()
         if( bh != null ) Some( bh.bus ) else None
      }
   }

   private case class AudioImpl( server: Server, numChannels: Int ) extends AbstractAudioImpl {
      import RichAudioBus._

      private val readers  = Ref( Set.empty[ User ])
      private val writers  = Ref( Set.empty[ User ])

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
         val r0   = readers()
         if( !r0.contains( u )) return
         val r    = r0 - u
         readers.set( r )
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
         val w0      = writers()
         if( !w0.contains( u )) return
         val w       = w0 - u
         writers.set( w )
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

   private case class TempAudioImpl( server: Server, numChannels: Int ) extends AbstractAudioImpl {
      import RichAudioBus._

      private val users = Ref( Set.empty[ User ])

      def addReader( u: User )( implicit tx: ProcTxn ) { add( u )}
      def addWriter( u: User )( implicit tx: ProcTxn ) { add( u )}

      private def add( u: User )( implicit tx: ProcTxn ) {
         val g = users()
         val bh  = if( g.isEmpty ) {
            val res = allocAudioBus( server, numChannels )
            bus.set( res )
            res
         } else { // re-use existing bus
            val res = bus()
            res.alloc
            res
         }
         val newBus = new AudioBus( server, bh.index, numChannels )
         users.transform( _ + u )
         // always perform this on the newly added
         // reader no matter if the bus is new:
         u.busChanged( newBus )
      }

      def removeReader( u: User )( implicit tx: ProcTxn ) { remove( u )}
      def removeWriter( u: User )( implicit tx: ProcTxn ) { remove( u )}

      private def remove( u: User )( implicit tx: ProcTxn ) {
         val rw = users()
         if( !rw.contains( u )) return
         users.set( rw - u )
         bus().free
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
         val rw = users()
         if( !rw.contains( u )) return
         users.set( rw - u )
         bus().free
      }
   }
}