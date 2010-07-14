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
 *    @version 0.12, 10-Jul-10
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
//      def migrateTo( newBus: RichAudioBus )( implicit tx: ProcTxn ) : Unit
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

//   def migrateTo( anotherBus: RichAudioBus )( implicit tx: ProcTxn ) : Unit
//
//   protected def add( readers: Iterable[ User ], writers: Iterable[ User ])( implicit tx: ProcTxn ) : Unit

//   def becomeSoundIn : Unit
//   def becomeSoundOut : Unit

//   def migrateTo( newBus: RichAudioBus )( implicit tx: ProcTxn ) : Unit
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
   def audio( server: Server, numChannels: Int ) : RichAudioBus = new AudioImpl( server, numChannels )
   def control( server: Server, numChannels: Int ) : RichControlBus = new ControlImpl( server, numChannels )
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
   def tmpAudio( server: Server, numChannels: Int ) : RichAudioBus = new TempAudioImpl( server, numChannels )

   def soundIn( server: Server, numChannels: Int, offset: Int = 0 ) : RichAudioBus = {
      val o = server.options
      require( offset +  numChannels <= o.inputBusChannels )
      HardwareImpl( new AudioBus( server, o.outputBusChannels, numChannels + offset ))
   }

   def soundOut( server: Server, numChannels: Int, offset: Int = 0 ) : RichAudioBus = {
      val o = server.options
      require( offset + numChannels <= o.outputBusChannels )
      HardwareImpl( new AudioBus( server, offset, numChannels ))
   }

//   trait User {
//      def busChanged( bus: AudioBus )( implicit tx: ProcTxn ) : Unit
//   }

   private var verbose = true

   private class BusHolder[ T <: Bus ]( val bus: T ) {
      private val useCount = Ref.withCheck( 0 ) { case 0 => bus.free }

      def alloc( implicit tx: ProcTxn ) {
         useCount += 1
         if( verbose ) println( bus.toString + ".alloc -> " + useCount() )
      }

      def free( implicit tx: ProcTxn ) {
         val cnt = useCount() - 1
         if( verbose ) println( bus.toString + ".free -> " + cnt )
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

   private def createReadOnlyBus( server: Server, numChannels: Int )( implicit tx: ProcTxn ) : AudioBusHolder =
      createRichAudioBus( server, numChannels, readOnlyBuses )

   private def createWriteOnlyBus( server: Server, numChannels: Int )( implicit tx: ProcTxn ) : AudioBusHolder =
      createRichAudioBus( server, numChannels, writeOnlyBuses )

   private def createRichAudioBus( server: Server, numChannels: Int, mapRef: Ref[ Map[ Server, ISortedMap[ Int, AudioBusHolder ]]])
                             ( implicit tx: ProcTxn ) : AudioBusHolder = {
      val chanMapO = mapRef().get( server )
      val bus: AudioBusHolder = chanMapO.flatMap( _.from( numChannels ).headOption.map( _._2 )).getOrElse({
         val res = new RichAudioBusHolder( Bus.audio( server, numChannels ), mapRef )
         res.add
         res
      })
//      bus.alloc
      bus
   }

   private def createAudioBus( server: Server, numChannels: Int )( implicit tx: ProcTxn ) : AudioBusHolder = {
      val bus = new BusHolder( Bus.audio( server, numChannels ))
//      bus.alloc
      bus
   }

   private def createControlBus( server: Server, numChannels: Int )( implicit tx: ProcTxn ) : ControlBusHolder = {
      val bus = new BusHolder( Bus.control( server, numChannels ))
//      bus.alloc
      bus
   }

   private abstract class AbstractAudioImpl extends RichAudioBus {
      import RichAudioBus._

      protected val readers   = Ref( Set.empty[ User ])
      protected val writers   = Ref( Set.empty[ User ])
   }

   private case class HardwareImpl( bus: AudioBus )
   extends AbstractAudioImpl {
      import RichAudioBus._

      def server        = bus.server
      def numChannels   = bus.numChannels

      def busOption( implicit tx: ProcTxn ) = Some( bus )

      def addReader( u: User )( implicit tx: ProcTxn ) { add( readers, u )}
      def addWriter( u: User )( implicit tx: ProcTxn ) { add( writers, u )}

      private def add( users: Ref[ Set[ User ]], u: User )( implicit tx: ProcTxn ) {
         users.transform( _ + u )
         u.busChanged( bus  )
      }

      def removeReader( u: User )( implicit tx: ProcTxn ) { remove( readers, u )}
      def removeWriter( u: User )( implicit tx: ProcTxn ) { remove( writers, u )}

      private def remove( users: Ref[ Set[ User ]], u: User )( implicit tx: ProcTxn ) {
         users.transform( _ - u )
      }

//      def migrateTo( anotherBus: RichAudioBus )( implicit tx: ProcTxn ) {
//         val r = readers()
//         val w = writers()
//         readers.set( Set.empty )
//         writers.set( Set.empty )
//         anotherBus.add( r, w )
//      }
//
//      protected def add( r: Iterable[ User ], w: Iterable[ User ])( implicit tx: ProcTxn ) {
//         readers.transform( _ ++ r )
//         writers.transform( _ ++ w )
//         r.foreach( _.busChanged( bus  ))
//         w.foreach( _.busChanged( bus  ))
//      }

      override def toString = "h-abus(" + bus + ")"
   }

   private abstract class BasicAudioImpl extends AbstractAudioImpl {
      import RichAudioBus._
      
      protected val bus       = Ref.make[ AudioBusHolder ]

      def busOption( implicit tx: ProcTxn ) = {
         val bh = bus()
         if( bh != null ) Some( bh.bus ) else None
      }

//      // XXX eventually AudioImpl could have an optimized version!
//      def migrateTo( anotherBus: RichAudioBus )( implicit tx: ProcTxn ) {
//         val r = readers()
//         val w = writers()
//         r.foreach( removeReader( _ ))
//         w.foreach( removeWriter( _ ))
//         anotherBus.add( r, w )
//      }
//
//      // XXX eventually AudioImpl could have an optimized version!
//      protected def add( r: Iterable[ User ], w: Iterable[ User ])( implicit tx: ProcTxn ) {
//         r.foreach( addReader( _ ))
//         w.foreach( addWriter( _ ))
//      }
   }

   private class AudioImpl( val server: Server, val numChannels: Int ) extends BasicAudioImpl {
      import RichAudioBus._

      override def toString = "sh-abus@" + hashCode

      def addReader( u: User )( implicit tx: ProcTxn ) {
         val rs   = readers()
         require( !rs.contains( u ))
         val bh   = if( rs.isEmpty ) {
            val ws = writers()
            if( ws.isEmpty ) { // no bus yet, create an empty shared one
               val res = createReadOnlyBus( server, numChannels )
               bus.set( res )
//println( "addReader : " + this + " ; allocReadOnlyBus " + res )
               res
            } else { // dispose old dummy bus, create new bus
               val res        = createAudioBus( server, numChannels )
               val newBus     = new AudioBus( server, res.index, numChannels )
               val oldHolder  = bus.swap( res )
               rs foreach { r =>
                  oldHolder.free
                  r.busChanged( newBus )
                  res.alloc
               }
               ws foreach { w =>
                  oldHolder.free
                  w.busChanged( newBus )
                  res.alloc
               }
//println( "addReader : " + this + " ; allocAudioBus " + res )
               res
            }
         } else { // re-use existing bus
            bus()
//            val res = new AudioBus( server, bh.index, numChannels )
//println( "addReader : " + this + " ; re-alloc " + res )
//            res
         }
         readers.set( rs + u )
         // always perform this on the newly added
         // reader no matter if the bus is new:
         bh.alloc
         val newBus = new AudioBus( server, bh.index, numChannels )
         u.busChanged( newBus )
      }

      def addWriter( u: User )( implicit tx: ProcTxn ) {
         val ws   = writers()
         require( !ws.contains( u ))
         val bh   = if( ws.isEmpty ) {
            val rs = readers()
            if( rs.isEmpty ) { // no bus yet, create an empty shared one
               val res = createWriteOnlyBus( server, numChannels )
               bus.set( res )
               res
            } else { // dispose old dummy bus, create new bus
               val res        = createAudioBus( server, numChannels )
               val newBus     = new AudioBus( server, res.index, numChannels )
               val oldHolder  = bus.swap( res )
               rs foreach { r =>
                  oldHolder.free
                  r.busChanged( newBus )
                  res.alloc
               }
               ws foreach { w =>
                  oldHolder.free
                  w.busChanged( newBus )
                  res.alloc
               }
               res
            }
         } else { // re-use existing bus
            bus()
         }
         writers.set( ws + u )
         // always perform this on the newly added
         // reader no matter if the bus is new:
         bh.alloc
         val newBus = new AudioBus( server, bh.index, numChannels )
         u.busChanged( newBus )
      }

      def removeReader( u: User )( implicit tx: ProcTxn ) {
         val rs0        = readers()
         if( !rs0.contains( u )) return
         val rs         = rs0 - u
         readers.set( rs )
         val oldHolder  = bus()
         oldHolder.free
         if( rs.isEmpty ) {
            val ws = writers()
            if( ws.nonEmpty ) { // they can all go to write only
               val bh = createWriteOnlyBus( server, numChannels )
               bus.set( bh )
               val newBus = new AudioBus( server, bh.index, numChannels )
               ws foreach { w =>
                  oldHolder.free
                  w.busChanged( newBus )
                  bh.alloc
               }
            }
         }
      }

      def removeWriter( u: User )( implicit tx: ProcTxn ) {
         val ws0        = writers()
         if( !ws0.contains( u )) return
         val ws         = ws0 - u
         writers.set( ws )
         val oldHolder  = bus()
         oldHolder.free
         if( ws.isEmpty ) {
            val rs = readers()
            if( rs.nonEmpty ) { // they can all go to write only
               val bh = createReadOnlyBus( server, numChannels )
               bus.set( bh )
               val newBus = new AudioBus( server, bh.index, numChannels )
               rs foreach { r =>
                  oldHolder.free
                  r.busChanged( newBus )
                  bh.alloc
               }
            }
         }
      }
   }

   private class TempAudioImpl( val server: Server, val numChannels: Int ) extends BasicAudioImpl {
      import RichAudioBus._

      override def toString = "tmp-abus@" + hashCode

      //      private val users = Ref( Set.empty[ User ])

      def addReader( u: User )( implicit tx: ProcTxn ) { add( readers, writers, u )}
      def addWriter( u: User )( implicit tx: ProcTxn ) { add( writers, readers, u )}

      private def add( users: Ref[ Set[ User ]], others: Ref[ Set[ User ]], u: User )( implicit tx: ProcTxn ) {
         val us = users()
         require( !us.contains( u ))
         // do _not_ check for null
         // because we might have a disposed
         // bus there, so we must make sure to
         // re-allocate a new bus each time
         // the users count goes to 1!
         val bh   = if( us.isEmpty && others().isEmpty ) {
            val res = createAudioBus( server, numChannels )
            bus.set( res )
            res
         } else { // re-use existing bus
            bus()
         }
         users.set( us + u )
         // always perform this on the newly added
         // reader no matter if the bus is new:
         bh.alloc
         val newBus = new AudioBus( server, bh.index, numChannels )
         u.busChanged( newBus )
      }

      def removeReader( u: User )( implicit tx: ProcTxn ) { remove( readers, u )}
      def removeWriter( u: User )( implicit tx: ProcTxn ) { remove( writers, u )}

      private def remove( users: Ref[ Set[ User ]], u: User )( implicit tx: ProcTxn ) {
         val rw = users()
         if( !rw.contains( u )) return
         users.set( rw - u )
         bus().free
      }
   }

   private class ControlImpl( val server: Server, val numChannels: Int ) extends RichControlBus {
      import RichControlBus._

      private val bus      = Ref.make[ ControlBusHolder ]
      private val readers  = Ref( Set.empty[ User ])
      private val writers  = Ref( Set.empty[ User ])

      override def toString = "cbus@" + hashCode

      def busOption( implicit tx: ProcTxn ) = {
         val bh = bus()
         if( bh != null ) Some( bh.bus ) else None
      }

      def addReader( u: User )( implicit tx: ProcTxn ) { add( readers, writers, u )}
      def addWriter( u: User )( implicit tx: ProcTxn ) { add( writers, readers, u )}

      private def add( users: Ref[ Set[ User ]], others: Ref[ Set[ User ]], u: User )( implicit tx: ProcTxn ) {
         val us = users()
         require( !us.contains( u ))
         // do _not_ check for null
         // because we might have a disposed
         // bus there, so we must make sure to
         // re-allocate a new bus each time
         // the users count goes to 1!
         val bh   = if( us.isEmpty && others().isEmpty ) {
            val res = createControlBus( server, numChannels )
            bus.set( res )
            res
         } else { // re-use existing bus
            bus()
         }
         users.set( us + u )
         // always perform this on the newly added
         // reader no matter if the bus is new:
         bh.alloc
         val newBus = new ControlBus( server, bh.index, numChannels )
         u.busChanged( newBus )
      }

      def removeReader( u: User )( implicit tx: ProcTxn ) { remove( readers, u )}
      def removeWriter( u: User )( implicit tx: ProcTxn ) { remove( writers, u )}

      private def remove( users: Ref[ Set[ User ]], u: User )( implicit tx: ProcTxn ) {
         val rw = users()
         if( !rw.contains( u )) return
         users.set( rw - u )
         bus().free
      }
   }
}