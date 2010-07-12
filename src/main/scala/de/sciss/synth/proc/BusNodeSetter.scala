/*
 *  BusNodeSetter.scala
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

import de.sciss.synth.AudioBus

/**
 *    @version 0.11, 12-Jul-10
 */
trait AudioBusNodeSetter {
   def add( implicit tx: ProcTxn )     : AudioBusNodeSetter
   def remove( implicit tx: ProcTxn )  : AudioBusNodeSetter
   def migrateTo( newBus: RichAudioBus )( implicit tx: ProcTxn ) : AudioBusNodeSetter
   def bus: RichAudioBus
}
object AudioBusNodeSetter {
   def reader( name: String, rb: RichAudioBus, rn: RichNode ) : AudioBusNodeSetter = new ReaderImpl( name, rb, rn )
   def writer( name: String, rb: RichAudioBus, rn: RichNode ) : AudioBusNodeSetter = new WriterImpl( name, rb, rn )
   def readerWriter( name: String, rb: RichAudioBus, rn: RichNode ) : AudioBusNodeSetter =
      new ReaderWriterImpl( name, rb, rn )

   private abstract class Impl( val name: String, val rn: RichNode )
   extends RichAudioBus.User with AudioBusNodeSetter {
      val added = Ref( false ) 

      def busChanged( b: AudioBus )( implicit tx: ProcTxn ) {
         rn.setIfOnline( name -> b.index )
      }

      def migrateTo( newBus: RichAudioBus )( implicit tx: ProcTxn ) : AudioBusNodeSetter = {
         require( newBus.numChannels == bus.numChannels )
         val wasAdded = added()
         if( wasAdded ) remove
         val res = newInstance( name, newBus, rn )
         if( wasAdded ) res.add
         res
      }

      def newInstance( name: String, bus: RichAudioBus, rn: RichNode ) : AudioBusNodeSetter
   }

   /*
    *    Careful not use case classes here, as multiple readers / writers for the
    *    same combo might be wanted in a read / write set!
    */
   private class ReaderImpl( _name: String, val bus: RichAudioBus, _rn: RichNode ) extends Impl( _name, _rn ) {
      def add( implicit tx: ProcTxn ) = {
         val wasAdded = added.swap( true )
         if( wasAdded ) error( "Was already added : " + this )
         bus.addReader( this )
         this
      }

      def remove( implicit tx: ProcTxn ) = {
         val wasAdded = added.swap( false )
         if( wasAdded ) bus.removeReader( this )
         this
      }

      def newInstance( name: String, bus: RichAudioBus, rn: RichNode ) = reader( name, bus, rn )
   }

   /*
    *    Careful not use case classes here, as multiple readers / writers for the
    *    same combo might be wanted in a read / write set!
    */
   private class WriterImpl( _name: String, val bus: RichAudioBus, _rn: RichNode ) extends Impl( _name, _rn ) {
      def add( implicit tx: ProcTxn ) = {
         val wasAdded = added.swap( true )
         if( wasAdded ) error( "Was already added : " + this )
         bus.addWriter( this )
         this
      }

      def remove( implicit tx: ProcTxn ) = {
         val wasAdded = added.swap( false )
         if( wasAdded ) bus.removeWriter( this )
         this
      }

      def newInstance( name: String, bus: RichAudioBus, rn: RichNode ) = writer( name, bus, rn )
   }

   /*
    *    Careful not use case classes here, as multiple readers / writers for the
    *    same combo might be wanted in a read / write set!
    */
   private class ReaderWriterImpl( _name: String, val bus: RichAudioBus, _rn: RichNode ) extends Impl( _name, _rn ) {
      object dummy extends RichAudioBus.User {
         def busChanged( b: AudioBus )( implicit tx: ProcTxn ) {}
      }

      def add( implicit tx: ProcTxn ) = {
         val wasAdded = added.swap( true )
         if( wasAdded ) error( "Was already added : " + this )
         bus.addReader( this )
         bus.addWriter( dummy )
         this
      }

      def remove( implicit tx: ProcTxn ) = {
         val wasAdded = added.swap( false )
         if( wasAdded ) {
            bus.removeWriter( dummy )
            bus.removeReader( this )
         }
         this
      }

      def newInstance( name: String, bus: RichAudioBus, rn: RichNode ) = readerWriter( name, bus, rn )
   }
}
