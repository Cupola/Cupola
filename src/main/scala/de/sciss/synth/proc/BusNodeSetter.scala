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
 *    @version 0.10, 06-Jul-10
 */
trait AudioBusNodeSetter {
   def add( implicit tx: ProcTxn )    : AudioBusNodeSetter
   def remove( implicit tx: ProcTxn ) : AudioBusNodeSetter
}
object AudioBusNodeSetter {
   def reader( name: String, rb: RichAudioBus, rn: RichNode ) : AudioBusNodeSetter = new ReaderImpl( name, rb, rn )
   def writer( name: String, rb: RichAudioBus, rn: RichNode ) : AudioBusNodeSetter = new WriterImpl( name, rb, rn )
   def readerWriter( name: String, rb: RichAudioBus, rn: RichNode ) : AudioBusNodeSetter =
      new ReaderWriterImpl( name, rb, rn )

   private abstract class Impl( name: String, rn: RichNode )
   extends RichAudioBus.User with AudioBusNodeSetter {
      def busChanged( b: AudioBus )( implicit tx: ProcTxn ) {
         rn.setIfOnline( name -> b.index )
      }
   }

   /*
    *    Careful not use case classes here, as multiple readers / writers for the
    *    same combo might be wanted in a read / write set!
    */
   private class ReaderImpl( _name: String, rb: RichAudioBus, _rn: RichNode ) extends Impl( _name, _rn ) {
      def add( implicit tx: ProcTxn )    = { rb.addReader(    this ); this }
      def remove( implicit tx: ProcTxn ) = { rb.removeReader( this ); this }
   }

   /*
    *    Careful not use case classes here, as multiple readers / writers for the
    *    same combo might be wanted in a read / write set!
    */
   private class WriterImpl( _name: String, rb: RichAudioBus, _rn: RichNode ) extends Impl( _name, _rn ) {
      def add( implicit tx: ProcTxn )    = { rb.addWriter(    this ); this }
      def remove( implicit tx: ProcTxn ) = { rb.removeWriter( this ); this }
   }

   /*
    *    Careful not use case classes here, as multiple readers / writers for the
    *    same combo might be wanted in a read / write set!
    */
   private class ReaderWriterImpl( _name: String, rb: RichAudioBus, _rn: RichNode ) extends Impl( _name, _rn ) {
      object dummy extends RichAudioBus.User {
         def busChanged( b: AudioBus )( implicit tx: ProcTxn ) {}
      }

      def add( implicit tx: ProcTxn ) = {
         rb.addReader( this )
         rb.addWriter( dummy )
         this
      }

      def remove( implicit tx: ProcTxn ) = {
         rb.removeWriter( dummy )
         rb.removeWriter( this )
         this
      }
   }
}
