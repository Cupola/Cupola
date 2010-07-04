/*
 *  AudioBusImpl.scala
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

package de.sciss.synth.proc.impl

import de.sciss.synth.proc._
import de.sciss.synth.{audio, AudioBus}

abstract class AudioBusImpl extends /* ProcAudioBus with */ RichAudioBus.User {
   protected val busRef : Ref[ Option[ RichAudioBus ]] = Ref( None )
   protected val syntheticRef = Ref( false )
//      protected val indexRef  = Ref( -1 )
//      protected val edges     = Ref( Set.empty[ ProcEdge ])
   private val edgesRef = Ref( Set.empty[ ProcEdge ])

   def bus( implicit tx: ProcTxn ) : Option[ RichAudioBus ] = busRef()
//      def index( implicit tx: ProcTxn ) = indexRef()
   def synthetic( implicit tx: ProcTxn ) = syntheticRef()

   def edges( implicit tx: ProcTxn ) = edgesRef()
   private[proc] def addEdge( e: ProcEdge )( implicit tx: ProcTxn ) {
      edgesRef.transform( _ + e )
      edgeAdded( e )
   }
   protected def edgeAdded( e: ProcEdge )( implicit tx: ProcTxn ) : Unit
}

abstract class AbstractAudioInputImpl
extends AudioBusImpl with ProcAudioInput {
   in =>

//      def name = param.name

   def bus_=( newBus: Option[ RichAudioBus ])( implicit tx: ProcTxn ) {
//      if( verbose ) println( "IN BUS " + proc.name + " / " + newBus )
      val oldBus = busRef.swap( newBus )
      if( oldBus != newBus ) { // crucial to avoid infinite loops
         oldBus.foreach( _.removeReader( this ))
         newBus.foreach( _.addReader( this ))   // invokes busChanged
         edges.foreach( _.out.bus_=( newBus ))
      }
   }

   def synthetic_=( newSyn: Boolean )( implicit tx: ProcTxn ) {
      val oldSyn = syntheticRef.swap( newSyn )
      if( oldSyn != newSyn ) {
         edges.foreach( _.out.synthetic_=( newSyn ))
      }
   }

//      def busChanged( bus: AudioBus )( implicit tx: ProcTxn ) {
//         if( verbose ) println( "IN INDEX " + proc.name + " / " + bus )
//         indexRef.set( bus.index )
//         proc.busChanged( this, bus )
//      }

   protected def edgeAdded( e: ProcEdge )( implicit tx: ProcTxn ) {}
}

class AudioInputImpl( val proc: ProcImpl, val name: String )
extends AbstractAudioInputImpl {
   override def toString = "aIn(" + proc.name + " @ " + name + ")"

//      protected val edges = Ref( Set.empty[ ProcEdge ])

   def busChanged( bus: AudioBus )( implicit tx: ProcTxn ) {
//      if( verbose ) println( "IN INDEX " + proc.name + " / " + bus )
//         indexRef.set( bus.index )
      proc.busParamChanged( this, bus )
   }
}

class AudioOutputImpl( val proc: ProcImpl, val name: String )
extends AudioBusImpl with ProcAudioOutput {
   out =>

//      def name = param.name

   override def toString = "aOut(" + proc.name + " @ " + name + ")"

//      protected val edges     = Ref.withObserver( Set.empty[ ProcEdge ]) { (oldSet, newSet) =>
//         val edgesRemoved     = oldSet.diff( newSet )
//         val edgesAdded       = newSet.diff( oldSet )
//         if( edgesRemoved.nonEmpty ) proc.dispatchAudioBusesDisconnected( edgesRemoved.toSeq: _* )
//         if( edgesAdded.nonEmpty )   proc.dispatchAudioBusesConnected(    edgesAdded.toSeq:   _* )
//      }

   protected def edgeAdded( e: ProcEdge )( implicit tx: ProcTxn ) {
      proc.audioBusConnected( e )
   }

   def bus_=( newBus: Option[ RichAudioBus ])( implicit tx: ProcTxn ) {
//      if( verbose ) println( "OUT BUS " + proc.name + " / " + newBus )
      val oldBus = busRef.swap( newBus )
      if( oldBus != newBus ) { // crucial to avoid infinite loops
         oldBus.foreach( _.removeWriter( this ))
         newBus.foreach( _.addWriter( this ))   // invokes busChanged
         edges.foreach( _.in.bus_=( newBus ))
      }
   }

   def synthetic_=( newSyn: Boolean )( implicit tx: ProcTxn ) {
      val oldSyn = syntheticRef.swap( newSyn )
      if( oldSyn != newSyn ) {
         edges.foreach( _.in.synthetic_=( newSyn ))
      }
   }

   def busChanged( bus: AudioBus )( implicit tx: ProcTxn ) {
//      if( verbose ) println( "OUT INDEX " + proc.name + " / " + bus )
//         indexRef.set( bus.index )
      proc.busParamChanged( this, bus )
   }

   def ~>( in: ProcAudioInput )( implicit tx: ProcTxn ) : Proc = {
      // handle edge
      val e = ProcEdge( out, in )
      require( !edges.contains( e ))
//         edges.transform( _ + e )
      addEdge( e )
      ProcDemiurg.addEdge( e )
      finishConnect( e )
      in.proc
   }

   private def finishConnect( e: ProcEdge )( implicit tx: ProcTxn ) {
      val oldSyn  = synthetic
      if( !oldSyn ) {
         synthetic = true
         bus.foreach( oldBus => {
            bus = Some( RichBus.audio( proc.server, oldBus.numChannels ))
         })
      } else {
         // XXX correct???
         e.in.synthetic   = true
         e.in.bus         = bus
      }
      e.in.addEdge( e )
  }

   def ~>( control: ProcControl )( implicit tx: ProcTxn ) : Proc = {
      val m = control.map( this )
      val e = m.edge // ProcEdge( out, m.input )
      require( !edges.contains( e ))
//         edges.transform( _ + e )
      addEdge( e )
      if( control.rate == Some( audio )) { // in this case we need to enforce topology
         ProcDemiurg.addEdge( e )
      }
      finishConnect( e )
      control.proc
   }

   def ~/>( in: ProcAudioInput )( implicit tx: ProcTxn ) : ProcAudioOutput = {
      proc.disconnect( this, in )
      this
   }

   def ~|( insert: (ProcAudioInput, ProcAudioOutput) )( implicit tx: ProcTxn ) : ProcAudioInsertion =
      new AudioInsertionImpl( proc, this, insert )
}

//   private class AudioInputMapImpl( control: ProcControl )
//   extends ProcAudioInput {
//
//   }

class AudioInsertionImpl( proc: ProcImpl, out: ProcAudioOutput, insert: (ProcAudioInput, ProcAudioOutput) )
                                ( implicit tx: ProcTxn )
extends ProcAudioInsertion {
   def |>( in: ProcAudioInput ) : ProcAudioInput = {
      proc.insert( out, in, insert )
      in
   }
}
