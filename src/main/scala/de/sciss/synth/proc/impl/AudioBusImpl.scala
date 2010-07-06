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
import de.sciss.synth.{ ControlSetMap => CSet, _ }
import ugen._

/**
 *    @version 0.11, 06-Jul-10
 */
object AudioBusImpl {
   var verbose = true
}
abstract class AudioBusImpl /* extends ProcAudioBus */ /* with RichAudioBus.User */ {
   import AudioBusImpl._ 

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
      if( verbose ) println( this.toString + " : addEdge( " + e + " )" )
      edgesRef.transform( _ + e )
      edgeAdded( e )
   }
   protected def edgeAdded( e: ProcEdge )( implicit tx: ProcTxn ) : Unit
}

abstract class AbstractAudioInputImpl
extends AudioBusImpl with ProcAudioInput {
   in =>

   import AudioBusImpl._

//      def name = param.name

   def bus_=( newBus: Option[ RichAudioBus ])( implicit tx: ProcTxn ) {
//      if( verbose ) println( "IN BUS " + proc.name + " / " + newBus )
      val oldBus = busRef.swap( newBus )
      if( oldBus != newBus ) { // crucial to avoid infinite loops
         if( verbose ) println( this.toString + " : bus = " + newBus + " (old: " + oldBus + ")" )
// YYY
//         oldBus.foreach( _.removeReader( this ))
//         newBus.foreach( _.addReader( this ))   // invokes busChanged
         edges.foreach( _.out.bus_=( newBus ))
      }
   }

   def synthetic_=( newSyn: Boolean )( implicit tx: ProcTxn ) {
      val oldSyn = syntheticRef.swap( newSyn )
      if( oldSyn != newSyn ) {
         if( verbose ) println( this.toString + " : synthetic = " + newSyn )
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
   import AudioBusImpl._
   
   override def toString = "aIn(" + proc.name + " @ " + name + ")"

//      protected val edges = Ref( Set.empty[ ProcEdge ])

//   def busChanged( bus: AudioBus )( implicit tx: ProcTxn ) {
////      if( verbose ) println( "IN INDEX " + proc.name + " / " + bus )
////         indexRef.set( bus.index )
//      proc.busParamChanged( this, bus )
//   }

//   def sendToBack( xfade: XFade, backGroup: RichGroup )( implicit tx: ProcTxn ) {
//// XXX what now?
//   }

   def play( implicit tx: ProcTxn ) {
      if( verbose ) println( this.toString + " : play" )
   }

   def stop( implicit tx: ProcTxn ) {
      if( verbose ) println( this.toString + " : stop" )
      bus foreach { rb =>
         tx.transit match {
            case xfade: XFade => bus = None // XXX ??? korrekt
            case _ =>
         }
      }
   }
}

class AudioOutputImpl( val proc: ProcImpl, val name: String )
extends AudioBusImpl with ProcAudioOutput {
   out =>

   import AudioBusImpl._

//      def name = param.name

   override def toString = "aOut(" + proc.name + " @ " + name + ")"

//   private def fadeInGraph( numChannels: Int ) = SynthGraph {
//      val line    = Line.kr( 0, 1, "$dur".ir, "$done".ir )
//      val wBus    = line
//      val wIn     = (1 - line).sqrt
//      val sigIn   = In.ar( "$in".kr, numChannels )
//      val bus     = "$bus".kr
//      val sigBus  = In.ar( bus, numChannels )
//      ReplaceOut.ar( bus, (sigIn * wIn) + (sigBus * wBus) )
//   }
//
//   private def fadeOutGraph( numChannels: Int ) = SynthGraph {
//      val line    = Line.kr( 0, 1, "$dur".ir, "$done".ir )
//      val wIn     = line
//      val wBus    = (1 - line).sqrt
//      val sigIn   = In.ar( "$in".kr, numChannels )
//      val bus     = "$bus".kr
//      val sigBus  = In.ar( bus, numChannels )
//      ReplaceOut.ar( bus, (sigIn * wIn) + (sigBus * wBus) )
//   }

   private def xfadeGraph( numChannels: Int ) = SynthGraph {
      val line    = EnvGen.kr( Env( "$start".ir, List( EnvSeg( 1, "$stop".ir, varShape( "$shape".ir )))),
         timeScale = "$dur".ir, doneAction = "$done".ir )
      val wIn     = (1 - line).sqrt
      val wBus    = line
      val sigIn   = In.ar( "$in".kr, numChannels )
      val bus     = "$bus".kr
      val sigBus  = In.ar( bus, numChannels )
      ReplaceOut.ar( bus, (sigIn * wIn) + (sigBus * wBus) )
   }

   private def routeGraph( numChannels: Int ) = SynthGraph {
      Line.kr( 0, 0, "$dur".ir, "$done".ir )
      Out.ar( "$out".kr, In.ar( "$in".kr , numChannels))
   }

   def play( implicit tx: ProcTxn ) {
      if( verbose ) println( this.toString + " : play" )
      bus foreach { rb =>
         tx.transit match {
            case Instant                  =>
            case d: DurationalTransition  => fade( d, rb, 0 -> 1, welchShape, freeSelf )
         }
      }
   }

   def stop( implicit tx: ProcTxn ) {
      if( verbose ) println( this.toString + " : stop" )
      bus foreach { rb =>
         tx.transit match {
            case Instant      =>
            case xfade: XFade => {
               fade( xfade, rb, 1 -> 0, sinShape, doNothing )
            }
            case glide: Glide => {
               fade( glide, rb, 1 -> 0, sinShape, freeSelf )
            }
         }
      }
   }

//   private def fadeGraph( numChannels: Int ) = SynthGraph {
//      // a bit "pricey" but smooth
//      // ; alternative worth checking out: using curve -4 and .sqrt
//      // (xfade produces equal power curve)
//      val line    = EnvGen.kr( Env( "$start".ir, List( EnvSeg( 1, "$stop".ir, sinShape ))),
//         timeScale = "$dur".ir, doneAction = "$done".ir ) //.squared
//      val idx      = "$bus".kr
//      ReplaceOut.ar( idx, In.ar( idx, numChannels ) * line )
//   }

// this has a too fast attack
//   private def xfadeGraph( numChannels: Int ) = SynthGraph {
//      val line    = EnvGen.kr( Env( "$start".ir, List( EnvSeg( 1, "$stop".ir, curveShape( -4 )))),
//         timeScale = "$dur".ir, doneAction = "$done".ir )
//      val wBus    = line.sqrt
//      val wIn     = (1 - line).sqrt
//      val sigIn   = In.ar( "$in".kr, numChannels )
//      val bus     = "$bus".kr
//      val sigBus  = In.ar( bus, numChannels )
//      ReplaceOut.ar( bus, (sigIn * wIn) + (sigBus * wBus) )
//   }

   //   private def fadeOutGraph( numChannels: Int ) = SynthGraph {
//      // a bit "pricey" but smooth
//      val line    = EnvGen.kr( Env( 1, List( EnvSeg( 1, 0, sinShape ))),
//         timeScale = "$dur".ir, doneAction = "$done".ir ).squared
//      val in      = "$in".kr
//      ReplaceOut.ar( in, In.ar( in, numChannels ) * line )
//   }

//   private def play( d: DurationalTransition, rb: RichAudioBus )( implicit tx: ProcTxn ) {
//      val rg   = proc.postGroup
//      val rsd  = RichSynthDef( rg.server, fadeGraph( rb.numChannels ))
//      val rs   = rsd.play( rg, List( "$start" -> 0, "$stop" -> 1, "$dur" -> d.dur,
//                                     "$done" -> freeSelf.id ), addToTail )
//      val reader  = new RichAudioBus.User {
//         def busChanged( b: AudioBus )( implicit tx0: ProcTxn ) { rs.set( true, "$bus" -> b.index )( tx0 )}
//      }
//      val writer  = new RichAudioBus.User {
//         def busChanged( b: AudioBus )( implicit tx0: ProcTxn ) {}
//      }
//      if( verbose ) println( this.toString + " : play : addRW (" + rb + ")" )
//      rb.addReader( reader )
//      rb.addWriter( writer )
//      rs.onEnd { tx0 =>
//         if( verbose ) println( this.toString + " : p    : removeRW (" + rb + ")" )
//         rb.removeReader( reader )( tx0 )
//         rb.removeWriter( writer )( tx0 )
//      }
//   }

   /**
    *    Consideration: Eventually we might safe a few UGens by checking if rb is private
    *    (in that case we don't need to care about what was previously on that bus.
    *    _however_ : if we allow multiple fadeouts to nest, we probably must
    *    conserve the old signal. thus --> don't branch around
    *
    *    Furthermore, we assume that the background layer will be created appropriately
    *    by the proc, so we just play to the regular groups
    */
   private def fade( d: DurationalTransition, rb: RichAudioBus, line: (Int, Int), shape: ConstEnvShape,
                     doneAction: DoneAction )( implicit tx: ProcTxn ) {
      val server        = rb.server
      val numChannels   = rb.numChannels
      val rsd1          = RichSynthDef( server, routeGraph( numChannels ))
      val rsd2          = RichSynthDef( server, xfadeGraph( numChannels ))
      val tmpBus        = RichBus.tmpAudio( server, numChannels )
      val rs1           = rsd1.play( proc.preGroup, List( "$dur" -> d.dur, "$done" -> doneAction.id ))
      val rs2           = rsd2.play( proc.postGroup,
         List( "$start" -> line._1, "$stop" -> line._2, "$shape" -> shape.id,
              "$dur" -> d.dur, "$done" -> doneAction.id ))
      rs1.read(      rb     -> "$in" )
      rs1.write(     tmpBus -> "$out" )
      rs2.read(      tmpBus -> "$in" )
      rs2.readWrite( rb     -> "$bus" )
   }

   protected def edgeAdded( e: ProcEdge )( implicit tx: ProcTxn ) {
      proc.audioBusConnected( e )
   }

   def bus_=( newBus: Option[ RichAudioBus ])( implicit tx: ProcTxn ) {
//      if( verbose ) println( "OUT BUS " + proc.name + " / " + newBus )
      val oldBus = busRef.swap( newBus )
      if( oldBus != newBus ) { // crucial to avoid infinite loops
         if( verbose ) println( this.toString + " : bus = " + newBus + " (old: " + oldBus + ")" )
// YYY
//         oldBus.foreach( _.removeWriter( this ))
//         newBus.foreach( _.addWriter( this ))   // invokes busChanged
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
