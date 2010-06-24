/*
 *  ProcFactoryBuilder.scala
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
import de.sciss.synth.io.AudioFile
import de.sciss.scalaosc.{ OSCBundle, OSCMessage }
import de.sciss.synth.osc.{ OSCSyncedMessage, OSCSynthNewMessage }
import actors.{ DaemonActor, Future, TIMEOUT }
import collection.breakOut
import collection.immutable.{ IndexedSeq => IIdxSeq, Seq => ISeq }
import ProcTransport._

/**
 *    @version 0.12, 24-Jun-10
 */
trait ProcFactoryBuilder {
   def name : String
   def pFloat( name: String, spec: ParamSpec, default: Option[ Float ]) : ProcParamFloat
   def pString( name: String, default: Option[ String ]) : ProcParamString
   def pAudioIn( name: String, default: Option[ RichBus ]) : ProcParamAudioInput
   def pAudioOut( name: String, default: Option[ RichBus ]) : ProcParamAudioOutput

   def graph( thunk: => GE ) : ProcGraph
   def graph( fun: GE => GE ) : ProcGraph

   def bufCue( name: String, path: String ) : ProcBuffer
   def bufCue( name: String, p: ProcParamString ) : ProcBuffer

   def finish : ProcFactory
}

object ProcFactoryBuilder extends ThreadLocalObject[ ProcFactoryBuilder ] {
   def apply( name: String )( thunk: => Unit ) : ProcFactory = {
      val b = new BuilderImpl( name )
      use( b ) {
         thunk
         b.finish
      }
   }

   // ---------------------------- ProcFactoryBuilder implementation ----------------------------

   private class BuilderImpl( val name: String ) extends ProcFactoryBuilder {
      private var finished                   = false
      private var params                     = Map[ String, ProcParam[ _ ]]()
      private var buffers                    = Map[ String, ProcBuffer ]()
      private var graph: Option[ ProcGraph ] = None
      private var entry: Option[ ProcEntry ] = None
      private var pAudioIns                  = Vector.empty[ ProcParamAudioInput ]
      private var pAudioOuts                 = Vector.empty[ ProcParamAudioOutput ]

      private var implicitAudioIn   = false
      private var implicitAudioOut  = false

      @inline private def requireOngoing = require( !finished, "ProcFactory build has finished" )

      def pFloat( name: String, spec: ParamSpec, default: Option[ Float ]) : ProcParamFloat = {
         requireOngoing
         val p = new ParamFloatImpl( name, spec, default )
         addParam( p )
         p
      }

      def pString( name: String, default: Option[ String ]) : ProcParamString = {
         requireOngoing
         val p = new ParamStringImpl( name, default )
         addParam( p )
         p
      }

      def pAudioIn( name: String, default: Option[ RichBus ]) : ProcParamAudioInput = {
         requireOngoing
         val p = new ParamAudioBusImpl( name, default )
         addParam( p )
         pAudioIns :+= p
         p
      }

      def pAudioOut( name: String, default: Option[ RichBus ]) : ProcParamAudioOutput = {
         requireOngoing
         val p = new ParamAudioBusImpl( name, default )
         addParam( p )
         pAudioOuts :+= p
         p
      }

//      def graph( thunk: => GE ) : ProcGraph = graph( () => thunk )

      def graph( fun: GE => GE ) : ProcGraph = {
         val res = graph( fun( Proc.local.getParam( "in" ).asInstanceOf[ ProcParamAudioInput ].ar ))
         implicitAudioIn = true
         res
      }

      def graph( thunk: => GE ) : ProcGraph = {
         requireOngoing
         require( graph.isEmpty, "Graph already defined" )
         val res = new GraphImpl( thunk )
         graph = Some( res )
         enter( res )
         implicitAudioOut = true
         res
      }

      def bufCue( name: String, path: String ) : ProcBuffer = {
         val b = new BufferImpl( name, implicit t => path )
         addBuffer( b )
         b
      }

      def bufCue( name: String, p: ProcParamString ) : ProcBuffer = {
         val b = new BufferImpl( name, implicit t => Proc.local.getString( p.name ))
         addBuffer( b )
         b
      }

       private def enter( e: ProcEntry ) {
         require( entry.isEmpty, "Entry already defined" )
         entry = Some( e )
      }

      def finish : ProcFactory = {
         requireOngoing
         require( entry.isDefined, "No entry point defined" )
         if( implicitAudioIn && !params.contains( "in" )) {
            pAudioIn( "in", None )
         }
         if( implicitAudioOut && !params.contains( "out" )) {
            pAudioOut( "out", None )
         }
         finished = true
         new FactoryImpl( name, entry.get, params, pAudioIns, pAudioOuts )
      }

      private def addParam( p: ProcParam[ _ ]) {
         require( !params.contains( p.name ), "Param name '" + p.name + "' already taken" )
         params += p.name -> p
      }

      private def addBuffer( b: ProcBuffer ) {
         require( !buffers.contains( b.name ), "Buffer name '" + b.name + "' already taken" )
         buffers += b.name -> b
      }
   }

//   private object BuilderDummy extends ProcFactoryBuilder {
//      def param( name: String, spec: ParamSpec, default: Float ) : ProcParam = outOfContext
//      def graph( name: String, thunk: => GE ) : ProcGraph = outOfContext
//      def enter( e: ProcEntry ) : Unit = outOfContext
//      def finish {}
//      def build( name: String ) : ProcFactory = outOfContext
//
//      private def outOfContext = error( "No way josé" )
//   }

   // ---------------------------- ProcFactory implementation ----------------------------

   private class FactoryImpl( val name: String, val entry: ProcEntry,
                              val params: Map[ String, ProcParam[ _ ]],
                              val pAudioIns: IIdxSeq[ ProcParamAudioInput ],
                              val pAudioOuts: IIdxSeq[ ProcParamAudioOutput ])
   extends ProcFactory {
      def make : Proc = new Impl( this, Server.default, name )

      override def toString = "gen(" + name + ")"
   }

   // ---------------------------- Proc implementation ----------------------------

   private object Impl {
//      class Exec( thunk: => Unit ) { def exec = thunk }
//      case class Running( group: Group, synth: Synth ) extends Stoppable {
//
//      }
   }

   private class Impl( fact: FactoryImpl, val server: Server, val name: String )
   extends Proc {
      proc =>

      import Impl._

      // ---- constructor ----
//      {
//         start       // start myself...
//         exec( withTx( NOW, 0, _.addProc( proc ))) // and announce...
//         addProc
//      }

//      private val sync           = new AnyRef
      private val running           = Ref[ Option[ ProcRunning ]]( None )
      private val groupVar          = Ref[ Option[ RichGroup ]]( None )
      private val pFloatValues      = Ref( Map.empty[ ProcParamFloat, Float ])
      private val pStringValues     = Ref( Map.empty[ ProcParamString, String ])
      private val pAudioBusValues   = Ref( Map.empty[ ProcParamAudioBus, RichBus ])
      private val edges             = Ref( Set.empty[ ProcEdge ])

      lazy val audioInputs          = fact.pAudioIns.map( new AudioInputImpl( this, _ ))
      lazy val audioOutputs         = fact.pAudioOuts.map( new AudioOutputImpl( this, _ ))

      def audioInput( name: String ) : ProcAudioInput    = audioInputs.find(  _.param.name == name ).get
      def audioOutput( name: String ) : ProcAudioOutput  = audioOutputs.find( _.param.name == name ).get

      private[proc] def getParam( name: String ) : ProcParam[ _ ] = fact.params( name )

      def setFloat( name: String, value: Float )( implicit tx: ProcTxn ) : Proc = {
         val p = fact.params( name ).asInstanceOf[ ProcParamFloat ]
         pFloatValues.transform( _ + (p -> value) )
         running().foreach( _.setFloat( name, value ))
         this
      }

      def setString( name: String, value: String )( implicit tx: ProcTxn ) : Proc = {
         val p = fact.params( name ).asInstanceOf[ ProcParamString ]
         pStringValues.transform( _ + (p -> value) )
         running().foreach( _.setString( name, value ))
         this
      }

      def setAudioBus( name: String, value: RichBus )( implicit tx: ProcTxn ) : Proc = {
         val p = fact.params( name ).asInstanceOf[ ProcParamAudioBus ]
         pAudioBusValues.transform( _ + (p -> value) )
         running().foreach( _.setAudioBus( name, value ))
         this
      }

      def getFloat( name: String )( implicit tx: ProcTxn ) : Float = {
         val p = fact.params( name ).asInstanceOf[ ProcParamFloat ]
         pFloatValues().get( p ).getOrElse( p.default.getOrElse(
            error( "Param '" + name + "' has not yet been assigned ")))
      }

       def getString( name: String )( implicit tx: ProcTxn ) : String = {
          val p = fact.params( name ).asInstanceOf[ ProcParamString ]
          pStringValues().get( p ).getOrElse( p.default.getOrElse(
             error( "Param '" + name + "' has not yet been assigned ")))
      }

      def getAudioBus( name: String )( implicit tx: ProcTxn ) : RichBus = {
         val p = fact.params( name ).asInstanceOf[ ProcParamAudioBus ]
         pAudioBusValues().get( p ).getOrElse( p.default.getOrElse(
            error( "Param '" + name + "' has not yet been assigned ")))
      }

      def group( implicit tx: ProcTxn ) : Option[ RichGroup ] = groupVar()

      private[proc] def setGroup( g: RichGroup )( implicit tx: ProcTxn ) {
         groupVar.set( Some( g ))
         running().foreach( _.setGroup( g ))
      }

      private def await[ A ]( timeOut: Long, fut: Future[ A ])( handler: Function1[ Option[ A ], Unit ]) : Nothing = {
         fut.inputChannel.reactWithin( timeOut ) {
            case TIMEOUT => handler( None )
            case a       => handler( Some( a.asInstanceOf[ A ]))
         }
      }

      private[proc] def connect( out: ProcAudioOutput, in: ProcAudioInput )( implicit tx: ProcTxn ) {
//         val e = out -> in
         val e = ProcEdge( out, in )
         require( (out.proc == proc) && !edges().contains( e ))
         edges.transform( _ + e )
         ProcDemiurg.addEdge( e )
      }

      private[proc] def disconnect( out: ProcAudioOutput, in: ProcAudioInput ) {
         error( "NOT YET IMPLEMENTED" )
      }

      private[proc] def insert( out: ProcAudioOutput, in: ProcAudioInput,
                                insert: (ProcAudioInput, ProcAudioOutput) ) {
         error( "NOT YET IMPLEMENTED" )
      }

      private[proc] def busChanged( out: ProcAudioInput, index: Int ) {
         error( "NOT YET IMPLEMENTED" )
      }

      private[proc] def busChanged( out: ProcAudioOutput, index: Int ) {
         error( "NOT YET IMPLEMENTED" )
      }

      def play( implicit tx: ProcTxn ) : Proc = {
         if( running().isDefined ) {
            println( "WARNING: Proc.play - '" + this + "' already playing")
         } else {
            val run = Proc.use( proc ) {
               fact.entry.play( groupVar().getOrElse( RichGroup.default( server )))
            }
            lazy val l: Model.Listener = {
               case ProcRunning.Stopped => {
                  run.removeListener( l )
//                     exec( if( running == Some( run )) running = None ) // XXX propagate stop?
                  ProcTxn.atomic { t2 =>
                     running.transformIfDefined({ case Some( run ) => None })( t2 )
                  }
               }
               case m => println( "Ooooops : " + m )
            }
            run.addListener( l )
            running.set( Some( run ))
         }
         this
      }

      def stop( implicit tx: ProcTxn ) : Proc = {
         running().foreach( r => {
            try { r.stop } catch { case ex => ex.printStackTrace() }
         })
         this
      }

      def isPlaying( implicit tx: ProcTxn ) : Boolean = running().isDefined

      override def toString = "proc(" + name + ")"
   }

   // ---------------------------- ProcGraph implementation ----------------------------

   private class GraphImpl( thunk: => GE ) extends ProcGraph {
      def fun = thunk
      def play( target: RichGroup )( implicit tx: ProcTxn ) : ProcRunning =
         new GraphBuilderImpl( this, tx ).play( target )
   }

   // ---------------------------- ProcGraphBuilder implementation ----------------------------

   private class GraphBuilderImpl( graph: GraphImpl, val tx: ProcTxn ) extends ProcGraphBuilder {
      var controls   = Set.empty[ ControlSetMap ]
      var buffers    = Set.empty[ BufferImpl ]

      def includeParam( p: ProcParam[ _ ]) {
         p match {
//            case pFloat: ProcParamFloat => controls += SingleControlSetMap( pFloat.name, Proc.local.getFloat( pFloat.name ))
            case pFloat: ProcParamFloat => controls += SingleControlSetMap( pFloat.name,
               Proc.local.getFloat( pFloat.name )( tx ))
            case pString: ProcParamString =>
//            case pAudioBus: ProcParamAudioBus => controls += SingleControlSetMap( pAudioBus.name,
//               Proc.local.getAudioBus( pAudioBus.name )( tx ).index )
            case x => println( "Ooops. what parameter is this? " + x ) // scalac doesn't check exhaustion...
         }
      }

      def includeBuffer( b: ProcBuffer ) {
         b match {
            case bi: BufferImpl => buffers += bi
            case _ => println( "WARNING: Currently not supporting buffer " + b )
         }
      }

      def play( target: RichGroup ) : ProcRunning = {
         ProcGraphBuilder.use( this ) {
            val g          = SynthGraph.wrapOut( graph.fun, None )
            val server     = Proc.local.server
            val rsd        = RichSynthDef( server, g )( tx )
//            rsd.recv( tx )
//            val addAction  = addToHead
            val args       = controls.toSeq
//            val synth      = Synth( server )
            val bufSeq     = buffers.toSeq
            val bufs       = bufSeq.map( _.create( server )( tx ))
//            val rs         = ProcDemiurg.addSynth( synth, g, synth.newMsg( _, target, args, addAction ), bufs )( tx )
            val rs         = rsd.play( target, args, addToHead, bufs )( tx )
            bufSeq.zip( bufs ).foreach( tup => {
               val (b, rb) = tup
               b.disposeWith( rb, rs )
            })
            new RunningImpl( rs )
         }
      }
   }

   private class RunningImpl( rs: RichSynth ) extends ProcRunning {
      import ProcRunning._
      
//      var pendingControls
      rs.synth.onEnd {
//         println( "n_end : " + synth )
         dispatch( Stopped )
      }
//      synth.server ! synthNewMsg

      def stop( implicit tx: ProcTxn ) = {
//         wa.add( rs, rs.synth.freeMsg )
         rs.free()
      }

      def setString( name: String, value: String )( implicit tx: ProcTxn ) { error( "not yet supported" )}

      def setFloat( name: String, value: Float )( implicit tx: ProcTxn ) {
//         wa.add( rs, rs.synth.setMsg( name -> value ))
         rs.set( true, name -> value )
      }

      def setAudioBus( name: String, value: RichBus )( implicit tx: ProcTxn ) { error( "not yet supported" )}

      def setGroup( g: RichGroup )( implicit tx: ProcTxn ) {
//         wa.add( rs, rs.synth.moveToHeadMsg( g ))
         rs.moveToHead( true, g )
      }
   }

   // ---------------------------- ProcBuffer implementation ----------------------------

   private class AudioInputImpl( val proc: Impl, val param: ProcParamAudioInput )
   extends ProcAudioInput with RichBus.User {
//      private val bus = Ref( RichBus.soundIn( proc.server, 1 ))
      private val busRef : Ref[ Option[ RichBus ]] = Ref( None )
      def bus( implicit tx: ProcTxn ) : Option[ RichBus ] = busRef()
      def bus_=( newBus: Option[ RichBus ])( implicit tx: ProcTxn ) {
         val oldBus = busRef.swap( newBus )
         if( oldBus != newBus ) {
            oldBus.foreach( _.removeReader( this ))
            newBus.foreach( _.addReader( this ))   // invokes busChanged
         }
      }

      def busChanged( index: Int )( implicit tx: ProcTxn ) {
         proc.busChanged( this, index )
      }
   }

   private class AudioOutputImpl( val proc: Impl, val param: ProcParamAudioOutput )
   extends ProcAudioOutput with RichBus.User {
//      private val bus = Ref( RichBus.soundOut( proc.server, 1 ))
      private val busRef : Ref[ Option[ RichBus ]] = Ref( None )
      def bus( implicit tx: ProcTxn ) : Option[ RichBus ] = busRef()
      def bus_=( newBus: Option[ RichBus ])( implicit tx: ProcTxn ) {
         val oldBus = busRef.swap( newBus )
         if( oldBus != newBus ) {
            oldBus.foreach( _.removeWriter( this ))
            newBus.foreach( _.addWriter( this ))   // invokes busChanged
         }
      }

      def busChanged( index: Int )( implicit tx: ProcTxn ) {
         proc.busChanged( this, index )
      }

      def ~>( in: ProcAudioInput )( implicit tx: ProcTxn ) : ProcAudioInput = {
         proc.connect( this, in )  // establishes the edge

/*
create + set output bus if necessary
was silent-bus ? : after-commit : free silent-bus
: after-rollback --> free bus
// --> future refinement: re-use buses if possible

check ain
- was patched --> disconnect
// --> future enhancement : special multi-in-bus (for summing)
- numChannels changed && p2 playing --> stop p2
- set input bus
- numChannels changed && p2 was playing?
  [ALGO1]( proc = p2 ) :=
    - start proc
    - for each pAudioOutBus if numChannels changes:
	- each target vertex playing --> stop proc
	- free old synthetic bus
	- create + assign new bus
	- stopped procs : start [ALGO1]( _ )
// note: (23-jun-10) : ProcParamAudioOutput:ar needs change
 */
         in.bus = busRef()
//
//         if( proc.isPlaying ) {
//
//         } else {
////            bus() match {
////               case Some( oldBus: RichBus ) => {
////                  oldBus.share
////               }
//////            case Some( oldBus: DummySharedBus ) => {
//////               val newBus = SharedBus.alloc( proc.server, oldBus.numChannels )
//////               oldBus.free
//////               bus.set( Some( newBus ))
//////            }
////            }
//         }
         in
      }

      def ~/>( in: ProcAudioInput )( implicit tx: ProcTxn ) : ProcAudioOutput = {
         proc.disconnect( this, in )
         this
      }

      def ~|( insert: (ProcAudioInput, ProcAudioOutput) )( implicit tx: ProcTxn ) : ProcAudioInsertion =
         new AudioInsertionImpl( proc, this, insert )
   }

   private class AudioInsertionImpl( proc: Impl, out: ProcAudioOutput, insert: (ProcAudioInput, ProcAudioOutput) )
                                   ( implicit tx: ProcTxn )
   extends ProcAudioInsertion {
      def |>( in: ProcAudioInput ) : ProcAudioInput = {
         proc.insert( out, in, insert )
         in
      }
   }

   // XXX either BufferCueImpl or put cueing information in Graph instead
   private class BufferImpl( val name: String, path: ProcTxn => String ) extends ProcBuffer {
      def controlName   = "buf$" + name

      def create( server: Server )( implicit tx: ProcTxn ) : RichBuffer = {
         val b = Buffer( server )
         val rb = RichBuffer( b )
         rb.alloc( 32768, numChannels )
         rb.cue( path( tx ))
         rb
      }

      def disposeWith( rb: RichBuffer, rs: RichSynth ) {
         rs.synth.onEnd { rb.server ! rb.buf.closeMsg( rb.buf.freeMsg )} // XXX update RichBuffer fields !
      }

      def numChannels : Int = {
         try {
            val spec = AudioFile.readSpec( path( ProcGraphBuilder.local.tx ))
            spec.numChannels
         } catch {
            case e => e.printStackTrace()
            1  // XXX what should we do? --> FAIL
         }
      }

      def id : GE = {
         ProcGraphBuilder.local.includeBuffer( this )
         controlName.kr
      }
   }

   // ---------------------------- ProcParam implementations ----------------------------

   private class ParamFloatImpl( val name: String, val spec: ParamSpec, val default: Option[ Float ])
   extends ProcParamFloat {
   }

   private class ParamStringImpl( val name: String, val default: Option[ String ])
   extends ProcParamString {
   }

   private class ParamAudioBusImpl( val name: String, val default: Option[ RichBus ])
   extends ProcParamAudioInput with ProcParamAudioOutput {
      def numChannels : Int = {
         val b = Proc.local.getAudioBus( name )( ProcGraphBuilder.local.tx )
         b.numChannels
      }
   }
}