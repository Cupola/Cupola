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

import de.sciss.synth.{ audio => arate, control => krate, _ }
import de.sciss.synth.io.AudioFile
import de.sciss.scalaosc.{ OSCBundle, OSCMessage }
import de.sciss.synth.osc.{ OSCSyncedMessage, OSCSynthNewMessage }
import actors.{ DaemonActor, Future, TIMEOUT }
import collection.breakOut
import collection.immutable.{ IndexedSeq => IIdxSeq, Seq => ISeq }
import ProcTransport._
import de.sciss.synth.ugen.{In, Out}

/**
 *    @version 0.12, 24-Jun-10
 */
trait ProcFactoryBuilder {
   def name : String
   def pControl( name: String, spec: ParamSpec, default: Float ) : ProcParamControl
   def pAudio( name: String, spec: ParamSpec, default: Float ) : ProcParamAudio
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

   var verbose = false

   // ---------------------------- ProcFactoryBuilder implementation ----------------------------

   private class BuilderImpl( val name: String ) extends ProcFactoryBuilder {
      private var finished                   = false
      private var paramMap                   = Map.empty[ String, ProcParam[ _ ]]
      private var paramSeq                   = Vector.empty[ ProcParam[ _ ]]
      private var buffers                    = Map[ String, ProcBuffer ]()
      private var graph: Option[ ProcGraph ] = None
      private var entry: Option[ ProcEntry ] = None
      private var pAudioIns                  = Vector.empty[ ProcParamAudioInput ]
      private var pAudioOuts                 = Vector.empty[ ProcParamAudioOutput ]

      private var implicitAudioIn   = false
      private var implicitAudioOut  = false

      @inline private def requireOngoing = require( !finished, "ProcFactory build has finished" )

      def pControl( name: String, spec: ParamSpec, default: Float ) : ProcParamControl = {
         requireOngoing
         val p = new ParamControlImpl( name, spec, default )
         addParam( p )
         p
      }

      def pAudio( name: String, spec: ParamSpec, default: Float ) : ProcParamAudio = {
         requireOngoing
         val p = new ParamControlImpl( name, spec, default )
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
         pAudioIn( name, default, false )
      }

      private def pAudioIn( name: String, default: Option[ RichBus ], physical: Boolean ) : ProcParamAudioInput = {
         val p = new ParamAudioInputImpl( name, default, physical )
         addParam( p )
         pAudioIns :+= p
         p
      }

      def pAudioOut( name: String, default: Option[ RichBus ]) : ProcParamAudioOutput = {
         requireOngoing
         pAudioOut( name, default, false )
      }


      def pAudioOut( name: String, default: Option[ RichBus ], physical: Boolean ) : ProcParamAudioOutput = {
         val p = new ParamAudioOutputImpl( name, default, physical )
         addParam( p )
         pAudioOuts :+= p
         p
      }

      def graph( fun: GE => GE ) : ProcGraph = {
         val res = graph( fun( Proc.local.param( "in" ).asInstanceOf[ ProcParamAudioInput ].ar ))
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
         if( implicitAudioIn && !paramMap.contains( "in" )) {
            pAudioIn( "in", None, true )
         }
//println( "implicitAudioOut = " + implicitAudioOut + "; params.contains( \"out\" ) = " + params.contains( "out" ))
         if( implicitAudioOut && !paramMap.contains( "out" )) {
            pAudioOut( "out", None, true )
         }
         finished = true
         new FactoryImpl( name, entry.get, paramMap, paramSeq, pAudioIns, pAudioOuts )
      }

      private def addParam( p: ProcParam[ _ ]) {
         require( !paramMap.contains( p.name ), "Param name '" + p.name + "' already taken" )
         paramMap  += p.name -> p
         paramSeq :+= p 
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
                              val paramMap: Map[ String, ProcParam[ _ ]],
                              val paramSeq: IIdxSeq[ ProcParam[ _ ]],
                              val pAudioIns: IIdxSeq[ ProcParamAudioInput ],
                              val pAudioOuts: IIdxSeq[ ProcParamAudioOutput ])
   extends ProcFactory {
      def make( implicit tx: ProcTxn ) : Proc = {
         val res = new Impl( this, Server.default, name )
         ProcDemiurg.addVertex( res )
         res
      }

      override def toString = "gen(" + name + ")"
   }

   // ---------------------------- Proc implementation ----------------------------

//   private object Impl {
////      class Exec( thunk: => Unit ) { def exec = thunk }
////      case class Running( group: Group, synth: Synth ) extends Stoppable {
////
////      }
//   }

   private class Impl( fact: FactoryImpl, val server: Server, val name: String )
   extends Proc {
      proc =>

//      import Impl._
      import Proc._

      // ---- constructor ----
//      {
//         start       // start myself...
//         exec( withTx( NOW, 0, _.addProc( proc ))) // and announce...
//         addProc
//      }

//      private val sync           = new AnyRef
      private val running           = Ref.withObserver[ Option[ ProcRunning ]]( None ) { (oldRun, newRun) =>
         if( oldRun.isDefined != newRun.isDefined ) {
            dispatch( PlayingChanged( proc, newRun.isDefined ))
         }
      }
      private val groupVar          = Ref[ Option[ RichGroup ]]( None )
//      private val pFloatValues      = Ref( Map.empty[ ProcParamFloat, Float ])
      private val pStringValues     = Ref( Map.empty[ ProcParamString, String ])
      private val pAudioBusValues   = Ref( Map.empty[ ProcParamAudioBus, RichBus ])

      lazy val audioInputs          = fact.pAudioIns.map( new AudioInputImpl( this, _ ))
      lazy val audioOutputs         = fact.pAudioOuts.map( new AudioOutputImpl( this, _ ))
      lazy val controls             = fact.paramSeq.collect {
         case pControl: ProcParamControl  => new ControlImpl( proc, pControl, krate )
         case pAudio: ProcParamAudio      => new ControlImpl( proc, pAudio, arate )
      }
      private lazy val controlMap: Map[ String, ProcControl ] = controls.map( c => (c.name -> c) )( breakOut )

      def audioInput( name: String ) : ProcAudioInput    = audioInputs.find(  _.param.name == name ).get
      def audioOutput( name: String ) : ProcAudioOutput  = audioOutputs.find( _.param.name == name ).get

      def param( name: String ) : ProcParam[ _ ] = fact.paramMap( name )
      def params : IIdxSeq[ ProcParam[ _ ]] = fact.paramSeq

      def control( name: String ) : ProcControl = controlMap( name )

//      def setFloat( name: String, value: Float )( implicit tx: ProcTxn ) : Proc = {
//         val p = fact.paramMap( name ).asInstanceOf[ ProcParamFloat ]
//         pFloatValues.transform( _ + (p -> value) )
//         running().foreach( _.setFloat( name, value ))
//         this
//      }

      def setString( name: String, value: String )( implicit tx: ProcTxn ) : Proc = {
         val p = fact.paramMap( name ).asInstanceOf[ ProcParamString ]
         pStringValues.transform( _ + (p -> value) )
         running().foreach( _.setString( name, value ))
         this
      }

      def setAudioBus( name: String, value: RichBus )( implicit tx: ProcTxn ) : Proc = {
         val p = fact.paramMap( name ).asInstanceOf[ ProcParamAudioBus ]
         pAudioBusValues.transform( _ + (p -> value) )
//         running().foreach( _.setAudioBus( name, value ))
         val aout = audioOutput( name )
         aout.bus = Some( value ) // invokes busChanged
         this
      }

//      def getFloat( name: String )( implicit tx: ProcTxn ) : Float = {
//         val p = fact.paramMap( name ).asInstanceOf[ ProcParamFloat ]
//         pFloatValues().get( p ).getOrElse( p.default.getOrElse( pError( name )))
//      }

       def getString( name: String )( implicit tx: ProcTxn ) : String = {
          val p = fact.paramMap( name ).asInstanceOf[ ProcParamString ]
          pStringValues().get( p ).getOrElse( p.default.getOrElse( pError( name )))
      }

      def getAudioBus( name: String )( implicit tx: ProcTxn ) : RichBus = {
         val p = fact.paramMap( name ).asInstanceOf[ ProcParamAudioBus ]
         pAudioBusValues().get( p ).getOrElse( p.default.getOrElse( pError( name )))
      }

      private def pError( name: String ) = throw new ProcParamUnspecifiedException( name )

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

//      private[proc] def connect( out: ProcAudioOutput, in: ProcAudioInput )( implicit tx: ProcTxn ) {
//         // handle edge
//         val e = ProcEdge( out, in )
//         require( (out.proc == proc) && !edges().contains( e ))
//         edges.transform( _ + e )
//         ProcDemiurg.addEdge( e )
//      }

      private[proc] def disconnect( out: ProcAudioOutput, in: ProcAudioInput ) {
         error( "NOT YET IMPLEMENTED" )
      }

      private[proc] def insert( out: ProcAudioOutput, in: ProcAudioInput,
                                insert: (ProcAudioInput, ProcAudioOutput) ) {
         error( "NOT YET IMPLEMENTED" )
      }

//      private[proc] def busChanged( out: ProcAudioInput, index: Int, numChannels: Int )( implicit tx: ProcTxn ) {
//         running().foreach( _.busChanged( name, index, numChannels ))
//      }
//
//      private[proc] def busChanged( out: ProcAudioOutput, index: Int, numChannels: Int )( implicit tx: ProcTxn ) {
//         running().foreach( _.busChanged( name, index, numChannels ))
//      }

      private[proc] def busChanged( bus: ProcAudioBus, index: Int, numChannels: Int )( implicit tx: ProcTxn ) {
         if( verbose ) println( "PROC BUSCHANGED " + name + " / " + bus.name + " / " + index + " / " + running() )
         running().foreach( _.busChanged( bus.name, index, numChannels ))
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

      private[proc] def dispatchControlChange( ctrl: ProcControl, newValue: Float ) {
         dispatch( ControlsChanged( ctrl -> newValue ))
      }

      private[proc] def controlChange( ctrl: ProcControl, newValue: Float )( implicit tx: ProcTxn ) {
         running().foreach( _.setFloat( ctrl.name, newValue ))
      }

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

      def includeParam( param: ProcParam[ _ ]) {
         val p = Proc.local 
         param match {
//            case pFloat: ProcParamFloat => controls += SingleControlSetMap( pFloat.name, Proc.local.getFloat( pFloat.name ))
            case pFloat: ProcParamFloat => controls += SingleControlSetMap( pFloat.name,
               p.control( pFloat.name ).value( tx ))
            case pString: ProcParamString =>
            case pAudioBus: ProcParamAudioInput => controls += SingleControlSetMap( pAudioBus.name,
               p.audioInput( pAudioBus.name ).index( tx ))
            case pAudioBus: ProcParamAudioOutput => controls += SingleControlSetMap( pAudioBus.name,
               p.audioOutput( pAudioBus.name ).index( tx ))
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
//            val g          = SynthGraph.wrapOut( graph.fun, None )
            val g = SynthGraph {
               val res1 = graph.fun
               val rate = Rate.highest( res1.outputs.map( _.rate ): _* )
               if( (rate == arate) || (rate == krate) ) {
//                  val res2 = fadeTime.map( fdt => makeFadeEnv( fdt ) * res1 ) getOrElse res1
//                  val out = "out".kr
//                  if( rate == audio ) {
//                     Out.ar( out, res2 )
//                  } else {
//                     Out.kr( out, res2 )
//                  }
                  (Proc.local.param( "out" ).asInstanceOf[ ProcParamAudioOutput ]).ar( res1 )
               } else res1
            }

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

      def busChanged( name: String, index: Int, numChannels: Int )( implicit tx: ProcTxn ) {
//         error( "not yet supported" )
         // XXX check numChannels
         rs.set( true, name -> index )
      }

      def setGroup( g: RichGroup )( implicit tx: ProcTxn ) {
//         wa.add( rs, rs.synth.moveToHeadMsg( g ))
         rs.moveToHead( true, g )
      }
   }

   // ---------------------------- ProcBuffer implementation ----------------------------

   private abstract class AudioBusImpl extends /* ProcAudioBus with */ RichBus.User {
      protected val busRef : Ref[ Option[ RichBus ]] = Ref( None )
      protected val syntheticRef = Ref( false )
      protected val indexRef  = Ref( -1 )
      protected val edges     = Ref( Set.empty[ ProcEdge ])

      def bus( implicit tx: ProcTxn ) : Option[ RichBus ] = busRef()
      def index( implicit tx: ProcTxn ) = indexRef()
      def synthetic( implicit tx: ProcTxn ) = syntheticRef()
   }

   private class AudioInputImpl( val proc: Impl, val param: ProcParamAudioInput )
   extends AudioBusImpl with ProcAudioInput {
      in => 

      def name = param.name

      override def toString = "aIn(" + proc.name + " @ " + param.name + ")"

      def bus_=( newBus: Option[ RichBus ])( implicit tx: ProcTxn ) {
         if( verbose ) println( "IN BUS " + proc.name + " / " + newBus )
         val oldBus = busRef.swap( newBus )
         if( oldBus != newBus ) { // crucial to avoid infinite loops
            oldBus.foreach( _.removeReader( this ))
            newBus.foreach( _.addReader( this ))   // invokes busChanged
            edges().foreach( _.out.bus_=( newBus ))
         }
      }

      def synthetic_=( newSyn: Boolean )( implicit tx: ProcTxn ) {
         val oldSyn = syntheticRef.swap( newSyn )
         if( oldSyn != newSyn ) {
            edges().foreach( _.out.synthetic_=( newSyn ))
         }
      }

      def busChanged( index: Int, numChannels: Int )( implicit tx: ProcTxn ) {
         if( verbose ) println( "IN INDEX " + proc.name + " / " + index )
         indexRef.set( index )
         proc.busChanged( this, index, numChannels )
      }

      private[proc] def addEdge( e: ProcEdge )( implicit tx: ProcTxn ) {
         edges.transform( _ + e )
      }
   }

   private class AudioOutputImpl( val proc: Impl, val param: ProcParamAudioOutput )
   extends AudioBusImpl with ProcAudioOutput {
      out =>

      def name = param.name

      override def toString = "aOut(" + proc.name + " @ " + param.name + ")"

      def bus_=( newBus: Option[ RichBus ])( implicit tx: ProcTxn ) {
         if( verbose ) println( "OUT BUS " + proc.name + " / " + newBus )
         val oldBus = busRef.swap( newBus )
         if( oldBus != newBus ) { // crucial to avoid infinite loops
            oldBus.foreach( _.removeWriter( this ))
            newBus.foreach( _.addWriter( this ))   // invokes busChanged
            edges().foreach( _.in.bus_=( newBus ))
         }
      }

      def synthetic_=( newSyn: Boolean )( implicit tx: ProcTxn ) {
         val oldSyn = syntheticRef.swap( newSyn )
         if( oldSyn != newSyn ) {
            edges().foreach( _.in.synthetic_=( newSyn ))
         }
      }

      def busChanged( index: Int, numChannels: Int )( implicit tx: ProcTxn ) {
         if( verbose ) println( "OUT INDEX " + proc.name + " / " + index )
         indexRef.set( index )
         proc.busChanged( this, index, numChannels )
      }

      def ~>( in: ProcAudioInput )( implicit tx: ProcTxn ) : ProcAudioInput = {
//         proc.connect( this, in )  // establishes the edge

         // handle edge
         val e = ProcEdge( out, in )
         require( !edges().contains( e ))
         edges.transform( _ + e )
         ProcDemiurg.addEdge( e )
         
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
         val oldSyn  = synthetic
         if( !oldSyn ) {
            synthetic = true
            bus.foreach( oldBus => {
               bus = Some( RichBus.alloc( proc.server, oldBus.numChannels ))
            })
         }
         in.addEdge( e )

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

   private class ParamControlImpl( val name: String, val spec: ParamSpec, val _default: Float )
   extends ProcParamControl with ProcParamAudio {
      def default = Some( _default )
   }

   private class ControlImpl( val _proc: Impl, param: ProcParamFloat, _rate: Rate )
   extends ProcControl {
      ctrl =>

      private var valueRef = Ref.withObserver( default ) { (oldV, newV) =>
         if( oldV != newV ) _proc.dispatchControlChange( ctrl, newV )
      }
      def proc : Proc = _proc
      def rate = Some( _rate )
      def default : Float = param.default.getOrElse( 0f )
      def spec : ParamSpec = param.spec
      def name : String = param.name
      def value( implicit tx: ProcTxn ) : Float = valueRef()
      def value_=( newValue: Float )( implicit tx: ProcTxn ) {
         val oldValue = valueRef.swap( newValue )
         if( oldValue != newValue ) _proc.controlChange( this, newValue )
      }
   }

   private class ParamStringImpl( val name: String, val default: Option[ String ])
   extends ProcParamString {
   }

//   private class ParamAudioBusImpl( val name: String, val default: Option[ RichBus ])
//   extends ProcParamAudioInput with ProcParamAudioOutput {
//      def numChannels : Int = {
//         val b = Proc.local.getAudioBus( name )( ProcGraphBuilder.local.tx )
//         b.numChannels
//      }
//   }

   private class ParamAudioInputImpl( val name: String, val default: Option[ RichBus ], physical: Boolean )
   extends ProcParamAudioInput {
      def numChannels : Int = {
         val pb   = ProcGraphBuilder.local
         pb.includeParam( this )
         val b    = Proc.local.getAudioBus( name )( pb.tx )
         b.numChannels
      }

      def ar : GE = {
         val p    = Proc.local
         val pb   = ProcGraphBuilder.local
         implicit val tx   = pb.tx
         val b = try {
            p.getAudioBus( name )
         } catch { case e: ProcParamUnspecifiedException => {
            val ain = p.audioInput( name )
            if( ain.synthetic ) {
               val res = RichBus.alloc( p.server, 1 )
               ain.bus = Some( res )
               res
            } else if( physical ) {
               val res = RichBus.soundIn( p.server, 1 )
               ain.bus = Some( res )
               res
            } else throw e
         }}
         pb.includeParam( this ) // important: must be after ain.bus_=

         In.ar( name.kr, b.numChannels )
      }
   }

   private class ParamAudioOutputImpl( val name: String, val default: Option[ RichBus ], physical: Boolean )
   extends ProcParamAudioOutput {
      def numChannels : Int = {
         val pb   = ProcGraphBuilder.local
         pb.includeParam( this )
         val b    = Proc.local.getAudioBus( name )( pb.tx )
         b.numChannels
      }

      def ar( sig: GE ) : GE = {
         val numChannels   = sig.numOutputs
         val p             = Proc.local
         val pb            = ProcGraphBuilder.local 
         implicit val tx   = pb.tx
         val b = try {
            p.getAudioBus( name )
         } catch { case e: ProcParamUnspecifiedException => {
            val aout = p.audioOutput( name )
            if( aout.synthetic ) {
               val res = RichBus.alloc( p.server, numChannels )
               aout.bus = Some( res )
               res
            } else if( physical ) {
               val res = RichBus.soundOut( p.server, numChannels )
               aout.bus = Some( res )
               res
            } else throw e
         }}
         pb.includeParam( this ) // important: must be after aout.bus_=
         val sig2: GE = if( b.numChannels == numChannels ) {
            sig
         } else {
            println( "WARNING: Coercing output signal from " + numChannels + " into " + b.numChannels + " channels" )
            val chans   = sig.outputs
            List.tabulate( b.numChannels )( ch => chans( ch % numChannels ))
         }
         Out.ar( name.kr, sig2 )
      }
   }
}