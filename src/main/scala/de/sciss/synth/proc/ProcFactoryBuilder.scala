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
import collection.immutable.{ IndexedSeq => IIdxSeq, Queue => IQueue, Seq => ISeq }
import ProcTransport._
import ugen._

/**
 *    @version 0.12, 01-Jul-10
 */
trait ProcFactoryBuilder {
   def name : String
   def pControl( name: String, spec: ParamSpec, default: Float ) : ProcParamControl
   def pAudio( name: String, spec: ParamSpec, default: Float ) : ProcParamAudio
   def pString( name: String, default: Option[ String ]) : ProcParamString
   def pAudioIn( name: String, default: Option[ RichAudioBus ]) : ProcParamAudioInput
   def pAudioOut( name: String, default: Option[ RichAudioBus ]) : ProcParamAudioOutput

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
      private var paramMap                   = Map.empty[ String, ProcParam ]
      private var paramSeq                   = Vector.empty[ ProcParam ]
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
         val p = new ParamAudioImpl( name, spec, default )
         addParam( p )
         p
      }

      def pString( name: String, default: Option[ String ]) : ProcParamString = {
         requireOngoing
         val p = new ParamStringImpl( name, default )
         addParam( p )
         p
      }

      def pAudioIn( name: String, default: Option[ RichAudioBus ]) : ProcParamAudioInput = {
         requireOngoing
         pAudioIn( name, default, false )
      }

      private def pAudioIn( name: String, default: Option[ RichAudioBus ], physical: Boolean ) : ProcParamAudioInput = {
         val p = new ParamAudioInputImpl( name, default, physical )
         addParam( p )
         pAudioIns :+= p
         p
      }

      def pAudioOut( name: String, default: Option[ RichAudioBus ]) : ProcParamAudioOutput = {
         requireOngoing
         pAudioOut( name, default, false )
      }


      def pAudioOut( name: String, default: Option[ RichAudioBus ], physical: Boolean ) : ProcParamAudioOutput = {
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

      private def addParam( p: ProcParam ) {
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
                              val paramMap: Map[ String, ProcParam ],
                              val paramSeq: IIdxSeq[ ProcParam ],
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

   private def pError( name: String ) = throw new ProcParamUnspecifiedException( name )
   private def mError( name: String ) = error( "Bus mapping (" + name + ") must be complete at this stage" )
   
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
      private val playGroupVar      = Ref[ Option[ RichGroup ]]( None )
//      private val pFloatValues    = Ref( Map.empty[ ProcParamFloat, Float ])
      private val pStringValues     = Ref( Map.empty[ ProcParamString, String ])
//      private val pAudioBusValues   = Ref( Map.empty[ ProcParamAudioBus, RichAudioBus ])
//      private val controlMapMap   = Ref( Map.empty[ ProcControl, ControlMapping ])

      lazy val audioInputs          = fact.pAudioIns.map(  p => new AudioInputImpl(  this, p.name ))
      lazy val audioOutputs         = fact.pAudioOuts.map( p => new AudioOutputImpl( this, p.name ))
      lazy val controls             = fact.paramSeq.collect {
         case pControl: ProcParamControl  => new ControlImpl( proc, pControl, krate )
         case pAudio: ProcParamAudio      => new ControlImpl( proc, pAudio, arate )
      }
      private lazy val controlMap: Map[ String, ProcControl ] = controls.map( c => (c.name -> c) )( breakOut )

      def audioInput( name: String ) : ProcAudioInput    = audioInputs.find(  _.name == name ).get
      def audioOutput( name: String ) : ProcAudioOutput  = audioOutputs.find( _.name == name ).get

      def param( name: String ) : ProcParam = fact.paramMap( name )
      def params : IIdxSeq[ ProcParam ] = fact.paramSeq

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

//      def setAudioBus( name: String, value: RichAudioBus )( implicit tx: ProcTxn ) : Proc = {
//         val p = fact.paramMap( name ).asInstanceOf[ ProcParamAudioBus ]
//         pAudioBusValues.transform( _ + (p -> value) )
////         running().foreach( _.setAudioBus( name, value ))
//         val aout = audioOutput( name )
//         aout.bus = Some( value ) // invokes busChanged
//         this
//      }

//      def getFloat( name: String )( implicit tx: ProcTxn ) : Float = {
//         val p = fact.paramMap( name ).asInstanceOf[ ProcParamFloat ]
//         pFloatValues().get( p ).getOrElse( p.default.getOrElse( pError( name )))
//      }

       def getString( name: String )( implicit tx: ProcTxn ) : String = {
          val p = fact.paramMap( name ).asInstanceOf[ ProcParamString ]
//          pStringValues().get( p ).getOrElse( p.default.getOrElse( pError( name )))
          pStringValues().get( p ).getOrElse( pError( name ))
      }

//      def getAudioBus( name: String )( implicit tx: ProcTxn ) : RichAudioBus = {
//         val p = fact.paramMap( name ).asInstanceOf[ ProcParamAudioBus ]
////         pAudioBusValues().get( p ).getOrElse( p.default.getOrElse( pError( name )))
//         pAudioBusValues().get( p ).getOrElse( pError( name ))
//      }

      def groupOption( implicit tx: ProcTxn ) : Option[ RichGroup ] = groupVar()
//      def groupOption_=( newGroup: Option[ RichGroup ])( implicit tx: ProcTxn ) {
//         val oldGroup = groupVar.swap( newGroup )
//         // XXX what should we do, free it?
//         oldGroup.foreach( _.free )
//         running().foreach( _.setGroup( newGroup ))
//      }

      def group( implicit tx: ProcTxn ) : RichGroup = {
         groupOption.getOrElse({
            val g    = Group( server )
            val res  = RichGroup( g )
            res.play( RichGroup.default( server ))
            group    = res
            res
         })
      }
      def group_=( newGroup: RichGroup )( implicit tx: ProcTxn ) {
//         groupOption = Some( newGroup )
         val newGroupO  = Some( newGroup )
         val oldGroupO  = groupVar.swap( newGroupO )
         val pgo = playGroupOption
         pgo.foreach( _.moveToHead( true, newGroup ))
         // XXX also move the mapping synths!
         if( pgo.isEmpty ) { // we are still playing in the main group, hence update that
            running().foreach( _.setGroup( newGroup ))
         }
         // XXX what should we do, free it?
         oldGroupO.foreach( _.free( true ))
      }

      def playGroupOption( implicit tx: ProcTxn ) : Option[ RichGroup ] = playGroupVar()

      def playGroup( implicit tx: ProcTxn ) : RichGroup = {
//println( "playGroup" )
         playGroupOption.getOrElse({
            val g       = Group( server )
            val res     = RichGroup( g )
            res.play( group ) // creates group if necessary
            playGroup   = res
            res
         })
      }

      def playGroup_=( newGroup: RichGroup )( implicit tx: ProcTxn ) {
//println( "playGroup_" )
         val newGroupO  = Some( newGroup )
         val oldGroupO  = playGroupVar.swap( newGroupO )
         running().foreach( _.setGroup( newGroup ))
         // XXX what should we do, free it?
         oldGroupO.foreach( rg => {
            rg.free( true ) // after running.setGroup !
         })
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

      def play( implicit tx: ProcTxn ) : Proc = {
         if( running().isDefined ) {
            println( "WARNING: Proc.play - '" + this + "' already playing")
         } else {
            val run = Proc.use( proc ) {
//               val target = playGroupOption.getOrElse( groupOption.getOrElse( RichGroup.default( server )))
               fact.entry.play
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

      private[proc] def dispatchMappingChange( ctrl: ProcControl, newValue: Option[ ProcControlMapping ]) {
         dispatch( MappingsChanged( ctrl -> newValue ))
      }

      private[proc] def dispatchAudioBusesConnected( edges: ProcEdge* ) {
         dispatch( AudioBusesConnected( edges: _* ))
      }

      private[proc] def dispatchAudioBusesDisconnected( edges: ProcEdge* ) {
         dispatch( AudioBusesDisconnected( edges: _* ))
      }

      private[proc] def controlChanged( ctrl: ProcControl, newValue: Float )( implicit tx: ProcTxn ) {
         running().foreach( _.setFloat( ctrl.name, newValue ))
      }

//      // XXX should be unified with controlChange such that we look up if the
//      // control is currently mapped or not?
//      private[proc] def controlMapChanged( ctrl: ProcControl, rate: Rate, index: Int, numChannels: Int )
//                                         ( implicit tx: ProcTxn ) {
//         group.foreach( rg => rate match {
//            case `arate` => rg.mapan( true, (ctrl.name, index, numChannels) )
//            case `krate` => rg.mapn(  true, (ctrl.name, index, numChannels) )
//         })
//      }

      private[proc] def busParamChanged( bus: ProcAudioBus, abus: AudioBus )( implicit tx: ProcTxn ) {
         if( verbose ) println( "PROC BUS PARAM CHANGED " + name + " / " + bus.name + " / " + abus + " / " + running() )
         running().foreach( _.busChanged( bus.name, abus ))
      }

//      private[proc] def busMapChanged( bus: ControlAudioMapping, abus: AudioBus )( implicit tx: ProcTxn ) {
//         if( verbose ) println( "PROC BUS MAP CHANGED " + name + " / " + bus.name + " / " + abus + " / " + running() )
////         running().foreach( _.busChanged( bus.name, abus ))
//         error( "NOT YET IMPLEMENTED" )
//      }

//      private[proc] def controlMapped( m: ControlAudioMapping )( implicit tx: ProcTxn ) {
//         m.connect
//      }

      override def toString = "proc(" + name + ")"
   }

   // ---------------------------- ProcGraph implementation ----------------------------

   private class GraphImpl( thunk: => GE ) extends ProcGraph {
      def fun = thunk
      def play( implicit tx: ProcTxn ) : ProcRunning =
         new GraphBuilderImpl( this, tx ).play
   }

   // ---------------------------- ProcGraphBuilder implementation ----------------------------

   private class GraphBuilderImpl( graph: GraphImpl, val tx: ProcTxn ) extends ProcGraphBuilder {
//      var controls   = Set.empty[ ControlSetMap ]
      var usedParams = Set.empty[ ProcParam ]
      var buffers    = Set.empty[ BufferImpl ]

      // XXX we might not even need this, at least for graphs
      // as the included parameters are directly accessible
      // from the SynthGraph !
      def includeParam( param: ProcParam ) {
         usedParams += param
      }

      def includeBuffer( b: ProcBuffer ) {
         b match {
            case bi: BufferImpl => buffers += bi
            case _ => println( "WARNING: Currently not supporting buffer " + b )  // XXX
         }
      }

      /*
       *    Since the mappings might create the playGroup,
       *    and Proc will have running = None _during_ this
       *    call, it is crucial not to pass in a target
       *    node, but instead have the graphbuilder determine
       *    the target!
       */
      def play : ProcRunning = {
         implicit val t = tx
         ProcGraphBuilder.use( this ) {
            val p = Proc.local
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
                  (p.param( "out" ).asInstanceOf[ ProcParamAudioOutput ]).ar( res1 )
               } else res1
            }

            val server     = p.server
            val rsd        = RichSynthDef( server, g )
            val bufSeq     = buffers.toSeq
            val bufs       = bufSeq.map( _.create( server ))

            var setMaps    = Vector.empty[ ControlSetMap ]  // warning: rs.newMsg doesn't support setn style! XXX
//            var kbusMaps   = IQueue.empty[ ControlKBusMap ]
//            var abusMaps   = IQueue.empty[ ControlABusMap ]
            var mappings   = IQueue.empty[ ProcControlMapping ]
//Debug.breakpoint
            usedParams.foreach( _ match {
               case pFloat: ProcParamFloat => {
                  val name = pFloat.name
                  val ctrl = p.control( name )
                  ctrl.mapping.map( m => mappings = mappings.enqueue( m )).getOrElse({
                     setMaps :+= SingleControlSetMap( name, ctrl.value )
                  })
               }
               case pAudioBus: ProcParamAudioInput => {
                  setMaps :+= SingleControlSetMap(
                     pAudioBus.name, p.audioInput( pAudioBus.name ).bus.get.busOption.get.index )
               }
               case pAudioBus: ProcParamAudioOutput => {
                  setMaps :+= SingleControlSetMap(
                     pAudioBus.name, p.audioOutput( pAudioBus.name ).bus.get.busOption.get.index )
               }
               case x => println( "Ooops. what parameter is this? " + x ) // scalac doesn't check exhaustion...
            })
            // crucial here to ensure that group is
            // created and used, if we expect mappings.
            val target = if( mappings.nonEmpty ) {
//               p.playGroup
               p.playGroupOption.getOrElse( p.group )
            } else {
               p.playGroupOption.getOrElse( p.groupOption.getOrElse( RichGroup.default( server )))
            }
            val bufsZipped = bufSeq.zip( bufs )
            setMaps ++= bufsZipped.map( tup => SingleControlSetMap( tup._1.controlName, tup._2.buf.id ))
            val rs = rsd.play( target, setMaps, addToHead, bufs )
//            if( kbusMaps.nonEmpty ) rs.mapn(  true, kbusMaps: _* )
//            if( abusMaps.nonEmpty ) rs.mapan( true, abusMaps: _* )
            mappings.foreach( _.play ) // XXX where's the stop??
            bufsZipped.foreach( tup => {
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

      def busChanged( name: String, bus: AudioBus )( implicit tx: ProcTxn ) {
//         error( "not yet supported" )
         // XXX check numChannels
         rs.set( true, name -> bus.index )
      }

      def setGroup( g: RichGroup )( implicit tx: ProcTxn ) {
//         wa.add( rs, rs.synth.moveToHeadMsg( g ))
         rs.moveToHead( true, g )
      }
   }

   // ---------------------------- ProcBuffer implementation ----------------------------

   private abstract class AudioBusImpl extends /* ProcAudioBus with */ RichAudioBus.User {
      protected val busRef : Ref[ Option[ RichAudioBus ]] = Ref( None )
      protected val syntheticRef = Ref( false )
//      protected val indexRef  = Ref( -1 )
//      protected val edges     = Ref( Set.empty[ ProcEdge ])
      protected def edges : Ref[ Set[ ProcEdge ]] //     = Ref( Set.empty[ ProcEdge ])

      def bus( implicit tx: ProcTxn ) : Option[ RichAudioBus ] = busRef()
//      def index( implicit tx: ProcTxn ) = indexRef()
      def synthetic( implicit tx: ProcTxn ) = syntheticRef()
   }

   private abstract class AbstractAudioInputImpl
   extends AudioBusImpl with ProcAudioInput {
      in =>

//      def name = param.name

      def bus_=( newBus: Option[ RichAudioBus ])( implicit tx: ProcTxn ) {
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

//      def busChanged( bus: AudioBus )( implicit tx: ProcTxn ) {
//         if( verbose ) println( "IN INDEX " + proc.name + " / " + bus )
//         indexRef.set( bus.index )
//         proc.busChanged( this, bus )
//      }

      private[proc] def addEdge( e: ProcEdge )( implicit tx: ProcTxn ) {
         edges.transform( _ + e )
      }
   }

   private class AudioInputImpl( val proc: Impl, val name: String )
   extends AbstractAudioInputImpl {
      override def toString = "aIn(" + proc.name + " @ " + name + ")"

      protected val edges = Ref( Set.empty[ ProcEdge ])

      def busChanged( bus: AudioBus )( implicit tx: ProcTxn ) {
         if( verbose ) println( "IN INDEX " + proc.name + " / " + bus )
//         indexRef.set( bus.index )
         proc.busParamChanged( this, bus )
      }
   }

   private class AudioOutputImpl( val proc: Impl, val name: String )
   extends AudioBusImpl with ProcAudioOutput {
      out =>

//      def name = param.name

      override def toString = "aOut(" + proc.name + " @ " + name + ")"

      protected val edges     = Ref.withObserver( Set.empty[ ProcEdge ]) { (oldSet, newSet) =>
         val edgesRemoved     = oldSet.diff( newSet )
         val edgesAdded       = newSet.diff( oldSet )
         if( edgesRemoved.nonEmpty ) proc.dispatchAudioBusesDisconnected( edgesRemoved.toSeq: _* )
         if( edgesAdded.nonEmpty )   proc.dispatchAudioBusesConnected(    edgesAdded.toSeq:   _* )
      }

      def bus_=( newBus: Option[ RichAudioBus ])( implicit tx: ProcTxn ) {
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

      def busChanged( bus: AudioBus )( implicit tx: ProcTxn ) {
         if( verbose ) println( "OUT INDEX " + proc.name + " / " + bus )
//         indexRef.set( bus.index )
         proc.busParamChanged( this, bus )
      }

      def ~>( in: ProcAudioInput )( implicit tx: ProcTxn ) : Proc = {
         // handle edge
         val e = ProcEdge( out, in )
         require( !edges().contains( e ))
         edges.transform( _ + e )
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
         val e = ProcEdge( out, m.input )
         require( !edges().contains( e ))
         edges.transform( _ + e )
         if( control.rate == Some( arate )) { // in this case we need to enforce topology
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
         try { // XXX should call includeBuffer ?
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

   private class ParamControlImpl( val name: String, val spec: ParamSpec, val default: Float )
   extends ProcParamControl {
      def kr : GE = {
         val p             = Proc.local
         val pb            = ProcGraphBuilder.local
         implicit val tx   = pb.tx
         val c             = p.control( name )
         pb.includeParam( this )
         c.mapping.map( m => {
            val outBus = m.output // .get
            require( outBus.rate == `krate` )
            // stupidly we have two arguments...
            name.kr( default, List.fill( outBus.numChannels - 1 )( default ): _* )
         }).getOrElse( name.kr( default ))
      }
   }

   private class ParamAudioImpl( val name: String, val spec: ParamSpec, val default: Float )
   extends ProcParamAudio {
      def ar : GE = {
         val p             = Proc.local
         val pb            = ProcGraphBuilder.local
         implicit val tx   = pb.tx
         val c             = p.control( name )
         pb.includeParam( this )
         c.mapping.map( m => {
            val outBus = m.output // .get
            require( outBus.rate == `arate` )
            // stupidly we have two arguments...
            name.ar( default, List.fill( outBus.numChannels - 1 )( default ): _* )
         }).getOrElse( name.ar( default ))
      }
   }

   private class ControlImpl( val proc: Impl, param: ProcParamFloat, val _rate: Rate )
   extends ProcControl {
      ctrl =>

      private var valueRef = Ref.withObserver( default ) { (oldV, newV) =>
         if( oldV != newV ) proc.dispatchControlChange( ctrl, newV )
      }
      private val mappingRef = Ref.withObserver[ Option[ ProcControlMapping ]]( None ) { (oldM, newM) =>
         if( oldM != newM ) proc.dispatchMappingChange( ctrl, newM )
      }

      def rate = Some( _rate )
      def default : Float = param.default // .getOrElse( 0f )
      def spec : ParamSpec = param.spec
      def name : String = param.name
      def value( implicit tx: ProcTxn ) : Float = valueRef()
      def value_=( newValue: Float )( implicit tx: ProcTxn ) {
         val oldValue = valueRef.swap( newValue )
         if( oldValue != newValue ) proc.controlChanged( ctrl, newValue )
      }

      def mapping( implicit tx: ProcTxn ) : Option[ ProcControlMapping ] = mappingRef()

      def map( aout: ProcAudioOutput )( implicit tx: ProcTxn ) : ProcControlAMapping = {
//         require( aout.proc.server == proc.server ) // that should be in ~> hopefully
         require( mapping.isEmpty, "Already mapped" )
         val m = _rate match {
            case `krate` => new ControlKBusMapping( aout, ctrl )
            case `arate` => new ControlABusMapping( aout, ctrl )
            case _ => error( "Cannot map rate " + _rate )
         }
         mappingRef.set( Some( m ))
//         m.connect
//         this
         m
      }

      def canMap( aout: ProcAudioOutput )( implicit tx: ProcTxn ) : Boolean = !isMapped
   }

   private abstract class ControlBusMapping extends AbstractAudioInputImpl with ProcControlAMapping {
      def source: ProcAudioOutput
      def target: ControlImpl
      def name    = target.name + "#map"
      def proc    = target.proc
//      def connect( implicit tx: ProcTxn ) { source ~> this } // XXX NO WE DON'T NEED TO ENFORCE TOPOLOGY !!!

      val synth   = Ref[ Option[ RichSynth ]]( None )

      protected val edges = Ref( Set.empty[ ProcEdge ])
      
      def input : ProcAudioInput = this

      /**
       *    That means the mapping source bus changed.
       *    If numChannels changes we need to rebuild.
       *    Otherwise the mapping synth's "in" param
       *    needs update.
       */
      def busChanged( bus: AudioBus )( implicit tx: ProcTxn ) {
         if( verbose ) println( "IN INDEX " + proc.name + " / " + bus )
         // XXX check numChannels
         synth().foreach( _.set( true, "in" -> bus.index ))
//         indexRef.set( bus.index )
//         proc.busMapChanged( this, bus )
      }

      def play( implicit tx: ProcTxn ) {
//         require( synth().isEmpty, "Already playing" )

         val inBus   = bus.get.busOption.get
         val g       = graph( inBus )
         val rsd     = RichSynthDef( inBus.server, g )
         val rs      = rsd.play( source.proc.playGroup, List( "in" -> inBus.index ), addAfter )

         val oldSynth = synth.swap( Some( rs ))
         addOutputConsumers   // requires that synth has been assigned!
         oldSynth.foreach( _.free( true ))
      }

      def stop( implicit tx: ProcTxn ) {
         // we set synth to None first, so
         // the removeReader calls don't produce
         // unnecessary n_set messages
         synth.swap( None ).foreach( _.free( true ))
         removeOutputConsumers
      }

      protected def graph( inBus: AudioBus ) : SynthGraph
      protected def addOutputConsumers( implicit tx: ProcTxn ) : Unit
      protected def removeOutputConsumers( implicit tx: ProcTxn ) : Unit
   }
   
   object ControlKBusMapping {
      // important to choose a control name that won't conflict
      // with the proc's controls, since we now call map(a)n
      // on the main group, to allow lazy playGroup creation!
      val ctrlInName    = "$i"
      val ctrlOutName   = "$ko"
   }
   private class ControlKBusMapping( val source: ProcAudioOutput, val target: ControlImpl )
   extends ControlBusMapping {
      import ControlKBusMapping._

      override def toString = "aIn(" + proc.name + " @ " + name + ")"

      private var outputRef = Ref[ Option[ RichControlBus ]]( None )

      def output( implicit tx: ProcTxn ) = {
         outputRef().getOrElse({
            val inBus   = bus.get.busOption.get
            val res     = RichBus.control( inBus.server, inBus.numChannels )
            outputRef.set( Some( res ))
            res
         })
      }

      private val outputReader = new RichControlBus.User {
         def busChanged( bus: ControlBus )( implicit tx: ProcTxn ) {
//println( "(r)busChanged " + bus )
//            target.proc.playGroup.mapn( true, target.name -> bus )
            target.proc.group.mapn( true, target.name -> bus )
         }
      }

      private val outputWriter = new RichControlBus.User {
         def busChanged( bus: ControlBus )( implicit tx: ProcTxn ) {
//println( "(w)busChanged " + bus )
            synth().foreach( rs => {
//println( "--> " + rs )
               rs.set( true, ctrlOutName -> bus.index )
            })
         }
      }

      protected def graph( inBus: AudioBus ) = SynthGraph {
         val in      = A2K.kr( In.ar( ctrlInName.kr, inBus.numChannels ))
         val clipped = Clip.kr( in, -1, 1 )
         val out     = target.spec.map( clipped.madd( 0.5f, 0.5f ))
         Out.kr( ctrlOutName.kr, out )
      }

      protected def addOutputConsumers( implicit tx: ProcTxn ) {
         output.addReader( outputReader )
         output.addWriter( outputWriter )
      }

      protected def removeOutputConsumers( implicit tx: ProcTxn ) {
         output.removeReader( outputReader )
         output.removeWriter( outputWriter )
      }
   }

   object ControlABusMapping {
      // important to choose a control name that won't conflict
      // with the proc's controls, since we now call map(a)n
      // on the main group, to allow lazy playGroup creation!
      val ctrlInName    = "$i"
      val ctrlOutName   = "$o"
   }
   private class ControlABusMapping( val source: ProcAudioOutput, val target: ControlImpl )
   extends ControlBusMapping {
      import ControlABusMapping._
      
      override def toString = "aIn(" + proc.name + " @ " + name + ")"

      private var outputRef = Ref[ Option[ RichAudioBus ]]( None )

      def output( implicit tx: ProcTxn ) = {
         outputRef().getOrElse({
            val inBus   = bus.get.busOption.get
            val res     = RichBus.audio( inBus.server, inBus.numChannels )
            outputRef.set( Some( res ))
            res
         })
      }

      private val outputReader = new RichAudioBus.User {
         def busChanged( bus: AudioBus )( implicit tx: ProcTxn ) {
//            target.proc.playGroup.mapan( true, target.name -> bus )
            target.proc.group.mapan( true, target.name -> bus )
         }
      }

      private val outputWriter = new RichAudioBus.User {
         def busChanged( bus: AudioBus )( implicit tx: ProcTxn ) {
            synth().foreach( _.set( true, ctrlOutName -> bus.index ))
         }
      }

      protected def graph( inBus: AudioBus ) = SynthGraph {
         val in      = In.ar( ctrlInName.kr, inBus.numChannels )
         val clipped = Clip.ar( in, -1, 1 )
         val out     = target.spec.map( clipped.madd( 0.5f, 0.5f ))
         Out.ar( ctrlOutName.kr, out )
      }

      protected def addOutputConsumers( implicit tx: ProcTxn ) {
         output.addReader( outputReader )
         output.addWriter( outputWriter )
      }

      protected def removeOutputConsumers( implicit tx: ProcTxn ) {
         output.removeReader( outputReader )
         output.removeWriter( outputWriter )
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

   private class ParamAudioInputImpl( val name: String, val default: Option[ RichAudioBus ], physical: Boolean )
   extends ProcParamAudioInput {
//      def numChannels : Int = {
//         val pb   = ProcGraphBuilder.local
//         pb.includeParam( this )
////         val b    = Proc.local.getAudioBus( name )( pb.tx )
//         val b    = Proc.local.audioInput( name ) // ( pb.tx )
//         b.numChannels
//      }

      def ar : GE = {
         val p    = Proc.local
         val pb   = ProcGraphBuilder.local
         implicit val tx   = pb.tx
//         val b = try {
//            p.getAudioBus( name )
//         } catch { case e: ProcParamUnspecifiedException => {
            val ain = p.audioInput( name )
val b = ain.bus.getOrElse({
            if( ain.synthetic ) {
               val res = RichBus.audio( p.server, 1 )
               ain.bus = Some( res )
               res
            } else if( physical ) {
               val res = RichBus.soundIn( p.server, 1 )
               ain.bus = Some( res )
               res
            } else pError( name ) // throw e
})
//         }}
         pb.includeParam( this ) // important: must be after ain.bus_= NO NOT TRUE - DOES NOT MATTER

         In.ar( name.kr, b.numChannels )
      }
   }

   private class ParamAudioOutputImpl( val name: String, val default: Option[ RichAudioBus ], physical: Boolean )
   extends ProcParamAudioOutput {
//      def numChannels : Int = {
//         val pb   = ProcGraphBuilder.local
//         pb.includeParam( this )
//         val b    = Proc.local.getAudioBus( name )( pb.tx )
//         b.numChannels
//      }

      def ar( sig: GE ) : GE = {
         val numChannels   = sig.numOutputs
         val p             = Proc.local
         val pb            = ProcGraphBuilder.local 
         implicit val tx   = pb.tx
//         val b = try {
//            p.getAudioBus( name )
//         } catch { case e: ProcParamUnspecifiedException => {
            val aout = p.audioOutput( name )
val b = aout.bus.getOrElse({
            if( aout.synthetic ) {
               val res = RichBus.audio( p.server, numChannels )
               aout.bus = Some( res )
               res
            } else if( physical ) {
               val res = RichBus.soundOut( p.server, numChannels )
               aout.bus = Some( res )
               res
            } else pError( name ) // throw e
      })
//         }}
         pb.includeParam( this ) // important: must be after aout.bus_= NO NOT TRUE DOES NOT MATTER
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