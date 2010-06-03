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

/**
 *    @version 0.11, 03-Jun-10
 */
trait ProcFactoryBuilder {
   def name : String
   def pFloat( name: String, spec: ParamSpec, default: Option[ Float ]) : ProcParamFloat
   def pString( name: String, default: Option[ String ]) : ProcParamString
   def pAudioIn( name: String, default: Option[ (Int, Int) ]) : ProcParamAudioInBus
   def pAudioOut( name: String, default: Option[ (Int, Int) ]) : ProcParamAudioOutBus

   def graph( thunk: => GE ) : ProcGraph
//   def enter( entry: ProcEntry ) : Unit

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

      def pAudioIn( name: String, default: Option[ (Int, Int) ]) : ProcParamAudioInBus = {
         requireOngoing
         val p = new ParamAudioBusImpl( name, default, Proc.local.getAudioBusDirect( name ).numChannels )
         addParam( p )
         p
      }

      def pAudioOut( name: String, default: Option[ (Int, Int) ]) : ProcParamAudioOutBus = {
         requireOngoing
         val p = new ParamAudioBusImpl( name, default, Proc.local.getAudioBusDirect( name ).numChannels )
         addParam( p )
         p
      }

      def graph( thunk: => GE ) : ProcGraph = {
         requireOngoing
         require( graph.isEmpty, "Graph already defined" )
         val res = new GraphImpl( thunk )
         graph = Some( res )
         enter( res )
         res
      }

      def bufCue( name: String, path: String ) : ProcBuffer = {
         val b = new BufferImpl( name, path )
         addBuffer( b )
         b
      }

      def bufCue( name: String, p: ProcParamString ) : ProcBuffer = {
         val b = new BufferImpl( name, Proc.local.getStringDirect( p.name ))
         addBuffer( b )
         b
      }

       private def enter( e: ProcEntry ) {
         require( entry.isEmpty, "Entry already defined" )
         entry = Some( e )
      }

      def finish : ProcFactory = {
         requireOngoing
         finished = true
         require( entry.isDefined, "No entry point defined" )
         new FactoryImpl( name, entry.get, params )
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

   private class FactoryImpl( val name: String, val entry: ProcEntry, val params: Map[ String, ProcParam[ _ ]])
   extends ProcFactory {
      def make : Proc = new Impl( name, ProcWorldActor.default, this )

      override def toString = "gen(" + name + ")"
   }

   // ---------------------------- Proc implementation ----------------------------

   private object Impl {
      class Exec( thunk: => Unit ) { def exec = thunk }
//      case class Running( group: Group, synth: Synth ) extends Stoppable {
//
//      }
   }

   private class Impl( val name: String, wa: ProcWorldActor, fact: FactoryImpl ) extends DaemonActor with Proc {
      proc =>

      import Impl._

      start  // start myself...

//      private val sync           = new AnyRef
      private var running : Option[ ProcRunning ] = None
      private var pFloatValues      = Map.empty[ ProcParamFloat, Float ]
      private var pStringValues     = Map.empty[ ProcParamString, String ]
      private var pAudioBusValues   = Map.empty[ ProcParamAudioBus, AudioBus ]

      def server = wa.server

      def act = loop { react {
         case e: Exec => {
//            println( "Executing........." )
            try {
               e.exec
            }
            catch { case ex: ActorBodyException => ex.printStackTrace() }
//            println( ".........gnitucexE" )
         }
         case m => println( "Unknown message " + m )
      }}

      def audioInput( name: String ) : ProcAudioInput = {
         error( "Not yet implemented" )
      }

      def audioOutput( name: String ) : ProcAudioOutput = {
         error( "Not yet implemented" )
      }

      def setFloat( name: String, value: Float ) : Proc = tryExec {
         val p = fact.params( name ).asInstanceOf[ ProcParamFloat ]
         pFloatValues += p -> value
         running.foreach( _.setFloat( name, value ))
      }

      def setString( name: String, value: String ) : Proc = tryExec {
         val p = fact.params( name ).asInstanceOf[ ProcParamString ]
         pStringValues += p -> value
         running.foreach( _.setString( name, value ))
      }

      def setAudioBus( name: String, value: AudioBus ) : Proc = tryExec {
         val p = fact.params( name ).asInstanceOf[ ProcParamAudioBus ]
         pAudioBusValues += p -> value
         running.foreach( _.setAudioBus( name, value ))
      }

      def getFloat( name: String ) : Future[ Float ] = {
         proc !!( new Exec( reply( getFloatDirect( name ))), { case f: Float => f })
      }

      def getFloatDirect( name: String ) : Float = {
         val p = fact.params( name ).asInstanceOf[ ProcParamFloat ]
         pFloatValues.get( p ).getOrElse( p.default.getOrElse(
            error( "Param '" + name + "' has not yet been assigned ")))
      }

      def getString( name: String ) : Future[ String ] = {
         proc !!( new Exec( reply( getStringDirect( name ))), { case s: String => s })
      }

      def getStringDirect( name: String ) : String = {
         val p = fact.params( name ).asInstanceOf[ ProcParamString ]
         pStringValues.get( p ).getOrElse( p.default.getOrElse(
            error( "Param '" + name + "' has not yet been assigned ")))
      }

      def getAudioBus( name: String ) : Future[ AudioBus ] = {
         proc !!( new Exec( reply( getAudioBusDirect( name ))), { case ab: AudioBus => ab })
      }

      def getAudioBusDirect( name: String ) : AudioBus = {
         val p = fact.params( name ).asInstanceOf[ ProcParamAudioBus ]
         pAudioBusValues.get( p ).getOrElse( p.default.map( tup => AudioBus( server, tup._1, tup._2 )).getOrElse(
            error( "Param '" + name + "' has not yet been assigned ")))
      }

      private def await[ A ]( timeOut: Long, fut: Future[ A ])( handler: Function1[ Option[ A ], Unit ]) {
         fut.inputChannel.reactWithin( timeOut ) {
            case TIMEOUT => handler( None )
            case a       => handler( Some( a.asInstanceOf[ A ]))
         }
      }

      def play : Proc = {
         wa.transport.sched( new ProcSched {
            def play( preparePos: Long, latency: Int ) = txPlay( preparePos, latency )
            def discarded {}
         }, ProcTransport.NOW, ProcTransport.UNKNOWN_LATENCY )
         wa.transport.play
         this
      }

      private def txPlay( preparePos: Long, latency: Int ) {
         println( "txPlay " + preparePos + ", " + latency )
         exec {
            println( "execPlay " + preparePos + ", " + latency )
            if( running.isDefined ) {
               println( "WARNING: Proc.play - '" + this + "' already playing")
            } else {
               val futTx = try {
                  wa.openTx( preparePos, latency )
               }
               catch { case ex => throw new ActorBodyException( ex )}
               await( 5000L, futTx ) {
                  case None => println( "timeout!" )
                  case Some( tx ) => {
                     println( "play : tx opened" )
                     try {
                        val run = Proc.use( proc ) { fact.entry.play( tx )}
                        lazy val l: Model.Listener = {
                           case ProcRunning.Stopped => {
                              run.removeListener( l )
                              exec( if( running == Some( run )) running = None ) // XXX propagate stop?
                           }
                           case m => println( "Ooooops : " + m )
                        }
                        run.addListener( l )
                        running = Some( run )
                        tx.commit
                     }
                     catch { case ex => {
//                        e.printStackTrace()
                        tx.abort
                        throw new ActorBodyException( ex )
                     }}
                  }
               }
            }
         }
      }

      private def exec( thunk: => Unit ) : Proc = {
         proc ! new Exec( thunk )
         this
      }

      private def tryExec( thunk: => Unit ) : Proc = {
         proc ! new Exec( try { thunk } catch { case ex => throw new ActorBodyException( ex )})
         this
      }

      def stop : Proc = exec {
         running.foreach( r => {
            try { r.stop } catch { case ex => ex.printStackTrace() }
//               stoppable = None
         })
      }

      def isPlaying : Future[ Boolean ] = {
         proc !!( new Exec( running.isDefined ), { case b: Boolean => b })
      }

      override def toString = "proc(" + name + ")"

//      def getParamValue[ T ]( p: ProcParam[ T ]) : T = gen.params( p.name ).asInstanceOf[ ProcParam[ T ]].default.get
   }

   // ---------------------------- ProcGraph implementation ----------------------------

   private class GraphImpl( thunk: => GE ) extends ProcGraph {
      def fun : GE = thunk

      def play( tx: ProcTransaction ) : ProcRunning = new GraphBuilderImpl( this ).play( tx )
   }

   // ---------------------------- ProcGraphBuilder implementation ----------------------------

   private class GraphBuilderImpl( graph: GraphImpl ) extends ProcGraphBuilder {
      var controls   = Set.empty[ ControlSetMap ]
      var buffers    = Set.empty[ BufferImpl ]

      def includeParam( p: ProcParam[ _ ]) {
         p match {
//            case pFloat: ProcParamFloat => controls += SingleControlSetMap( pFloat.name, Proc.local.getFloat( pFloat.name ))
            case pFloat: ProcParamFloat => controls += SingleControlSetMap( pFloat.name,
               Proc.local.getFloatDirect( pFloat.name ))
            case pString: ProcParamString =>
            case pAudioBus: ProcParamAudioBus => controls += SingleControlSetMap( pAudioBus.name,
               Proc.local.getAudioBusDirect( pAudioBus.name ).index )
            case x => println( "Ooops. what parameter is this? " + x ) // scalac doesn't check exhaustion...
         }
      }

      def includeBuffer( b: ProcBuffer ) {
         b match {
            case bi: BufferImpl => buffers += bi
            case _ => println( "WARNING: Currently not supporting buffer " + b )
         }
      }

      def play( tx: ProcTransaction ) : ProcRunning = {
         ProcGraphBuilder.use( this ) {
            val g          = SynthGraph.wrapOut( graph.fun, None )
            val server     = Proc.local.server
            val target     = server.defaultGroup // XXX
            val addAction  = addToHead
            val args       = controls.toSeq
            val synth      = Synth( server )
            val bufs: Seq[ RichBuffer ] = buffers.map( bi => {
               val b = Buffer( server )
               tx.addBuffer( b, bi.creationMessage( b, synth ))
            })( breakOut )
            tx.addSynth( g, synth.newMsg( _, target, args, addAction ), bufs )
//            val df   = SynthDef( graph.gen.name, g )
//            val server  = Proc.local.server
//            val synth   = Synth( server )
//            val bufMsgs = buffers.map( _.creationMessage( synth ))
//            val rcvMsg  = df.recvMsg // (  ))
//            val synthMsg = synth.newMsg( df.name, args = controls.toSeq )
//            val syncMsg = server.syncMsg
//            server !!( OSCBundle( (bufMsgs.toSeq :+ rcvMsg :+ syncMsg): _* ), {
//               case OSCSyncedMessage( syncMsg.id ) => new RunningImpl( synth, synthMsg )
//            })
            new RunningImpl( synth )
         }
      }
   }

   private class RunningImpl( synth: Synth ) extends ProcRunning {
      import ProcRunning._
      
//      var pendingControls
      synth.onEnd {
         println( "n_end : " + synth )
         dispatch( Stopped )
      }
//      synth.server ! synthNewMsg

      def stop = synth.free // XXX
      def setString( name: String, value: String ) { error( "not yet supported" )}
      def setFloat( name: String, value: Float ) { synth.set( name -> value )} // XXX
      def setAudioBus( name: String, value: AudioBus ) { error( "not yet supported" )}
   }

   // ---------------------------- ProcBuffer implementation ----------------------------

   // XXX either BufferCueImpl or put cueing information in Graph instead
   private class BufferImpl( val name: String, path: => String ) extends ProcBuffer {
      def controlName   = "buf$" + name
//      val scBuf         = Buffer( Server.default ) // XXX Server.default no good
//
//      def kr : GE = {
//         controlName.kr
//      }
//      def embed : GE = {
//         gb.controls += ControlMap( b.controlName ... )
//      }

      def creationMessage( b: Buffer, synth: Synth ): OSCMessage = {
//         val b = Buffer( synth.server )
         synth.onEnd { b.close; b.free }
         b.allocMsg( 32768, numChannels, b.cueMsg( path ))
      }

      def numChannels : Int = {
//         ProcGraphBuilder.local.includeBuffer( this )
         try {
            val spec = AudioFile.readSpec( path )
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

   private class ParamAudioBusImpl( val name: String, val default: Option[ (Int, Int) ],
                                    numChannelsThunk: => Int )
   extends ProcParamAudioInBus with ProcParamAudioOutBus {
      def numChannels : Int = numChannelsThunk
   }
}