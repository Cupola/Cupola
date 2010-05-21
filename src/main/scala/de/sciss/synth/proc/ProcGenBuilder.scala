/*
 *  ProcGenBuilder.scala
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

import de.sciss.synth.{ Buffer, ControlSetMap, GE, GraphBuilder, Group, SingleControlSetMap, SC, Server,
                        Synth, SynthDef }
import SC._
import de.sciss.synth.io.AudioFile
import de.sciss.scalaosc.{ OSCBundle, OSCMessage }
import de.sciss.synth.osc.{OSCSynthNewMessage, OSCSyncedMessage}
import actors.{TIMEOUT, Future, DaemonActor}

trait ProcGenBuilder {
   def name : String
   def pFloat( name: String, spec: ParamSpec, default: Option[ Float ]) : ProcParamFloat
   def pString( name: String, default: Option[ String ]) : ProcParamString
   def graph( thunk: => GE ) : ProcGraph
//   def enter( entry: ProcEntry ) : Unit

   def bufCue( name: String, path: String ) : ProcBuffer
   def bufCue( name: String, p: ProcParamString ) : ProcBuffer

   def finish : ProcGen
}

object ProcGenBuilder extends ThreadLocalObject[ ProcGenBuilder ] {
   def apply( name: String )( thunk: => Unit ) : ProcGen = {
      val b = new BuilderImpl( name )
      use( b ) {
         thunk
         b.finish
      }
   }

   // ---------------------------- ProcGenBuilder implementation ----------------------------

   private class BuilderImpl( val name: String ) extends ProcGenBuilder {
      private var finished                   = false
      private var params                     = Map[ String, ProcParam[ _ ]]()
      private var buffers                    = Map[ String, ProcBuffer ]()
      private var graph: Option[ ProcGraph ] = None
      private var entry: Option[ ProcEntry ] = None

      @inline private def requireOngoing = require( !finished, "ProcGen build has finished" )

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

      def graph( thunk: => GE ) : ProcGraph = {
         requireOngoing
         require( graph.isEmpty, "Graph already defined" )
         val res = new GraphImpl( this, thunk )
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
         val b = new BufferImpl( name, Proc.local.asInstanceOf[ Impl ].getStringHuschHusch( p.name ))
         addBuffer( b )
         b
      }

       private def enter( e: ProcEntry ) {
         require( entry.isEmpty, "Entry already defined" )
         entry = Some( e )
      }

      def finish : ProcGen = {
         requireOngoing
         finished = true
         require( entry.isDefined, "No entry point defined" )
         new GenImpl( name, entry.get, params )
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

//   private object BuilderDummy extends ProcGenBuilder {
//      def param( name: String, spec: ParamSpec, default: Float ) : ProcParam = outOfContext
//      def graph( name: String, thunk: => GE ) : ProcGraph = outOfContext
//      def enter( e: ProcEntry ) : Unit = outOfContext
//      def finish {}
//      def build( name: String ) : ProcGen = outOfContext
//
//      private def outOfContext = error( "No way josé" )
//   }

   // ---------------------------- ProcGen implementation ----------------------------

   private class GenImpl( val name: String, val entry: ProcEntry, val params: Map[ String, ProcParam[ _ ]])
   extends ProcGen {
      def make : Proc = new Impl( name, this )

      override def toString = "gen(" + name + ")"
   }

   // ---------------------------- Proc implementation ----------------------------

   private object Impl {
      class Exec( thunk: => Unit ) { def exec = thunk }
//      case class Running( group: Group, synth: Synth ) extends Stoppable {
//
//      }
   }

   private class Impl( val name: String, gen: GenImpl ) extends DaemonActor with Proc {
      proc =>

      import Impl._

      start  // start myself...

//      private val sync           = new AnyRef
      private var running : Option[ ProcRunning ] = None
      private var pFloatValues   = Map.empty[ ProcParamFloat, Float ]
      private var pStringValues  = Map.empty[ ProcParamString, String ]

      def act = loop { react {
         case e: Exec => {
//            println( "Executing........." )
            e.exec
//            println( ".........gnitucexE" )
         }
         case m => println( "Unknown message " + m )
      }}

      def setFloat( name: String, value: Float ) : Proc = exec {
         val p = gen.params( name ).asInstanceOf[ ProcParamFloat ]
         pFloatValues += p -> value
         running.foreach( _.setFloat( name, value ))
      }

      def setString( name: String, value: String ) : Proc = exec {
         val p = gen.params( name ).asInstanceOf[ ProcParamString ]
         pStringValues += p -> value
         running.foreach( _.setString( name, value ))
      }

      def getFloat( name: String ) : Future[ Float ] = {
         proc !!( new Exec( reply( getFloatHuschHusch( name ))), { case f: Float => f })
      }

      def getFloatHuschHusch( name: String ) : Float = {
         val p = gen.params( name ).asInstanceOf[ ProcParamFloat ]
         pFloatValues.get( p ).getOrElse( p.default.getOrElse(
            error( "Param '" + name + "' has not yet been assigned ")))
      }

      def getString( name: String ) : Future[ String ] = {
         proc !!( new Exec( reply( getStringHuschHusch( name ))), { case s: String => s })
      }

      def getStringHuschHusch( name: String ) : String = {
         val p = gen.params( name ).asInstanceOf[ ProcParamString ]
         pStringValues.get( p ).getOrElse( p.default.getOrElse(
            error( "Param '" + name + "' has not yet been assigned ")))
      }

      private def await[ A ]( timeOut: Long, fut: Future[ A ])( handler: PartialFunction[ Option[ A ], Unit ]) {
         fut.inputChannel.reactWithin( timeOut ) {
            case TIMEOUT => handler( None )
            case a       => handler( Some( a.asInstanceOf[ A ]))
         }
      }

      def play : Proc = {
         exec {
            if( running.isDefined ) {
               println( "WARNING: Proc.play - '" + this + "' already playing")
            } else {
               val futPlay = Proc.use( proc ) { gen.entry.play }
//               futPlay.inputChannel.reactWithin( 5000L ) {}
               await( 5000L, futPlay ) {
                  case Some( run ) => {
//println( "PREPARED!" )
                     lazy val l: AnyRef => Unit = _ match {
                        case ProcRunning.Stopped => {
      //                     println( "---1" )
                           run.removeListener( l )
                           proc ! new Exec( if( running == Some( run )) running = None ) // XXX propagate stop?
                        }
                        case m => println( "Ooooops : " + m )
                     }
                     run.addListener( l )
                     running = Some( run )
                  }
                  case None => println( "timeout!" )
               }
            }
         }
      }

      private def exec( thunk: => Unit ) : Proc = {
         proc ! new Exec( thunk )
         this
      }

      def stop : Proc = exec {
         running.foreach( r => {
            r.stop
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

   private class GraphImpl( val gen: BuilderImpl, thunk: => GE ) extends ProcGraph {
      def fun : GE = thunk

      def play : Future[ ProcRunning ] = new GraphBuilderImpl( this ).play
   }

   // ---------------------------- ProcGraphBuilder implementation ----------------------------

   private class GraphBuilderImpl( graph: GraphImpl ) extends ProcGraphBuilder {
      var controls   = Set.empty[ ControlSetMap ]
      var buffers    = Set.empty[ BufferImpl ]

      def includeParam( p: ProcParam[ _ ]) {
         p match {
//            case pFloat: ProcParamFloat => controls += SingleControlSetMap( pFloat.name, Proc.local.getFloat( pFloat.name ))
            case pFloat: ProcParamFloat => controls += SingleControlSetMap( pFloat.name,
               Proc.local.asInstanceOf[ Impl ].getFloatHuschHusch( pFloat.name )) // XXX should use self instead of Proc.local
            case _ =>
         }
      }

      def includeBuffer( b: ProcBuffer ) {
         b match {
            case bi: BufferImpl => buffers += bi
            case _ => println( "WARNING: Currently not supporting buffer " + b )
         }
      }

      def play : Future[ ProcRunning ] = {
         ProcGraphBuilder.use( this ) {
            // XXX try to cache defs if structure does not change
            val g    = GraphBuilder.wrapOut( graph.fun, None )
            val df   = SynthDef( graph.gen.name, g )

//            val bndl = OSCBundle(
//               gb.buffers.map( _.prepareMsg ) :: List( df.recvMsg( synth.newMsg ))
//            )
            val server  = Server.default // XXX
            val synth   = Synth( server )
            val bufMsgs = buffers.map( _.creationMessage( synth ))
            val rcvMsg  = df.recvMsg // (  ))
            val synthMsg = synth.newMsg( df.name, args = controls.toSeq )
            val syncMsg = server.syncMsg
            server !!( OSCBundle( (bufMsgs.toSeq :+ rcvMsg :+ syncMsg): _* ), {
               case OSCSyncedMessage( syncMsg.id ) => new RunningImpl( synth, synthMsg )
            })
         }
      }
   }

   // XXX synth not guaranteed to exist since spawned asynchronously...
   private class RunningImpl( synth: Synth, synthNewMsg: OSCSynthNewMessage ) extends ProcRunning {
      import ProcRunning._
      
//      var pendingControls
      synth.onEnd { /* println( "ENDO" );*/ dispatch( Stopped )}
      synth.server ! synthNewMsg

      def stop = synth.free // XXX
      def setString( name: String, value: String ) {}
      def setFloat( name: String, value: Float ) { synth.set( name -> value )} // XXX
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

      def creationMessage( synth: Synth ): OSCMessage = {
         val b = Buffer( synth.server )
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
            1  // XXX what should we do?
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
}