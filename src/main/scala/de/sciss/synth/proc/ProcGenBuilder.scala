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

import de.sciss.synth.{ Buffer, ControlSetMap, GE, GraphBuilder, SingleControlSetMap, SC, Server,
                        Synth, SynthDef }
import SC._
import de.sciss.synth.io.AudioFile
import de.sciss.scalaosc.{ OSCBundle, OSCMessage }

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
         val b = new BufferImpl( name, Proc.local.getString( p.name ))
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

   private class Impl( val name: String, gen: GenImpl ) extends Proc {
      private val sync           = new AnyRef
      private var stoppable : Option[ Stoppable ] = None
      private var pFloatValues   = Map[ ProcParamFloat, Float ]()
      private var pStringValues  = Map[ ProcParamString, String ]()

      def setFloat( name: String, value: Float ) : Proc = {
         sync.synchronized {
            val p = gen.params( name ).asInstanceOf[ ProcParamFloat ]
            pFloatValues += p -> value
//            stoppable.foreach( _.setNum( name, num ))
            this
         }
      }

      def setString( name: String, value: String ) : Proc = {
         sync.synchronized {
            val p = gen.params( name ).asInstanceOf[ ProcParamString ]
            pStringValues += p -> value
//            stoppable.foreach( _.setLoc( name, loc ))
            this
         }
      }

      def getFloat( name: String ) : Float = {
         sync.synchronized {
            val p = gen.params( name ).asInstanceOf[ ProcParamFloat ]
            pFloatValues.get( p ).getOrElse( p.default.getOrElse(
               error( "Param '" + name + "' has not yet been assigned ")))
         }
      }

      def getString( name: String ) : String = {
         sync.synchronized {
            val p = gen.params( name ).asInstanceOf[ ProcParamString ]
            pStringValues.get( p ).getOrElse( p.default.getOrElse(
               error( "Param '" + name + "' has not yet been assigned ")))
         }
      }

      def play : Proc = {
         sync.synchronized {
            if( stoppable.isDefined ) {
               println( "WARNING: Proc.play - '" + this+ "' already playing")
            } else {
               val res = Proc.use( this ) { gen.entry.play }
               lazy val l: AnyRef => Unit = _ match {
                  case Stoppable.Stopped => sync.synchronized {
                     res.removeListener( l )
                     if( stoppable == Some( res )) stoppable = None // XXX propagate stop?
                  }
               }
               res.addListener( l )
               stoppable = Some( res )
            }
            this
         }
      }

      def stop : Proc = {
         sync.synchronized {
            stoppable.map( s => {
               s.stop
               stoppable = None
            }) getOrElse {
               println( "WARNING: Proc.stop - '" + this+ "' not playing")
            }
            this
         }
      }

      def isPlaying = sync.synchronized { stoppable.isDefined }

      override def toString = "proc(" + name + ")"

//      def getParamValue[ T ]( p: ProcParam[ T ]) : T = gen.params( p.name ).asInstanceOf[ ProcParam[ T ]].default.get
   }

   // ---------------------------- ProcGraph implementation ----------------------------

   private class GraphImpl( val gen: BuilderImpl, thunk: => GE ) extends ProcGraph {
      def fun : GE = thunk

      def play : Stoppable = new GraphBuilderImpl( this ).play
   }

   // ---------------------------- ProcGraphBuilder implementation ----------------------------

   private class GraphBuilderImpl( graph: GraphImpl ) extends ProcGraphBuilder {
      var controls   = Set.empty[ ControlSetMap ]
      var buffers    = Set.empty[ BufferImpl ]

      def includeParam( p: ProcParam[ _ ]) {
         p match {
            case pFloat: ProcParamFloat => controls += SingleControlSetMap( pFloat.name, Proc.local.getFloat( pFloat.name ))
            case _ =>
         }
      }

      def includeBuffer( b: ProcBuffer ) {
         b match {
            case bi: BufferImpl => buffers += bi
            case _ => println( "WARNING: Currently not supporting buffer " + b )
         }
      }

      def play : Stoppable = {
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
            val rcvMsg  = df.recvMsg( synth.newMsg( df.name, args = controls.toSeq ))
            server ! OSCBundle( (bufMsgs.toSeq :+ rcvMsg): _* )
            new StoppableImpl({
//               println( "aqui" )
               synth.free
            })
         }
      }
   }

   private class StoppableImpl( thunk: => Unit ) extends Stoppable {
      def stop =  thunk
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