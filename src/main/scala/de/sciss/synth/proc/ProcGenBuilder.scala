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

import de.sciss.temporal.FileLocation
import de.sciss.synth.{ Constant, ControlSetMap, GE, GraphBuilder, SingleControlSetMap }

trait ProcGenBuilder {
   def name : String
   def createNum( name: String, spec: ParamSpec, default: Option[ Float ]) : ProcParamNum
   def createLoc( name: String, default: Option[ FileLocation ]) : ProcParamLoc
   def graph( thunk: => GE ) : ProcGraph
//   def enter( entry: ProcEntry ) : Unit

   def bufCue( name: String, loc: FileLocation ) : ProcBuffer
   def bufCue( name: String, p: ProcParamLoc ) : ProcBuffer

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

      def createNum( name: String, spec: ParamSpec, default: Option[ Float ]) : ProcParamNum = {
         requireOngoing
         val p = new ParamNumImpl( name, spec, default )
         addParam( p )
         p
      }

      def createLoc( name: String, default: Option[ FileLocation ]) : ProcParamLoc = {
         requireOngoing
         val p = new ParamLocImpl( name, default )
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

      def bufCue( name: String, loc: FileLocation ) : ProcBuffer = {
         val b = new BufferImpl( name, loc.uri.getPath ) // XXX resolve against file locations manager
         addBuffer( b )
         b
      }

      def bufCue( name: String, p: ProcParamLoc ) : ProcBuffer = {
         val b = new BufferImpl( name, Proc.local.getLoc( p.name ).uri.getPath )
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
   }

   // ---------------------------- Proc implementation ----------------------------

   private class Impl( val name: String, gen: GenImpl ) extends Proc {
      private val sync           = new AnyRef
      private var stoppable : Option[ Stoppable ] = None
      private var paramNumValues = Map[ ProcParamNum, Float ]()
      private var paramLocValues = Map[ ProcParamLoc, FileLocation ]()

      def setNum( name: String, num: Float ) : Proc = {
         sync.synchronized {
            val p = gen.params( name ).asInstanceOf[ ProcParamNum ]
            paramNumValues += p -> num
//            stoppable.foreach( _.setNum( name, num ))
            this
         }
      }

      def setLoc( name: String, loc: FileLocation ) : Proc = {
         sync.synchronized {
            val p = gen.params( name ).asInstanceOf[ ProcParamLoc ]
            paramLocValues += p -> loc
//            stoppable.foreach( _.setLoc( name, loc ))
            this
         }
      }

      def getNum( name: String ) : Float = {
         sync.synchronized {
            val p = gen.params( name ).asInstanceOf[ ProcParamNum ]
            paramNumValues.get( p ).getOrElse( p.default.getOrElse(
               error( "Param '" + name + "' has not yet been assigned ")))
         }
      }

      def getLoc( name: String ) : FileLocation = {
         sync.synchronized {
            val p = gen.params( name ).asInstanceOf[ ProcParamLoc ]
            paramLocValues.get( p ).getOrElse( p.default.getOrElse(
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

      def includeParam( p: ProcParam[ _ ]) {
         p match {
            case pNum: ProcParamNum => controls += SingleControlSetMap( pNum.name, Proc.local.getNum( pNum.name ))
            case _ =>
         }
      }

      def play : Stoppable = {
         ProcGraphBuilder.use( this ) {
            // XXX try to cache defs if structure does not change
            val df = GraphBuilder.wrapOut( graph.gen.name, graph.fun, None )

//            val bndl = OSCBundle(
//               gb.buffers.map( _.prepareMsg ) :: List( df.recvMsg( synth.newMsg ))
//            )
            val synth = df.play
            new StoppableImpl({
               println( "aqui" )
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

      def numChannels : Int = {
         println( "WARNING: BufferImpl : numChannels : not yet implemented" )
         1 // XXX
      }

      def id : GE = {
         println( "WARNING: BufferImpl : id : not yet implemented" )
         Constant( 0 ) // XXX
      }
   }

   // ---------------------------- ProcParam implementations ----------------------------

   private class ParamNumImpl( val name: String, val spec: ParamSpec, val default: Option[ Float ])
   extends ProcParamNum {
   }

   private class ParamLocImpl( val name: String, val default: Option[ FileLocation ])
   extends ProcParamLoc {
   }
}