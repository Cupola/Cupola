package de.sciss.cupola

import de.sciss.synth.{ GE, GraphBuilder, SC, SynthDef }
import SC._

/*

 */
object DSL {
   def gen( name: String )( thunk: => Unit ) : ProcGen = {
      val res = ProcGen( name )( thunk )
      // res.announce
      res
   }
   
   def param( name: String, spec: ParamSpec, default: Float ) : ProcParam = ProcGen.builder.param( name, spec, default )
   def graph( name: String )( thunk: => GE ) : ProcGraph = ProcGen.builder.graph( name, thunk )
   def enter( entry: ProcEntry ) : Unit = ProcGen.builder.enter( entry )

   implicit def paramToGE( p: ProcParam ) : GE = p.kr
}

trait ProcParam {
   def kr      : GE = name.kr( default )
   def mapKr   : GE = spec.map( name.kr( spec.unmap( default ))) 
   def name    : String
   def spec    : ParamSpec
   def default : Float
}

trait ProcEntry {
   def play : Unit
}

trait ProcGraph extends ProcEntry {
   def name: String
}

trait Proc {
   def name : String
   def play : Unit
//   def stop : Unit
}

trait ProcGen {
   def name : String
   def make : Proc 
}

trait ProcGenBuilder {
   def param( name: String, spec: ParamSpec, default: Float ) : ProcParam
   def graph( name: String, thunk: => GE ) : ProcGraph
   def enter( entry: ProcEntry ) : Unit

   def finish : Unit
   def build( name: String ) : ProcGen
}

object ProcGen {
   private val builders = new ThreadLocal[ ProcGenBuilder ] {
      override protected def initialValue = BuilderDummy
   }
   def builder: ProcGenBuilder = builders.get

   def apply( name: String )( thunk: => Unit ) : ProcGen = {
      val b = new BuilderImpl
      builders.set( b )
      try {
         thunk
         b.finish
         b.build( name )
      } finally {
         builders.set( BuilderDummy )
      }
   }

   private class BuilderImpl extends ProcGenBuilder {
      private var finished                   = false
      private var params                     = Map[ String, ProcParam ]()
      private var graphs                     = Map[ String, ProcGraph ]()
      private var entry: Option[ ProcEntry ] = None

      def param( name: String, spec: ParamSpec, default: Float ) : ProcParam = {
         require( !params.contains( name ), "Param name '" + name + "' already taken" )
         val res = new ParamImpl( name, spec, default )
         params += name -> res
         res
      }

      def graph( name: String, thunk: => GE ) : ProcGraph = {
         require( !graphs.contains( name ), "Graph name '" + name + "' already taken" )
         val res = new GraphImpl( GraphBuilder.wrapOut( name, thunk, Some( 0f )))
         graphs += name -> res
         res
      }

      def enter( e: ProcEntry ) {
         require( entry.isEmpty, "Entry already defined" )
         entry = Some( e )
      }

      def finish {
         require( !finished )
         finished = true
         require( entry.isDefined, "No entry point defined" )
      }

      def build( name: String ) : ProcGen = new GenImpl( name, entry.get )
   }

   private object BuilderDummy extends ProcGenBuilder {
      def param( name: String, spec: ParamSpec, default: Float ) : ProcParam = outOfContext
      def graph( name: String, thunk: => GE ) : ProcGraph = outOfContext
      def enter( e: ProcEntry ) : Unit = outOfContext
      def finish {}
      def build( name: String ) : ProcGen = outOfContext

      private def outOfContext = error( "No way josé" )
   }

   private class GenImpl( val name: String, entry: ProcEntry ) extends ProcGen {
      def make : Proc = new Impl( name, entry )
   }

   private class Impl( val name: String, entry: ProcEntry ) extends Proc {
      def play {
         entry.play
      }
   }

   private class GraphImpl( df: SynthDef ) extends ProcGraph {
      def name : String = df.name

      def play {
         df.play
      }
   }

   private class ParamImpl( val name: String, val spec: ParamSpec, val default: Float )
      extends ProcParam {
   }
}