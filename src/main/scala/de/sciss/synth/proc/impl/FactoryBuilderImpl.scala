package de.sciss.synth.proc.impl

import de.sciss.synth.proc._
import de.sciss.synth.{Rate, audio, control, GE}

class FactoryBuilderImpl( val name: String ) extends ProcFactoryBuilder {
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

   def pControl( name: String, spec: ParamSpec, default: Double ) : ProcParamControl = {
      requireOngoing
      val p = new ParamControlImpl( name, spec, default )
      addParam( p )
      p
   }

   def pAudio( name: String, spec: ParamSpec, default: Double ) : ProcParamAudio = {
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

   private def getImplicitIn : GE = Proc.local.param( "in" ).asInstanceOf[ ProcParamAudioInput ].ar
   private def getImplicitOut( sig: GE ) : GE = {
      val rate = Rate.highest( sig.outputs.map( _.rate ): _* )
      if( (rate == audio) || (rate == control) ) {
         Proc.local.param( "out" ).asInstanceOf[ ProcParamAudioOutput ].ar( sig )
      } else sig
   }

   def filter( fun: GE => Unit ) : ProcGraph = {
      val fullFun = () => fun( getImplicitIn )
      graph( fullFun, true, false )
   }

   def filterOutput( fun: GE => GE ) : ProcGraph = {
      val fullFun: Function0[ Unit ] = () => {
         val in   = getImplicitIn
         val out  = fun( in )
         getImplicitOut( out )
      }
      graph( fullFun, true, true )
   }

   def synth( fun: () => Unit ) : ProcGraph = {
      graph( fun, false, false )
   }

   def synthOutput( fun: () => GE ) : ProcGraph = {
      val fullFun: Function0[ Unit ] = () => getImplicitOut( fun() )
      graph( fullFun, false, true )
   }

   private def graph( fun: () => Unit, imIn: Boolean, imOut: Boolean ) : ProcGraph = {
      requireOngoing
      require( graph.isEmpty, "Graph already defined" )
      val res = new GraphImpl( fun )
      graph = Some( res )
      enter( res )
      implicitAudioIn   = imIn
      implicitAudioOut  = imOut
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
//         pAudioIn( "in", None, true )
         pAudioIn( "in", None, false )
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