/*
 *  FactoryBuilderImpl.scala
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
import de.sciss.synth.{Rate, audio, control, GE}

/**
 *    @version 0.11, 13-Jul-10
 */
object FactoryBuilderImpl {
   def gen( name: String ) : ProcFactoryBuilder =
      new FactoryBuilderImpl( name, ProcGen, false, true )

   def filter( name: String ) : ProcFactoryBuilder =
      new FactoryBuilderImpl( name, ProcFilter, true, true )

   def diff( name: String ) : ProcFactoryBuilder =
      new FactoryBuilderImpl( name, ProcDiff, true, false )
}

class FactoryBuilderImpl private( val name: String, val anatomy: ProcAnatomy,
                                  implicitAudioIn: Boolean, implicitAudioOut: Boolean )
extends ProcFactoryBuilder {
   private var finished                   = false
   private var paramMap                   = Map.empty[ String, ProcParam ]
   private var paramSeq                   = Vector.empty[ ProcParam ]
   private var buffers                    = Map[ String, ProcBuffer ]()
   private var graph: Option[ ProcGraph ] = None
   private var entry: Option[ ProcEntry ] = None
   private var pAudioIns                  = Vector.empty[ ProcParamAudioInput ]
   private var pAudioOuts                 = Vector.empty[ ProcParamAudioOutput ]

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

   def graphIn( fun: GE => GE ) : ProcGraph = {
      val fullFun = () => fun( getImplicitIn )
      graph( fullFun )
   }

   def graphInOut( fun: GE => GE ) : ProcGraph = {
      val fullFun = () => {
         val in   = getImplicitIn
         val out  = fun( in )
         getImplicitOut( out )
      }
      graph( fullFun )
   }

   def graphOut( fun: () => GE ) : ProcGraph = {
      val fullFun = () => getImplicitOut( fun() )
      graph( fullFun )
   }

   def graph( fun: () => GE ) : ProcGraph = {
      requireOngoing
      require( graph.isEmpty, "Graph already defined" )
      val res = new GraphImpl( fun )
      graph = Some( res )
      enter( res )
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
      new FactoryImpl( name, anatomy, entry.get, paramMap, paramSeq, pAudioIns, pAudioOuts )
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