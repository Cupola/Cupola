/*
 *  DSL.scala
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

import de.sciss.synth.{ Buffer, ControlSetMap, GE, GraphBuilder, SC, Server, SingleControlSetMap, SynthDef }
import SC._
import de.sciss.scalaosc.OSCBundle

/**
 *    @version 0.11, 14-May-10
 */
object DSL {
   // ---- scope : outside ----

   def gen( name: String )( thunk: => Unit ) : ProcGen = {
      val res = ProcGenBuilder( name )( thunk )
      // res.announce
      res
   }

   // ---- scope : gen (ProcGenBuilder) ----

   def pFloat( name: String, spec: ParamSpec, default: Option[ Float ]) : ProcParamFloat =
      ProcGenBuilder.local.pFloat( name, spec, default )
   def pString( name: String, default: Option[ String ]) : ProcParamString =
      ProcGenBuilder.local.pString( name, default )
   def graph( thunk: => GE ) : ProcGraph = ProcGenBuilder.local.graph( thunk )
//   def enter( entry: ProcEntry ) : Unit = ProcGenBuilder.local.enter( entry )

   def bufCue( name: String, path: String ) : ProcBuffer =
      ProcGenBuilder.local.bufCue( name, path )
   def bufCue( name: String, p: ProcParamString ) : ProcBuffer   = ProcGenBuilder.local.bufCue( name, p )

   // ---- scope : graph (ProcGraphBuilder) ----
   
//   implicit def paramNumToGE( p: ProcParamNum ) : GE = p.embed
//   implicit def bufferToGE( b: ProcBuffer ) : GE = b.embed
}

trait ProcBuffer {
   def name : String
//   def kr : GE // = name.kr

   // ---- scope : graph (ProcGraphBuilder) ----
   
   def id : GE
   def numChannels : Int
}

trait ProcGen {
   def name : String
   def make : Proc 
}
