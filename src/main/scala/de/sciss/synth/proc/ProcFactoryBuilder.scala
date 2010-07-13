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
import collection.immutable.{ IndexedSeq => IIdxSeq, Queue => IQueue, Seq => ISeq, Set => ISet }
import impl.FactoryBuilderImpl
import ProcTransport._
import ugen._

/**
 *    @version 0.12, 13-Jul-10
 */
trait ProcFactoryBuilder {
   def name : String
   def pControl( name: String, spec: ParamSpec, default: Double ) : ProcParamControl
   def pAudio( name: String, spec: ParamSpec, default: Double ) : ProcParamAudio
   def pString( name: String, default: Option[ String ]) : ProcParamString
   def pAudioIn( name: String, default: Option[ RichAudioBus ]) : ProcParamAudioInput
   def pAudioOut( name: String, default: Option[ RichAudioBus ]) : ProcParamAudioOutput

   def graphOut( fun: () => GE ) : ProcGraph
   def graph( fun: () => GE ) : ProcGraph
   def graphIn( fun: GE => GE ) : ProcGraph
   def graphInOut( fun: GE => GE ) : ProcGraph

   def bufCue( name: String, path: String ) : ProcBuffer
   def bufCue( name: String, p: ProcParamString ) : ProcBuffer

   def finish : ProcFactory

   def anatomy: ProcAnatomy
}

object ProcFactoryBuilder extends ThreadLocalObject[ ProcFactoryBuilder ] {
   def gen( name: String )( thunk: => Unit ) : ProcFactory =
      apply( FactoryBuilderImpl.gen( name ), thunk )

   def filter( name: String )( thunk: => Unit ) : ProcFactory =
      apply( FactoryBuilderImpl.filter( name ), thunk )

   def diff( name: String )( thunk: => Unit ) : ProcFactory =
      apply( FactoryBuilderImpl.diff( name ), thunk )

   private def apply( b: ProcFactoryBuilder, thunk: => Unit ) : ProcFactory = use( b ) {
      thunk
      b.finish
   }
}