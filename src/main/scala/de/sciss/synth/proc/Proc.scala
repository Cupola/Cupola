/*
 *  Proc.scala
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
import actors.Future
import de.sciss.synth.{AudioBus, Server}

/**
 *    @version 0.11, 03-Jun-10
 *
 *    @todo XXX after switching to using an actor
 *          to represent a proc, we should get rid
 *          of the thread-local variable, and replace
 *          occurences of Proc.local with Actor.self 
 */
object Proc extends ThreadLocalObject[ Proc ] {
}

trait Proc {
   def name : String
   def play : Proc
   def stop : Proc
   def isPlaying : Future[ Boolean ]
   def server : Server

//   def getParamValue[ T ]( name: String ) : T
//   def getParamValue[ T ]( p: ProcParam[ T ]) : T
//   def getParamLoc( name: String ) : FileLocation

   def getFloat( name: String ) : Future[ Float ]
   def setFloat( name: String, value: Float ) : Proc
   def getString( name: String ) : Future[ String ]
   def setString( name: String, value: String ) : Proc
   def getAudioBus( name: String ) : Future[ AudioBus ]
   def setAudioBus( name: String, value: AudioBus ) : Proc

   def audioInput( name: String ) : ProcAudioInput
   def audioOutput( name: String ) : ProcAudioOutput

   private[proc] def getFloatDirect( name: String ) : Float
   private[proc] def getStringDirect( name: String ) : String
   private[proc] def getAudioBusDirect( name: String ) : AudioBus
}