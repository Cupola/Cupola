/*
 *  ProcTransport.scala
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

import collection.mutable.{ HashSet => MHashSet, ListBuffer, PriorityQueue, Queue => MQueue }
import math._
import actors.{DaemonActor, Actor, TIMEOUT}

/**
 *    @version 0.11, 03-Jun-10
 */
trait ProcTransport {
//   def play( obj: ProcSched ) : Unit
   val sampleRate: Double
   val tickFrames: Int
   def sched( obj: ProcSched, pos: Long, latency: Int = 0 ) : Unit
   def play : Unit // = play( 0L )
   def play( offset: Long ) : Unit
   def stop : Unit
}

trait ProcSched {
   def play( preparePos: Long, latency: Int ): Unit
   def discarded: Unit
}

object ProcTransport {
   var default : ProcTransport = null

   val NOW              = -1L // special position value
   val UNKNOWN_LATENCY  = -1  // special latency value

   @volatile var verbose   = false

   def apply( sampleRate: Double, tickFrames: Int ) : ProcTransport = {
      val res = new Impl( sampleRate, tickFrames )
      res.start
      res
   }

   private class Impl( val sampleRate: Double, val tickFrames: Int )
   extends DaemonActor with ProcTransport {
      trnsp =>

//      private val players        = new ListBuffer[ Player ]
      private val milliSmpPeriod = 1000.0 / sampleRate

      def sched( obj: ProcSched, pos: Long, latency: Int ) {
         this ! Sched( obj, pos, latency )
      }

      def play : Unit = play( 0L )
      def play( offset: Long ) { trnsp ! Play( offset )}
      def stop { trnsp ! Stop }

//      def addPlayer( player: Player ) {
//         players.synchronized {
//            players += player
//         }
//      }
//
//      def removePlayer( player: Player ) {
//         players.synchronized {
//            players -= player
//         }
//      }

      private val queue = new PriorityQueue[ Executable ]()( ExecutableOrdering )

      def act = loop { react {
         case Play( offset ) => {
            if( verbose ) println( "Transport.play " + offset )
            var pos        = offset // XXX should keep pos when trnsp stopped!
            var playing    = true
            val startTime  = System.currentTimeMillis
//            foreachPlayer( _.play( offset ))
            tick( offset )
            loopWhile( playing ) {
               val exec  = queue.head // tick guarantees that there is a head element
               val msecs = max( 0L, startTime - System.currentTimeMillis + ((exec.pos - offset) * milliSmpPeriod).toLong )
               if( verbose ) println( "Transport.wait " + msecs )
               reactWithin( msecs ) {
                  case Sched( obj, playPos, latency ) => {
                     if( verbose ) println( "Transport.sched " + playPos + ", " + latency )
                     val latency0 = if( latency != UNKNOWN_LATENCY ) latency else 0
                     val playPos0 = if( playPos != NOW ) playPos else {
                        max( pos, min( exec.pos, ((System.currentTimeMillis - startTime) * sampleRate).toLong + offset ))
                           + latency0
                     }
                     schedExec( obj, playPos0 - latency0, latency )
                  }
                  case TIMEOUT => {
                     val exec = queue.dequeue
                     pos = exec.pos
                     if( verbose ) println( "Transport.exec " + pos )
                     exec.execute
                  }
                  case Stop => {
                     if( verbose ) println( "Transport.stop" )
                     playing = false
                     queue.foreach( _.discarded )         // XXX no !
                     queue.clear                          // XXX no !
//                     foreachPlayer( _.stop )
                  }
                  case Play( _ ) => println( "WARNING: Transport already playing" )
                  case msg => println( "WARNING: Transport received unknown message " + msg )
               }
            }
         }
         case Sched( obj, playPos, latency ) => {
            if( verbose ) println( "Transport.sched " + playPos + ", " + latency )
            val latency0 = if( latency != UNKNOWN_LATENCY ) latency else 0
            val playPos0 = if( playPos != NOW ) playPos else 0L + latency0 // XXX should keep pos when trnsp stopped!
            schedExec( obj, playPos0 - latency0, latency )
         }
         case Stop => println( "WARNING: Transport already stopped" )
         case msg => println( "WARNING: Transport received unknown message " + msg )
      }}

      private def schedExec( obj: ProcSched, execPos: Long, latency: Int ) {
         queue += Execute( obj, execPos, latency )
      }


//      private def foreachPlayer( fun: Player => Unit ) {
//         players.synchronized {
//            players.foreach( p => try {
//               fun( p )
//            } catch { case e => e.printStackTrace })
//         }
//      }

      private def tick( pos: Long ) {
//         foreachPlayer( _.tick( pos ))
         queue enqueue Tick( pos + tickFrames )
      }

      // pos is play-pos
      private case class Sched( obj: ProcSched, pos: Long, latency: Int )
      private case object Stop
      private case class Play( offset: Long )

      private abstract sealed class Executable {
         def pos: Long
         def execute: Unit
         def discarded: Unit
      }

      private object ExecutableOrdering extends Ordering[ Executable ] {
         def compare( a: Executable, b: Executable ) = Ordering.Long.compare( b.pos, a.pos )
      }

      private case class Tick( pos: Long ) extends Executable {
         def execute = tick( pos )
         def discarded {}
      }

      // pos is exec-pos (play-pos minus latency)
      private case class Execute( obj: ProcSched, pos: Long, latency: Int )
      extends Executable {
         def execute    = obj.play( pos, latency )
         def discarded  = obj.discarded
      }
   }
}