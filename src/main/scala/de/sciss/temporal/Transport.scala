package de.sciss.temporal

import collection.mutable.{ HashSet => MHashSet, ListBuffer, PriorityQueue, Queue => MQueue }
import actors.{ Actor, TIMEOUT }
import math._

trait Transport {
//   def play( obj: Playable ) : Unit
   val sampleRate: Double
   val tickFrames: Long
   def sched( pos: Long, obj: Playable ) : Unit
   def play : Unit // = play( 0L )
   def play( offset: Long ) : Unit
   def stop : Unit
}

class BasicTransport( val sampleRate: Double, val tickFrames: Long )
extends Actor with Transport {
   trnsp =>

   private val players        = new ListBuffer[ Player ]
   private val milliSmpPeriod = 1000.0 / sampleRate

   def sched( pos: Long, obj: Playable ) {
      this ! Sched( pos, obj )
   }

   def play : Unit = play( 0L )
   def play( offset: Long ) { trnsp ! Play( offset )}
   def stop { trnsp ! Stop }

   def addPlayer( player: Player ) {
      players.synchronized {
         players += player
      }
   }

   def removePlayer( player: Player ) {
      players.synchronized {
         players -= player
      }
   }

   private val queue = new PriorityQueue[ Executable ]()( ExecutableOrdering )

   def act = loop { react {
      case Play( offset ) => {
         var pos        = offset
         var playing    = true
         val startTime  = System.currentTimeMillis
         foreachPlayer( _.play( offset ))
         tick( offset )
         loopWhile( playing ) {
            val exec  = queue.head // tick guarantees that there is a head element
            val msecs = max( 0L, startTime - System.currentTimeMillis + ((exec.pos - offset) * milliSmpPeriod).toLong )
            reactWithin( msecs ) {
               case Sched( execPos, obj ) => {
                  queue += Execute( execPos, obj )
               }
               case TIMEOUT => {
                  val exec = queue.dequeue
                  pos = exec.pos
                  exec.execute
               }
               case Stop => {
                  playing = false
                  queue.foreach( _.discarded )
                  queue.clear
                  foreachPlayer( _.stop )
               }
               case Play( _ ) => println( "WARNING: Transport already playing" )
               case msg => println( "WARNING: Transport received unknown message " + msg )
            }
         }
      }
      case Sched( _, obj ) => obj.discarded
      case Stop => println( "WARNING: Transport already stopped" )
      case msg => println( "WARNING: Transport received unknown message " + msg )
   }}

   private def foreachPlayer( fun: Player => Unit ) {
      players.synchronized {
         players.foreach( p => try {
            fun( p )
         } catch { case e => e.printStackTrace })
      }
   }

   private def tick( pos: Long ) {
      foreachPlayer( _.tick( pos ))
      queue enqueue Tick( pos + tickFrames )
   }

   private case class Sched( pos: Long, obj: Playable )
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
   private case class Execute( pos: Long, obj: Playable ) extends Executable {
      def execute    = obj.play( pos )
      def discarded  = obj.discarded
   }
}