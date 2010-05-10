package de.sciss.temporal

import collection.mutable.{ PriorityQueue => MPriorityQueue }
import actors.{ Actor, TIMEOUT }

//trait Scheduled {
//   def elapsed: Double
//   def execute: Unit
//}

//case class ScheduledActor( elapsed: Double )( body: => Unit ) extends Actor {
//   def act = body
//   def execute = start
//}

//case class Scheduled( elapsed: Double, body: => Unit ) {
//   def execute = body
//}
//
//trait Clock {
//   def sched( delta: Double )( body: => Unit ) : Unit
//}
//
//class LogicalClock extends Actor {
//   val queue = new MPriorityQueue()( new Ordering[ Scheduled ] {
//       def compare( a: Scheduled, b: Scheduled ) = Ordering.Double.compare( b.elapsed, a.elapsed )
//   })
//   def sysTime        = System.currentTimeMillis
//   lazy val startTime = sysTime
//
//   def sched( delta: Double )( body: => Unit ) {
//       val t = Actor.self match {
//           case s: Scheduled  => s.elapsed + delta
//           case _             => (sysTime - startTime) * 0.001 + delta
//       }
//       this ! ScheduledActor( this, t )( body )
//   }
//
//   def act = loop {
//       reactWithin( queue.headOption.map( s => max( 0L, startTime - sysTime +
//           (s.elapsed * 1000).toLong )) getOrElse 0x1000000000L ) {
//           case s: Scheduled  => queue += s
//           case TIMEOUT       => queue.dequeue.execute
//       }
//   }
//}
