package de.sciss.temporal

trait Player {
   def tick( pos: Long ) : Unit
   def play( pos: Long ) : Unit
   def stop : Unit
}

trait Playable {
   def play( pos: Long ): Unit
   def discarded: Unit
}