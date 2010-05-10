package de.sciss.cupola

import java.net.SocketAddress
import actors.{ Actor, OutputChannel }
import java.awt.EventQueue
//import swing.Swing
import de.sciss.scalaosc.{ OSCMessage, OSCReceiver, OSCTransmitter }
import de.sciss.synth.{ PlainServer, Server }
import collection.mutable.{ HashSet => MHashSet }

object Cupola extends Actor {
   import Actor._

   // messages received by this object
   case object Run
   case object Quit
   case object AddListener
   case object RemoveListener
   case object QueryLevel

   // messages sent out by this object to listeners
   case class LevelChanged( newLevel: Level, newSection: Section )

   val s: Server                 = new PlainServer( "Default" )
   val trackingPort              = 0x6375
   
   private var level: Level      = UnknownLevel
   private var section: Section  = Section1
   private val tracking          = {
      val rcv = OSCReceiver( 'udp, trackingPort )
      rcv.action = messageReceived
      rcv.start
      rcv
   }
   private val simulator         = {
      val trns = OSCTransmitter( 'udp )
      trns.target = tracking.localAddress
      trns.connect
      trns
   }
   private val listeners         = new MHashSet[ OutputChannel[ Any ]]

   def main( args: Array[ String ]) {
//      s.options.programPath.value = "/Users/rutz/Documents/devel/fromSVN/SuperCollider3/common/build/scsynth"
      s.addDoWhenBooted( this ! Run ) // important: PlainServer executes this in the OSC receiver thread, so fork!
      s.boot
      start
      guiRun { new GUI }
   }

   def guiRun( code: => Unit ) {
      EventQueue.invokeLater( new Runnable { def run = code })
   }

   def simulate( msg: OSCMessage ) { simulator.send( msg )}

   private def messageReceived( msg: OSCMessage, addr: SocketAddress, time: Long ) = msg match {
      case OSCMessage( "/cupola", "state", levelID: Int, sectionID: Int ) => levelChange( levelID, sectionID )
      case x => println( "Cupola: Ignoring OSC message '" + x + "'" )
   }

   private def levelChange( levelID: Int, sectionID: Int ) {
      val newLevel   = Level.all( levelID )
      val newSection = Section.all( sectionID )
      this ! LevelChanged( newLevel, newSection )
   }

   private def dispatch( msg: AnyRef ) {
      listeners.foreach( _ ! msg )
   }

   def act = loop {
      react {
         case msg: LevelChanged => {
            level    = msg.newLevel
            section  = msg.newSection
            dispatch( msg )
         }
         case QueryLevel      => () // reply( LevelChanged( level, section ))
         case Run             => run
         case Quit            => quit
         case AddListener     => listeners += sender
         case RemoveListener  => listeners -= sender 
         case x               => println( "Cupola: Ignoring actor message '" + x + "'" )
      }
   }

   def run {
      println( "Server booted. Starting Cupola..." )
//      (new ProcessManager).start
      s.dumpOSC(1)
      Test.run
   }

   def quit {
      s.quit
      s.dispose
      tracking.dispose
      System.exit( 0 )
   }
}