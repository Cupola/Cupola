/*
 *  Cupola.scala
 *  (Cupola)
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

package de.sciss.cupola

import de.sciss.synth.swing.{ NodeTreePanel, ServerStatusPanel }
import actors.{ Actor, DaemonActor, OutputChannel }
import collection.mutable.{ HashSet => MHashSet }
import java.awt.{GraphicsEnvironment, EventQueue}
import de.sciss.synth._
import de.sciss.nuages.{NuagesFrame, NuagesConfig}
import java.io.RandomAccessFile
import proc.{ DSL, ProcDemiurg, ProcTxn, Ref, TxnModel }
import DSL._
import de.sciss.osc._
import java.net.{InetSocketAddress, SocketAddress}

//case class CupolaUpdate( stage: Option[ (Level, Section) ])
case class CupolaUpdate( stage: Option[ Double ])

/**
 *    @version 0.12, 01-Aug-10
 */
object Cupola /* extends Actor */ extends TxnModel[ CupolaUpdate ] {
//   import Actor._

   type Update    = CupolaUpdate
   type Listener  = L

//   // messages received by this object
//   case object Run
//   case object Quit
//   case object AddListener
//   case object RemoveListener
//   case object QueryLevel

//   // messages sent out by this object to listeners
//   case class LevelChanged( newLevel: Level, newSection: Section )

   val BASE_PATH           = "/Users/rutz/Desktop/Cupola/"
   val AUTO_LOGIN          = true
   val NUAGES_ANTIALIAS    = false
   val INTERNAL_AUDIO      = false
   val MASTER_NUMCHANNELS  = 2 // 4
   val MASTER_OFFSET       = 0
   val MIC_OFFSET          = 0
   val FREESOUND_OFFLINE   = true
   val TRACKING_PORT       = 1201
   val TRACKING_PROTO      = TCP
   val TRACKING_LOOP       = true
   var masterBus : AudioBus = null

   lazy val SCREEN_BOUNDS =
         GraphicsEnvironment.getLocalGraphicsEnvironment.getDefaultScreenDevice.getDefaultConfiguration.getBounds

   private var vis: TrackingVis = null
   @volatile private var trackingDefeated = false

   private var stageRef: Ref[ Option[ Double ]] = Ref( None ) // Ref[ (Level, Section) ]( UnknownLevel -> Section1 )
   private val trackingAddr      = new InetSocketAddress( "127.0.0.1", TRACKING_PORT )
   private val tracking          = {
      val res = OSCClient( TRACKING_PROTO, 0, TRACKING_LOOP, OSCTrackingCodec )
      res.action = messageReceived
      res.target = trackingAddr
      res.start
      res
//      val res = JOSCServer.newUsing( TRACKING_PROTO, TRACKING_PORT, TRACKING_LOOP )
//      res.addOSCListener( new JOSCListener {
//         def messageReceived( jmsg: JOSCMessage, addr: SocketAddress, time: Long ) {
//            val args = (0 until jmsg.getArgCount()) map { i => jmsg.getArg( i ) /* match {
//               case f: java.lang.Float    => f.floatValue
//               case i: java.lang.Integer  => i.intValue
//               case x => x
//            } */
//            }
//            val msg  = OSCMessage( jmsg.getName(), args: _* )
//            Cupola.messageReceived( msg, addr, time )
//         }
//      })
//      res.dumpOSC( 1, System.out )
//      res.start()
//      res
   }
   private val trackingLocalAddr = tracking.localAddress

//   private val simulator         = {
//      val trns = OSCTransmitter( TRACKING_PROTO )
//      trns.target = tracking.getLocalAddress() // tracking.localAddress
////      trns.dumpOSC( 1, System.out )
//      trns.connect
//      trns
//   }
//   private val listeners         = new MHashSet[ OutputChannel[ Any ]]

   val options          = {
      val o = new ServerOptionsBuilder()
      if( INTERNAL_AUDIO ) {
         o.deviceNames        = Some( "Built-in Microphone" -> "Built-in Output" )
      } else {
         o.deviceName         = Some( "MOTU 828mk2" )
      }
      o.inputBusChannels   = 10
      o.outputBusChannels  = 10
      o.audioBusChannels   = 512
      o.loadSynthDefs      = false
      o.memorySize         = 65536
      o.zeroConf           = false
      o.build
   }

   val support = new REPLSupport
   val pm      = new ProcessManager

   @volatile var s: Server       = _
   @volatile var booting: BootingServer = _
   @volatile var config: NuagesConfig = _

   def main( args: Array[ String ]) {
//      s.options.programPath.value = "/Users/rutz/Documents/devel/fromSVN/SuperCollider3/common/build/scsynth"
//      s.addDoWhenBooted( this ! Run ) // important: PlainServer executes this in the OSC receiver thread, so fork!
//      start
      guiRun { init }
   }

//   private def initGUI {
//      val sspw = new ServerStatusPanel( s ).makeWindow
//      val ntp  = new NodeTreePanel( s )
//      val ntpw = ntp.makeWindow
//      ntpw.setLocation( sspw.getX, sspw.getY + sspw.getHeight + 32 )
//      val sif  = new ScalaInterpreterFrame( s, ntp )
//      sif.setLocation( sspw.getX + sspw.getWidth + 32, sif.getY )
//
//      sspw.setVisible( true )
//      ntpw.setVisible( true )
//      sif.setVisible( true )
//   }

   protected def emptyUpdate = CupolaUpdate( None )
   protected def fullUpdate( implicit tx: ProcTxn ) = CupolaUpdate( stageRef() )

   def guiRun( code: => Unit ) {
      EventQueue.invokeLater( new Runnable { def run = code })
   }

//   def simulate( msg: OSCMessage ) { simulator ! msg }
   def simulate( p: OSCPacket ) { tracking ! p }
   def simulateLocal( p: OSCPacket ) {
      simulateLocal( p, trackingLocalAddr, System.currentTimeMillis )
   }
   private def simulateLocal( p: OSCPacket, addr: SocketAddress, time: Long ) {
      p match {
         case m: OSCMessage => messageReceived( m, addr, time )
         case b: OSCBundle => b.packets.foreach( simulateLocal( _, addr, b.timetag ))
      }
   }

   def defeatTracking( defeat: Boolean ) { trackingDefeated = defeat }
   def dumpOSC( mode: Int ) { tracking.dumpIncomingOSC( mode )}

   private def messageReceived( msg: OSCMessage, addr: SocketAddress, time: Long ) {
      if( trackingDefeated && (addr != trackingLocalAddr) ) {
         println( "defeated : " + addr + " != " + trackingLocalAddr )
         return
      }
      msg match {
         case t: OSCTrackingMessage => {
            stageChange( Some( t.state / 8.0 ))
            if( vis != null ) vis.update( t )
         }
         case x => println( "Cupola: Ignoring OSC message '" + x + "'" )
      }
   }

   private def stageChange( newStage: Option[ Double ]) {
      ProcTxn.atomic { implicit tx =>
//         val newStage: (Level, Section) = Level.all( levelID ) -> Section.all( sectionID )
         val oldStage = stageRef.swap( newStage )
//println( "OLD " + oldStage + " / " + newStage )
         if( oldStage != newStage ) {
            touch
            val u = updateRef()
            updateRef.set( u.copy( stage = newStage ))
            pm.stageChange( oldStage, newStage )
         }
      }
   }

//   private def dispatch( msg: AnyRef ) {
//      listeners.foreach( _ ! msg )
//   }

   def init {
      // prevent actor starvation!!!
      // --> http://scala-programming-language.1934581.n4.nabble.com/Scala-Actors-Starvation-td2281657.html
      System.setProperty( "actors.enableForkJoin", "false" )

      val sif  = new ScalaInterpreterFrame( support /* ntp */ )
      val ssp  = new ServerStatusPanel()
      val sspw = ssp.makeWindow
      val ntp  = new NodeTreePanel()
      val ntpw = ntp.makeWindow
      ntpw.setLocation( sspw.getX, sspw.getY + sspw.getHeight + 32 )
      sspw.setVisible( true )
      ntpw.setVisible( true )
      sif.setLocation( sspw.getX + sspw.getWidth + 32, sif.getY )
      sif.setVisible( true )
      booting = Server.boot( options = options )
      booting.addListener {
         case BootingServer.Preparing( srv ) => {
            ssp.server = Some( srv )
            ntp.server = Some( srv )
         }
         case BootingServer.Running( srv ) => {
            ProcDemiurg.addServer( srv )
            s = srv
            support.s = srv

            // nuages
            initNuages
            new GUI
            vis = new TrackingVis
            tracking ! OSCMessage( "/notify", 1 )

//            // freesound
//            val cred  = new RandomAccessFile( BASE_PATH + "cred.txt", "r" )
//            val credL = cred.readLine().split( ":" )
//            cred.close()
//            initFreesound( credL( 0 ), credL( 1 ))
         }
      }
      Runtime.getRuntime().addShutdownHook( new Thread { override def run = shutDown })
      booting.start
   }

   private def initNuages {
      masterBus  = if( INTERNAL_AUDIO ) {
         new AudioBus( s, 0, 2 )
      } else {
         new AudioBus( s, MASTER_OFFSET, MASTER_NUMCHANNELS )
      }
      val soloBus    = Bus.audio( s, 2 )
      val recordPath = BASE_PATH + "rec"
      config         = NuagesConfig( s, Some( masterBus ), Some( soloBus ), Some( recordPath ))
      val f          = new NuagesFrame( config )
      f.panel.display.setHighQuality( NUAGES_ANTIALIAS )
      f.setSize( 640, 480 )
      f.setLocation( ((SCREEN_BOUNDS.width - f.getWidth()) >> 1) + 100, 10 )
      f.setVisible( true )
      support.nuages = f
      CupolaNuages.init( s, f )
   }

//   def act = loop {
//      react {
//         case msg: LevelChanged => {
//            level    = msg.newLevel
//            section  = msg.newSection
//            dispatch( msg )
//         }
//         case QueryLevel      => () // reply( LevelChanged( level, section ))
////         case Run             => run
////         case Quit            => quit
//         case AddListener     => listeners += sender
//         case RemoveListener  => listeners -= sender
//         case x               => println( "Cupola: Ignoring actor message '" + x + "'" )
//      }
//   }

   def quit { System.exit( 0 )}

   private def shutDown { // sync.synchronized { }
      if( (s != null) && (s.condition != Server.Offline) ) {
         s.quit
         s = null
      }
      if( booting != null ) {
         booting.abort
         booting = null
      }
//      simulator.dispose
      if( tracking.isActive ) {
         try {
            tracking ! OSCMessage( "/notify", 0 )
            tracking ! OSCMessage( "/dumpOSC", 0 )
         } catch { case _ => }
      }
      tracking.dispose
    }
}