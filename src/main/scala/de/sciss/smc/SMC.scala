package de.sciss.smc

import de.sciss.synth.swing.{NodeTreePanel, ServerStatusPanel}
import de.sciss.synth.{BootingServer, Server}
import de.sciss.synth.proc.ProcDemiurg
import de.sciss.nuages.NuagesFrame
import java.io.RandomAccessFile
import javax.swing.WindowConstants
import de.sciss.freesound.SampleInfoCache
import de.sciss.freesound.swing.{SearchResultFrame, SearchQueryFrame, LoginFrame}
import de.sciss.synth.io.AudioFile

/**
 * Created by IntelliJ IDEA.
 * User: rutz
 * Date: 17.07.2010
 * Time: 23:21:48
 * To change this template use File | Settings | File Templates.
 */

object SMC {
   val BASE_PATH  = "/Users/rutz/Desktop/freesound/"
   val AUTO_LOGIN = true 

   @volatile var s: Server       = _
   @volatile var booting: BootingServer = _

   val support = new REPLSupport
   
   def run {
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
      booting = Server.boot()
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

            // freesound
            val cred  = new RandomAccessFile( BASE_PATH + "cred.txt", "r" )
            val credL = cred.readLine().split( ":" )
            cred.close()
            initFreesound( credL( 0 ), credL( 1 ))
         }
      }
      Runtime.getRuntime().addShutdownHook( new Thread { override def run = shutDown })
      booting.start
   }

   private def initNuages {
      val f = new NuagesFrame( s )
      f.setSize( 640, 480 )
      f.setVisible( true )
      support.nuages = f
      SMCNuages.init( s )
   }

   private def initFreesound( username: String, password: String ) {
      val icache = Some( SampleInfoCache.persistent( BASE_PATH + "infos" ))
      val downloadPath = Some( BASE_PATH + "samples" )
      val f = new LoginFrame()
      f.setDefaultCloseOperation( WindowConstants.EXIT_ON_CLOSE )
      f.setLocation( 40, 40 )
      f.setVisible( true )
      f.username  = username
      f.password_=( password )
      f.addListener {
         case LoginFrame.LoggedIn( login ) => {
            support.login = login
            val sqf = new SearchQueryFrame( f, login )
            sqf.setLocationRelativeTo( null )
            sqf.setLocation( sqf.getX(), 40 )
            sqf.setVisible( true )
            sqf.addListener {
               case SearchQueryFrame.NewSearch( idx, search ) => {
                  val title = "Freesound Search #" + idx + " (" + {
                        val kw = search.options.keyword
                        if( kw.size < 24 ) kw else kw.take( 23 ) + "…"
                     } + ")"
                  val srf = new SearchResultFrame( sqf, search, title, icache, downloadPath )
                  srf.setLocationRelativeTo( null )
                  srf.setVisible( true )
                  var checked = Set.empty[ String ]
                  srf.addListener {
                     case SearchResultFrame.SelectionChanged( sel @ _* ) => {
println( "SELECTION = " + sel )
                        val pathO = sel.headOption.flatMap( _.download.flatMap( path => {
println( "AQUI " + path )
                           if( checked.contains( path )) Some( path ) else {
                              try {
                                 val spec = AudioFile.readSpec( path )
                                 if( spec.numChannels > 0 && spec.numChannels <= 2 ) {
                                    checked += path
                                    Some( path )
                                 } else None
                              } catch { case e => None }
                           }
                        }))
println( "PATHO = " + pathO )
                        SMCNuages.freesoundFile = pathO
                     }
                  }
               }
            }
         }
      }
      if( AUTO_LOGIN ) f.performLogin
   }

   private def shutDown { // sync.synchronized { }
      if( (s != null) && (s.condition != Server.Offline) ) {
         s.quit
         s = null
      }
      if( booting != null ) {
         booting.abort
         booting = null
      }
   }
}