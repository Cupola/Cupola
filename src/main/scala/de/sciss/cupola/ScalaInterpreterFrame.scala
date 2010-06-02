package de.sciss.cupola

import java.awt.GraphicsEnvironment
import de.sciss.scalainterpreter.{ LogPane, ScalaInterpreterPane }
import de.sciss.synth.Server
import tools.nsc.Interpreter
import java.io.PrintStream
import javax.swing.{ JFrame, JSplitPane, SwingConstants, WindowConstants }
import de.sciss.synth.swing.NodeTreePanel

/**
 *    @version 0.11, 11-Apr-10
 */
class ScalaInterpreterFrame( /* s: Server, ntp: NodeTreePanel*/ )
extends JFrame( "Scala Interpreter" ) {
   val pane = new ScalaInterpreterPane
   private val sync = new AnyRef
   private var inCode: Option[ Interpreter => Unit ] = None

   // ---- constructor ----
   {
      val cp = getContentPane

      pane.initialText = pane.initialText +
"""
val g = gen( "process1" ) {
    val p1 = pFloat( "freq", ParamSpec(), Some( 882 ))

    graph {
        SinOsc.ar( p1.kr )
    }
}

val p = g.make
p.setFloat( "freq", 441 )
p.play
p.stop

val audioDir = "/Users/rutz/Desktop/Interface3/audio_work/"

val h = gen( "process2" ) {
    val p1  = pFloat( "speed", ParamSpec(), Some( 1 ))
    val p2  = pString( "path", Some( audioDir + "unused/Dienstvergehen3Splt3Hlb.aif" ))
    val b   = bufCue( "disk", p2 )

    graph {
        VDiskIn.ar( b.numChannels, b.id, p1.kr * BufRateScale.ir( b.id ), loop = 1 )
    }
}

val p = h.make
p.setString( "path", audioDir + "ZahnradGong1 den-L.aif" )
p.setFloat( "speed", 4 )
p.play
p.setFloat( "speed", 0.25f )
p.stop
p.play; p.stop    // this is safe now!

val fut = p.getFloat( "speed" )
if( fut.isSet ) println( fut() )
"""

      pane.initialCode = Some(
"""
import math._
import de.sciss.synth._
import de.sciss.synth.ugen._
import de.sciss.synth.swing._
import de.sciss.temporal._
import de.sciss.synth.proc._
import de.sciss.synth.proc.DSL._
"""
      )

      pane.bindingsCreator = Some( (in: Interpreter ) => {
         sync.synchronized {
            inCode.foreach( _.apply( in ))
         }
//         in.bind( "s", classOf[ Server ].getName, s )
//         in.bind( "ntp", classOf[ NodeTreePanel ].getName, ntp )
//         in.bind( "in", classOf[ Interpreter ].getName, in )
      })

      val lp = new LogPane
      lp.init
      pane.out = Some( lp.writer )
      Console.setOut( lp.outputStream )
      Console.setErr( lp.outputStream )
      System.setErr( new PrintStream( lp.outputStream ))

      pane.init
      val sp = new JSplitPane( SwingConstants.HORIZONTAL )
      sp.setTopComponent( pane )
      sp.setBottomComponent( lp )
      cp.add( sp )
      val b = GraphicsEnvironment.getLocalGraphicsEnvironment.getMaximumWindowBounds
      setSize( b.width / 2, b.height * 7 / 8 )
      sp.setDividerLocation( b.height * 2 / 3 )
      setLocationRelativeTo( null )
//    setLocation( x, getY )
      setDefaultCloseOperation( WindowConstants.EXIT_ON_CLOSE )
//    setVisible( true )
   }

   def withInterpreter( fun: Interpreter => Unit ) {
      sync.synchronized {
         pane.interpreter.map( fun( _ )) getOrElse {
            inCode = Some( fun )
         }
      }
   }
}