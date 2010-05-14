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
class ScalaInterpreterFrame( s: Server, ntp: NodeTreePanel )
extends JFrame( "Scala Interpreter" ) {

   // ---- constructor ----
   {
      val cp = getContentPane
      val ip = new ScalaInterpreterPane

      ip.initialText = ip.initialText +
"""
import de.sciss.cupola._
import DSL._

val g = gen( "process1" ) {
    val p1 = param( "freq", new ParamSpec, 880 )

    enter( graph( "main" ) {
        SinOsc.ar( p1 )
    })
}

val p = g.make
p.play
p.stop
"""

      ip.initialCode = Some(
"""
import math._
import de.sciss.synth._
import de.sciss.synth.SC._
import de.sciss.synth.ugen._
import de.sciss.synth.swing._
import de.sciss.temporal._
"""
      )

      ip.bindingsCreator = Some( (in: Interpreter ) => {
         in.bind( "s", classOf[ Server ].getName, s )
//         in.bind( "ntp", classOf[ NodeTreePanel ].getName, ntp )
         in.bind( "in", classOf[ Interpreter ].getName, in )
      })

      val lp = new LogPane
      lp.init
      ip.out = Some( lp.writer )
      Console.setOut( lp.outputStream )
      Console.setErr( lp.outputStream )
      System.setErr( new PrintStream( lp.outputStream ))

      ip.init
      val sp = new JSplitPane( SwingConstants.HORIZONTAL )
      sp.setTopComponent( ip )
      sp.setBottomComponent( lp )
      cp.add( sp )
      val b = GraphicsEnvironment.getLocalGraphicsEnvironment.getMaximumWindowBounds
      setSize( b.width / 2, b.height * 7 / 8 )
      sp.setDividerLocation( b.height * 2 / 3 )
      setLocationRelativeTo( null )
//      setLocation( x, getY )
      setDefaultCloseOperation( WindowConstants.EXIT_ON_CLOSE )
//      setVisible( true )
   }
}