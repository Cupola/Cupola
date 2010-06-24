package de.sciss.cupola

import de.sciss.scalainterpreter.{ LogPane, ScalaInterpreterPane }
import de.sciss.synth.Server
import tools.nsc.Interpreter
import java.io.PrintStream
import de.sciss.synth.swing.NodeTreePanel
import java.awt.event.KeyEvent
import java.awt.{Toolkit, GraphicsEnvironment}
import javax.swing._

/**
 *    @version 0.11, 04-Jun-10
 */
class ScalaInterpreterFrame( /* s: Server, ntp: NodeTreePanel*/ )
extends JFrame( "Scala Interpreter" ) {
   val pane = new ScalaInterpreterPane
   private val sync = new AnyRef
   private var inCode: Option[ Interpreter => Unit ] = None

   private val txnKeyStroke = {
      val ms = Toolkit.getDefaultToolkit.getMenuShortcutKeyMask
      KeyStroke.getKeyStroke( KeyEvent.VK_T, ms )
   }

   // ---- constructor ----
   {
      val cp = getContentPane

      pane.initialText = pane.initialText +
"""// Press '""" + KeyEvent.getKeyModifiersText( txnKeyStroke.getModifiers() ) + " + " +
      KeyEvent.getKeyText( txnKeyStroke.getKeyCode() ) + """' to execute transactionally.

val g1 = gen( "process1" ) {
    val p1 = pFloat( "freq", ParamSpec(), Some( 882 ))

    graph {
        SinOsc.ar( p1.kr )
    }
}

val p1 = g1.make

p1.setFloat( "freq", 441 )
p1.play

val g2 = gen( "process2" ) {
    val p1 = pFloat( "freq", ParamSpec(), Some( 1 ))

    graph { in =>
        in * SinOsc.ar( p1.kr )
    }
}

val p2 = g2.make
p1 ~> p2
p2.play


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

// ProcTxn.atomic { implicit t =>
    p.setString( "path", audioDir + "ZahnradGong1 den-L.aif" )
    p.setFloat( "speed", 4 )
    p.play
// }

p.setFloat( "speed", 0.25f )
p.stop
p.play; p.stop    // this has problems again ...



val audioDir = "/Users/rutz/Desktop/Interface3/audio_work/"

val h = gen( "process2" ) {
    val p1  = pFloat( "speed", ParamSpec(), Some( 1 ))
    val p2  = pString( "path", Some( audioDir + "unused/Dienstvergehen3Splt3Hlb.aif" ))
    val b   = bufCue( "disk", p2 )
    val p3  = pAudioOut( "out", None )

    graph {
        val sig = VDiskIn.ar( b.numChannels, b.id, p1.kr * BufRateScale.ir( b.id ), loop = 1 )
        p3.ar( sig )
    }
}

val i = gen( "process3" ) {
    val p1 = pFloat( "freq", ParamSpec(), Some( 100 ))
    val p2 = pAudioIn( "in", None )

    graph {
        FreqShift.ar( p2.ar, p1.kr )
    }
}

val p1 = h.make
val p2 = i.make
p1 ~> p2

// XXX this should be done automatically by ~>
val b = Bus.audio( Server.default, 2 )
p1.setAudioBus( "out", b )
p2.setAudioBus( "in", b )

p1.play
p2.play

p2.setFloat( "freq", -200 )
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

      pane.customKeyMapActions += txnKeyStroke -> (() => txnExecute)

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

   private var txnCount = 0

   def txnExecute {
      pane.getSelectedTextOrCurrentLine.foreach( txt => {
         val txnId  = txnCount
         txnCount += 1
         val txnTxt = """class _txnBody""" + txnId + """( implicit t: ProcTxn ) {
""" + txt + """
}
val _txnRes""" + txnId + """ = ProcTxn.atomic( implicit t => new _txnBody""" + txnId + """ )
import _txnRes""" + txnId + """._
"""

//         println( txnTxt )
         pane.interpret( txnTxt )
      })
   }

   def withInterpreter( fun: Interpreter => Unit ) {
      sync.synchronized {
         pane.interpreter.map( fun( _ )) getOrElse {
            inCode = Some( fun )
         }
      }
   }
}