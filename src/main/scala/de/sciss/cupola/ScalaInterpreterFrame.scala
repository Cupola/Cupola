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
class ScalaInterpreterFrame( support: REPLSupport /* s: Server, ntp: NodeTreePanel*/ )
extends JFrame( "Scala Interpreter" ) {
   val pane = new ScalaInterpreterPane
//   private val sync = new AnyRef
//   private var inCode: Option[ Interpreter => Unit ] = None
//   private var interpreter: Option[ Interpreter ] = None

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

s.dumpOSC(1)

val f = new de.sciss.nuages.NuagesFrame( s )
f.setSize( 200, 200 )
f.setVisible( true )

val g1 = ngen( "Sine" ) {
    val p1 = pFloat( "freq", ParamSpec(), Some( 882 ))

    graph { SinOsc.ar( p1.kr )}
}

val p1 = g1.make

p1.setFloat( "freq", 441 )
p1.play

val g2 = ngen( "Mod" ) {
    val p1 = pFloat( "freq", ParamSpec(), Some( 1 ))

    graph { _ * SinOsc.ar( p1.kr )}
}

val p2 = g2.make
p1 ~> p2
p2.play

val g3 = ngen( "Pan" ) {
    val p1 = pFloat( "freq", ParamSpec(), Some( 1 ))

    graph { in =>
        Pan2.ar( Mix( in ), SinOsc.ar( p1.kr ))
    }
}

val p3 = g3.make
p2 ~> p3
p3.play

///////

val f = new javax.swing.JFrame( "Freq" )
val slid = new javax.swing.JSlider()
f.getContentPane.add( slid )
slid.addChangeListener( new javax.swing.event.ChangeListener {
    def stateChanged( e: javax.swing.event.ChangeEvent ) {
        val freq = (slid.getValue() + 20).midicps
        ProcTxn.atomic { implicit t => p1.setFloat( "freq", freq )}
    }
})
f.setResizable( false )
f.pack
f.setVisible( true )

///////

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

val h = gen( "disk" ) {
    val p1  = pFloat( "speed", ParamSpec(), Some( 1 ))
    val p2  = pString( "path", Some( audioDir + "unused/Dienstvergehen3Splt3Hlb.aif" ))
    val b   = bufCue( "disk", p2 )

    graph {
        VDiskIn.ar( b.numChannels, b.id, p1.kr * BufRateScale.ir( b.id ), loop = 1 )
    }
}

val i = gen( "freqshift" ) {
    val p1 = pFloat( "freq", ParamSpec(), Some( 100 ))

    graph { in =>
        FreqShift.ar( in, p1.kr )
    }
}

val j = gen( "pan" ) {
    val p1 = pFloat( "pan", ParamSpec(), Some( 0.0 ))

    graph { in =>
        Pan2.ar( Mix( in ), p1.kr )
    }
}

val p1 = h.make
val p2 = i.make
val p3 = j.make
p1 ~> p2; p2 ~> p3

p1.play; p2.play; p3.play

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
import de.sciss.nuages.DSL._
import support._
"""
      )

      pane.bindingsCreator = Some( (in: Interpreter ) => {
//         sync.synchronized {
//            interpreter = Some( in )
//println( "bindingsCreator " + inCode.isDefined )
//            inCode.foreach( _.apply( in ))
//         }
         in.bind( "support", classOf[ REPLSupport ].getName, support )
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

//   def withInterpreter( fun: Interpreter => Unit ) {
//      sync.synchronized {
//println( "withInterpreter " + interpreter.isDefined )
//         interpreter.map( fun( _ )) getOrElse {
//            inCode = Some( fun )
//         }
//      }
//   }
}