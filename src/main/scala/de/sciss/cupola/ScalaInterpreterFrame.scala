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

val f = new de.sciss.nuages.NuagesFrame( s )
f.setSize( 640, 480 )
f.setVisible( true )

s.dumpOSC(1)

val g1 = ngen( "Sine" ) {
    val pf1 = pControl( "f1", ParamSpec( 20, 20000, ExpWarp ), 333 )
    val pf2 = pControl( "f2", ParamSpec( 20, 20000, ExpWarp ), 444 )

    graph { SinOsc.ar( List( pf1.kr, pf2.kr ))}
}

val p1 = g1.make
// xfade( 10 ) { p1( "freq" ) = (util.Random.nextInt( 100 ) + 40).midicps }

// glide( 20 ) { p1( "freq" ) = 441 }; p1.play
// glide( 20 ) { p1( "freq" ) = 111 }

//p1( "freq" ) = 441
//p1.play

val g2 = ngen( "Mod" ) {
    val p1 = pControl( "freq", ParamSpec( 0.1f, 20000, ExpWarp ), 1 )

    graph { _ * SinOsc.ar( p1.kr )}
}

val p2 = g2.make
xfade( 30 ) { p1 ~> p2; p2.play }
//p1 ~> p2
//p2.play
xfade( 30 ) { p1 ~/> p2 }

val g3 = ngen( "Pan" ) {
    val p1 = pControl( "freq", ParamSpec( 0.1f, 20000, ExpWarp ), 1 )

    graph { in =>
        Pan2.ar( Mix( in ), SinOsc.ar( p1.kr ))
    }
}

val p3 = g3.make
p2 ~> p3
p3.play

p3.stop; p2 ~> p3.control( "freq" ) // the funky shit
p3.play // cannot perform p3.stop and p3.play in one tx at the moment....

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

val h = ngen( "process2" ) {
    val p1  = pControl( "speed", ParamSpec( 0.1f, 10, ExpWarp ), 1 )
    val p2  = pString( "path", Some( audioDir + "unused/Dienstvergehen3Splt3Hlb.aif" ))
    val b   = bufCue( "disk", p2 )

    graph {
        VDiskIn.ar( b.numChannels, b.id, p1.kr * BufRateScale.ir( b.id ), loop = 1 )
    }
}

val p = h.make

// ProcTxn.atomic { implicit t =>
    p.setString( "path", audioDir + "ZahnradGong1 den-L.aif" )
    p( "speed" ) = 4
    p.play
// }

p( "speed" ) = 0.25
p.stop
p.play; p.stop    // this has problems again ...
xfade( 10 ) { p.dispose }

val audioDir = "/Users/rutz/Desktop/Interface3/audio_work/"

val h = ngen( "disk" ) {
    val p1  = pControl( "speed", ParamSpec( 0.1f, 10f, ExpWarp ), 1 )
    val p2  = pString( "path", Some( audioDir + "unused/Dienstvergehen3Splt3Hlb.aif" ))
    val b   = bufCue( "disk", p2 )

    graph {
        VDiskIn.ar( b.numChannels, b.id, p1.kr * BufRateScale.ir( b.id ), loop = 1 )
    }
}

val i = ngen( "freqshift" ) {
    val p1 = pControl( "freq", ParamSpec( -2000, 2000 ), 100 )

    graph { in => FreqShift.ar( in, p1.kr )}
}

val j = ngen( "pan" ) {
    val p1 = pControl( "pan", ParamSpec( -1, 1 ), 0 )

    graph { in => Pan2.ar( Mix( in ), p1.kr )}
}

val p1 = h.make
val p2 = i.make
val p3 = j.make
p1 ~> p2; p2 ~> p3

p1.setString( "path", audioDir + "unused/Dienstvergehen3Splt3Hlb.aif" ) // XXX defaults currenty not working 
p1.play; p2.play; p3.play

p2( "freq" ) = -200

/////

val pBub = ngen( "bubbles" ) {
   val f1  = pControl( "f1", ParamSpec( 0.1f, 10, ExpWarp ), 0.4f )
   val f2  = pControl( "f2", ParamSpec( 0.1f, 100, ExpWarp ), 8 )
   val det = pControl( "det", ParamSpec( 0.1f, 10, ExpWarp ), 0.90375f )
   graph {
      val freq2 = f2.kr
      val f = LFSaw.kr( f1.kr ).madd(24, LFSaw.kr(List( freq2, freq2 * det.kr))
         .madd(3, 80)).midicps
      CombN.ar(SinOsc.ar(f)*0.04, 0.2, 0.2, 4)
   }
}

val procBub = pBub.make

/////
val g1 = ngen( "Noise" ) {
    val p1 = pControl( "freq", ParamSpec( 20, 20000, ExpWarp ), 882 )

    graph { Resonz.ar( PinkNoise.ar, p1.kr, 0.25 )}
}

val p1 = g1.make
xfade( 10 ) { p1( "freq" ) = (util.Random.nextInt( 90 ) + 40).midicps; p1.play }
"..."
xfade( 10 ) { p1.stop }

p1.play
xfade( 10 ) { p1( "freq" ) = (util.Random.nextInt( 90 ) + 40).midicps }
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