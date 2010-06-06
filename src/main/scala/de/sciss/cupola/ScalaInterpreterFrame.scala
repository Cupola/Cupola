package de.sciss.cupola

import java.awt.GraphicsEnvironment
import de.sciss.scalainterpreter.{ LogPane, ScalaInterpreterPane }
import de.sciss.synth.Server
import tools.nsc.Interpreter
import java.io.PrintStream
import javax.swing.{ JFrame, JSplitPane, SwingConstants, WindowConstants }
import de.sciss.synth.swing.NodeTreePanel

/**
 *    @version 0.11, 04-Jun-10
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


import edu.stanford.ppl.ccstm._
val x = Ref( "Initial" )
class Test extends actors.Actor {
    def act {
        loop {
            ActorSTM.atomic { implicit t =>
                x.set( "Trying" )
                println( "enter sleep" )
                Thread.sleep( 10000 )
                println( "exit sleep" )
                val p = ActorSTM.pause
                ActorSTM.react {
                    case "Fail" => {
                        try {
                            p.resume { implicit t =>
                                println( "x is " + x() )
                                error( "aborting txn" )
                            }
                        } catch { case x => x.printStackTrace() }
                    }
                    case ("Succeed", v: String) => {
                        println( "x is " + x() )
                        x.set( v )
                        println( "commiting txn" )
                        p.resume( _ => () )
                    }
                }
            }
        }
    }
}

val test = new Test
test.start

x.single()  // --> Initial
test ! "Fail"
x.single()  // --> Initial
test ! ("Succeed", "Final")
x.single()  // --> Final
// why doesn't this block?
actors.Actor.actor { STM.atomic { implicit t => println( "Concurrent try..." ); x.set( "CC" ); println( "CC done" )}}


trait MaybeBound[T] { def apply() : T; def set( v: T ) : Unit }
class MaybeRef[T]( r: Ref[T] )( implicit txn: Txn ) extends MaybeBound[T] {
   def apply() = r.apply()
   def set( v: T ) = r.set( v )
}
class MaybeView[T]( r: Ref.View[T] )( implicit txn: Txn ) extends MaybeBound[T] {
   def apply() = r.apply()
   def set( v: T ) = r.set( v )
}
implicit def maybeRef[T](r: Ref[T])( implicit txn: Txn ) = new MaybeRef( r )
implicit def maybeView[T](r: Ref.View[T])( implicit txn: Txn ) = new MaybeView( r )

import edu.stanford.ppl.ccstm._
import actors.{ Actor, TIMEOUT }
import actors.Actor.{ actor => fork, loop }
import edu.stanford.ppl.ccstm.ActorSTM.{ atomic => actAtom, pause, reactWithin }
import java.io.IOException

val ref = Ref( "Initial" )
object Fork {
    def apply( name: String, delay: Int ) : Actor = fork {
        var cnt = 0
        loop {
            actAtom { implicit t =>
                t.afterCommit { t =>  println( name + " committed " + cnt )}
                t.afterRollback { t =>  println( name + " rolled back " + cnt )}
                ref.set( name + " begin " + cnt )
                println( name + " enter pause... ref=" + ref.get )
                val p = pause
                reactWithin( delay ) {
                    case TIMEOUT => try {
                        p.resume( _ => throw new IOException( name + " TIMEOUT" ))
                    } catch { case e => println( e.getMessage() )}
                    case x => println( name + " Dang!" ); p.resume { implicit t =>
                        if( t.status != Txn.Active ) println( name + " Ooooh, not active any more!" )
                        val o = ref.get
                        println( name + " enter set ref=" + o )
                        if( o != (name + " begin " + cnt) ) println( name + " ... INCONSISTENT! " + o )
                        ref.set( name + " end " + cnt )
                        cnt += 1
                    }
                }
            }

    }}
}
val a = Fork( "a", 3000 )
val b = Fork( "b", 3333 )

a ! ()
b ! ()
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