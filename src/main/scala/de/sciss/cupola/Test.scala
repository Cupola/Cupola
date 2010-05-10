package de.sciss.cupola

import de.sciss.temporal._
import DomainSpecificLanguage._
import collection.mutable.{ HashMap => MHashMap, ListBuffer }
import de.sciss.synth._
import de.sciss.synth.ugen._
import de.sciss.trees.{ Interval => TInterval, LongManager, ManagedLong, Rect, RTree, Shaped }
import SC._
import math._
import de.sciss.scalaosc.OSCBundle

object Test {
   val server     = Cupola.s
   val sampleRate = server.sampleRate
   val tickFrames = sampleRate.toLong
   val bufLatency = (0.5 * sampleRate).toLong // generous...
   val srvLatency = (0.1 * sampleRate).toLong
   val t          = new BasicTransport( sampleRate, tickFrames )
   val milliSmpPeriod = 1000.0 / sampleRate

   def run {
      SynthDef.recv( server, "disk1" ) {
         val buf = "bufID".ir
         val sig = VDiskIn.ar( 1, buf, BufRateScale.ir( buf ))
         Line.kr( 0, 0, "dur".ir, freeSelf )
         Out.ar( "out".kr, List( sig, sig ))
      }

      audioFileLocation( "/Users/rutz/Desktop/Interface3/audio_work/" )
      val afe = try {
         audioFile( "BetonPoch-L.aif" )
      } catch { case e => { e.printStackTrace; throw e }}
      afe.use
      rootContainer
      val afrh = audioFileRegion( offset = 0.secs, interval = 4.secs :< 10.secs )
      val p    = new AudioFilePlayer( rootContainer )
      t.start
      t.addPlayer( p )
      t.play
   }

//   trait Playing {
//      def stop : Unit
//   }

   class AudioFilePlayer( r: RegionLike ) extends Player {
      private var group: Option[ Group ]  = None
//    private val virtual                 = new RegionList
      private val actual                  = new RegionList
      private val state                   = new MHashMap[ ActualRegion, AudioFileRegionState ]
      private var playTime                = 0L

      // ---- constructor ----
      {
         // build actual list (we don't support virtual at the moment)
         val iter = r match {
            case c: ContainerLike => c.iterator
            case _ => List( r )
         }
//       actual.add( iter.map( SampledRegion.actual( _ )).collect({ case Some( r ) => r }): _* )
         iter.foreach( ActualRegion.actual( _ ).foreach( actual.add( _)))
         println( "ACTUAL " + actual )
      }

      def play( pos: Long ) {
         println( "PLAY " + pos )
         playTime = System.currentTimeMillis
         group = Some( Group.head( server.defaultGroup ))
         check( pos, pos )
      }

      def stop {
         group.foreach( g => {
            g.free
            group = None
         })
         state.synchronized {
//            state.values.foreach( _.stop )
            state.clear
         }
      }

      def tick( pos: Long ) {
         check( pos, pos + tickFrames )
      }

      private def check( sched: Long, pos: Long ) {
         actual.visitRange( Span( pos, pos + tickFrames ))( ar => if( !state.contains( ar )) ar match {
            case aafr: ActualAudioFileRegion => {
               val next = AudioFileRegionSchedBuffer( aafr )
               state.synchronized { state += aafr -> next }
               t.sched( sched, next )
            }
            case _ =>
         })
      }

      private sealed abstract class AudioFileRegionState extends Playable {
         def aafr: ActualAudioFileRegion
         def discarded { state.synchronized { state.remove( aafr )}}
      }

      private case class AudioFileRegionSchedBuffer( aafr: ActualAudioFileRegion )
      extends AudioFileRegionState {
         afrs =>

         def play( pos: Long ) {
            println( "ALLOC BUFFER" )
            val nextSched  = max( pos + bufLatency, aafr.span.start - srvLatency )
            val cueOffset  = aafr.offset + (nextSched + srvLatency) - aafr.span.start
            if( nextSched < aafr.span.stop ) { // we might need to skip to short spans
               Buffer.cue( server, aafr.path, cueOffset.toInt, completion = action( b => {
                  state.synchronized {
                     if( state.contains( aafr )) {
                        println( "<ACCEPTED>: ALLOC BUFFER" )
                        val next = AudioFileRegionPlay( aafr, b )
                        state += aafr -> next // overwrites ourself!
                        t.sched( nextSched, next )
                     } else { // transport was stopped in the mean time
                        println( "<DISCARDED>: ALLOC BUFFER" )
                        b.close( b.freeMsg )
                     }
                  }
               }))
            }
         }
      }

      private case class AudioFileRegionPlay( aafr: ActualAudioFileRegion, b: Buffer )
      extends AudioFileRegionState {
         def play( pos: Long ) {
            println( "PLAY SYNTH" )
            state.synchronized {
               if( state.contains( aafr )) {
                  println( "<ACCEPTED>: PLAY SYNTH" )
                  val synth = Synth( server )
                  synth.onEnd {
                     b.close( b.freeMsg )
                     state.synchronized { state -= aafr }
                  }
                  val pos2 = pos + srvLatency
                  val dur = (aafr.span.stop - pos2) / sampleRate
                  server ! OSCBundle.millis( playTime + (pos2 * milliSmpPeriod).toLong,
                     synth.newMsg( "disk1", server, List( "bufID" -> b.id, "dur" -> dur )))
               } else { // transport was stopped in the mean time
                  println( "<DISCARDED>: PLAY SYNTH" )
                  b.close( b.freeMsg )
               }
            }
         }
      }
   }

   class PlayableStop extends Playable {
      def play( pos: Long ) { t.stop }
      def discarded {}
   }

   object ActualRegion {
      def actual( r: RegionLike ) : Option[ ActualRegion ] = {
         val ival    = r.interval.fixed
         val startP   = ival.start
         val durP     = ival.dur
         (startP, durP) match {
            case (PeriodConst( startS ), PeriodConst( durS )) => {
               val span = Span( (startS * sampleRate).toLong, ((startS + durS) * sampleRate).toLong )
               r match {
                  case afr: AudioFileRegion  => (afr.offset.fixed, afr.audioFile.path) match {
                     case (PeriodConst( offsetS ), Some( file )) => {
                        val offset = (offsetS * afr.audioFile.sampleRate).toLong
                        Some( ActualAudioFileRegion( afr, span, offset, file.getAbsolutePath ))
                     }
                     case _ => None
                  }
                  case c: ContainerLike => Some( ActualContainer( c, span ))
                  case _  => None
               }
            }
            case _ => None
         }
      }
   }
   trait ActualRegion { def r: RegionLike; def span: Span }
   case class ActualContainer( r: ContainerLike, span: Span ) extends ActualRegion
   case class ActualAudioFileRegion( r: AudioFileRegion, span: Span, offset: Long, path: String ) extends ActualRegion

   class RegionList {
      implicit private def numberView( num: Long ) = new ManagedLong( num )
      implicit private val numberManager           = LongManager
      private type LongRect                        = Rect[ Long ]
      private type LongInterval                    = TInterval[ Long ]
      private val tree                             = new RTree[ Long, Stored ]( 1 )

      def add( ars: ActualRegion* ) {
         if( ars.isEmpty ) return
         ars.foreach( ar => tree.insert( Stored( ar )))
//       val modSpan = rs.reduceLeftOption( (r1, r2) => r1.span.unite( r2.span ))
//       modSpan.foreach( span => dispatch( RegionsAdded( modSpan, rs: _* )))
      }

      def remove( ars: ActualRegion* ) {
         ars.foreach( ar => tree.remove( Stored( ar )))
//       val modSpan = rs.reduceLeftOption( (r1, r2) => r1.span.unite( r2.span ))
//       modSpan.foreach( span => dispatch( RegionsRemoved( modSpan, rs: _* )))
      }

      def visitRange( span: Span )( f: (ActualRegion) => Unit ) {
        tree.findOverlapping( spanToRect( span ), (s: Stored) => {
          f( s.ar )
        })
      }

      private def spanToRect( span: Span ) =
        new LongRect( Vector( new LongInterval( span.start, span.stop )))

      // adaptor to RTree
      private case class Stored( val ar: ActualRegion ) extends Shaped[ Long ] {
        val shape = spanToRect( ar.span )
      }
   }
}