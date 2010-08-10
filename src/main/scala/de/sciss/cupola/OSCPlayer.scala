package de.sciss.cupola

import java.nio.ByteBuffer
import java.util.{Timer, TimerTask}
import java.io.{EOFException, RandomAccessFile, File}
import de.sciss.osc.{OSCBundle, OSCPacket, OSCPacketCodec}

class OSCPlayer( file: File, codec: OSCPacketCodec ) {
   player =>

   private val raf   = new RandomAccessFile( file, "r" )
   private val ch    = raf.getChannel()
   private val bb    = ByteBuffer.allocate( 8192 )
   private val sync  = new AnyRef
   private val timer = new Timer()

   var action: OSCPacket => Unit = p => ()

   private var nextBundle: OSCBundle = null
   private var startTime = 0L

//   def add( p: OSCPacket ) {
//      sync.synchronized {
//         bb.clear()
//         p.encode( codec, bb )
//         bb.flip()
//         raf.writeInt( bb.limit() )
//         ch.write( bb )
//      }
//   }

   def start {
      sync.synchronized {
         raf.seek( 0L )
         nextBundle = readBundle
         startTime  = OSCBundle.timetagToMillis( nextBundle.timetag )
         sched( 0L )
      }
   }

   def stop {
      sync.synchronized { timer.cancel() }
   }

   private def sched( dt: Long ) {
println( "DT = " + dt )
      timer.schedule( new TimerTask {
         def run = sync.synchronized {
            try {
               var delta = 0L
               do {
                  action( nextBundle )
                  nextBundle = readBundle // may throw EOF
                  delta = math.max( 0L, OSCBundle.timetagToMillis( nextBundle.timetag ) - startTime )
               } while( delta == 0L )
               sched( delta )
            }
            catch {
               case eof: EOFException => // stop
               case e => e.printStackTrace()
            }
         }
      }, dt )
   }

   private def readBundle : OSCBundle = {
      val size = raf.readInt()
      bb.rewind().limit( size )
      ch.read( bb )
      bb.flip()
      codec.decode( bb ) match {
         case b: OSCBundle => b
         case _ => error( "Expecting OSC bundles" )
      }
   }

   def close {
      sync.synchronized { stop; raf.close() }
   }
}