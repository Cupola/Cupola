package de.sciss.synth.proc.impl

import de.sciss.synth.proc._
import de.sciss.synth._
import collection.breakOut
import collection.immutable.{ Queue => IQueue }

/**
 *    @version 0.11, 12-Jul-10
 */
class GraphBuilderImpl( graph: GraphImpl, val tx: ProcTxn ) extends ProcGraphBuilder {
//      var controls   = Set.empty[ ControlSetMap ]
   var usedParams = Set.empty[ ProcParam ]
   var buffers    = Set.empty[ BufferImpl ]

   // XXX we might not even need this, at least for graphs
   // as the included parameters are directly accessible
   // from the SynthGraph !
   def includeParam( param: ProcParam ) {
      usedParams += param
   }

   def includeBuffer( b: ProcBuffer ) {
      b match {
         case bi: BufferImpl => buffers += bi
         case _ => println( "WARNING: Currently not supporting buffer " + b )  // XXX
      }
   }

   /**
    */
   def play : ProcRunning = {
      implicit val t = tx
      ProcGraphBuilder.use( this ) {
         val p             = Proc.local
         val g             = SynthGraph( graph.fun() )

         val server        = p.server
         val rsd           = RichSynthDef( server, g )
         val bufSeq        = buffers.toSeq
         val bufs          = bufSeq.map( _.create( server ))

         var setMaps       = Vector.empty[ ControlSetMap ]  // warning: rs.newMsg doesn't support setn style! XXX
         var accessories   = IQueue.empty[ TxnPlayer ]
         var audioInputs   = IQueue.empty[ (RichAudioBus, String) ]
         var audioOutputs  = IQueue.empty[ (RichAudioBus, String) ]
         var useCore       = false

         usedParams.foreach( _ match {
            case pFloat: ProcParamFloat => {
               val name = pFloat.name
               val cv    = p.control( name ).cv
               cv.mapping match {
                  case None => setMaps :+= SingleControlSetMap( name, cv.target.toFloat )
                  case Some( m ) => {
                     accessories = accessories.enqueue( m )
                     useCore     = true
                  }
               }
            }
//            case pAudioBus: ProcParamAudioInput => {
//               setMaps :+= SingleControlSetMap(
//                  pAudioBus.name, p.audioInput( pAudioBus.name ).bus.get.busOption.get.index )
//            }
            case pAudioBus: ProcParamAudioInput => {
               val b       = p.audioInput( pAudioBus.name )
               accessories = accessories.enqueue( b )
               audioInputs = audioInputs enqueue (b.bus.get -> pAudioBus.name)
            }
//            case pAudioBus: ProcParamAudioOutput => {
//               setMaps :+= SingleControlSetMap(
//                  pAudioBus.name, p.audioOutput( pAudioBus.name ).bus.get.busOption.get.index )
//            }
            case pAudioBus: ProcParamAudioOutput => {
               val b       = p.audioOutput( pAudioBus.name )
               accessories  = accessories.enqueue( b )
               audioOutputs = audioOutputs enqueue (b.bus.get -> pAudioBus.name)
            }
            case x => println( "Ooops. what parameter is this? " + x ) // scalac doesn't check exhaustion...
         })

         val (target, addAction) = p.runningTarget( false )
         val bufsZipped = bufSeq.zip( bufs )
         setMaps ++= bufsZipped.map( tup => SingleControlSetMap( tup._1.controlName, tup._2.buf.id ))
         val rs = rsd.play( target, setMaps, addAction, bufs )

         accessories.foreach( _.play ) // stop is in RunningGraphImpl
         var busMap: Map[ String, AudioBusNodeSetter ] =
                    audioInputs.map(  tup => tup._2 -> rs.read(  tup ))( breakOut )
         busMap ++= audioOutputs.map( tup => tup._2 -> rs.write( tup ))

println( "play " + p.name + " ; busses = " + busMap )

         bufsZipped.foreach( tup => {
            val (b, rb) = tup
            b.disposeWith( rb, rs )        // XXX should also go in RunningGraphImpl
         })
         new RunningGraphImpl( rs, accessories, busMap )
      }
   }
}