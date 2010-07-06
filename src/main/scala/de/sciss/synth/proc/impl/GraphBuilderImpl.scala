package de.sciss.synth.proc.impl

import de.sciss.synth.proc._
import de.sciss.synth._
import collection.immutable.{ Queue => IQueue }

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
         val p = Proc.local
         val g = SynthGraph {
            val res1 = graph.fun
            val rate = Rate.highest( res1.outputs.map( _.rate ): _* )
            if( (rate == audio) || (rate == control) ) {
//                  val res2 = fadeTime.map( fdt => makeFadeEnv( fdt ) * res1 ) getOrElse res1
//                  val out = "out".kr
//                  if( rate == audio ) {
//                     Out.ar( out, res2 )
//                  } else {
//                     Out.kr( out, res2 )
//                  }
               (p.param( "out" ).asInstanceOf[ ProcParamAudioOutput ]).ar( res1 )
            } else res1
         }

         val server        = p.server
         val rsd           = RichSynthDef( server, g )
         val bufSeq        = buffers.toSeq
         val bufs          = bufSeq.map( _.create( server ))

         var setMaps       = Vector.empty[ ControlSetMap ]  // warning: rs.newMsg doesn't support setn style! XXX
         var accessories   = IQueue.empty[ TxnPlayer ]
         var audioInputs   = IQueue.empty[ (String, RichAudioBus) ]
         var audioOutputs  = IQueue.empty[ (String, RichAudioBus) ]
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
               audioInputs = audioInputs enqueue (pAudioBus.name -> b.bus.get)
            }
//            case pAudioBus: ProcParamAudioOutput => {
//               setMaps :+= SingleControlSetMap(
//                  pAudioBus.name, p.audioOutput( pAudioBus.name ).bus.get.busOption.get.index )
//            }
            case pAudioBus: ProcParamAudioOutput => {
               val b       = p.audioOutput( pAudioBus.name )
               accessories  = accessories.enqueue( b )
               audioOutputs = audioOutputs enqueue (pAudioBus.name -> b.bus.get)
            }
            case x => println( "Ooops. what parameter is this? " + x ) // scalac doesn't check exhaustion...
         })
         // crucial here to ensure that group is
         // created and used, if we expect mappings.
         val target = if( useCore ) { // XXX we should find another solution for this dependancy !!!
            p.coreGroup
//            p.playGroupOption.getOrElse( p.group )
         } else {
//            p.playGroupOption.getOrElse( p.groupOption.getOrElse( RichGroup.default( server )))
            p.runningGroup
         }
//         val target = p.runningGroup
         val bufsZipped = bufSeq.zip( bufs )
         setMaps ++= bufsZipped.map( tup => SingleControlSetMap( tup._1.controlName, tup._2.buf.id ))
         // XXX addToTail is non-standard, but necessary for front-group less procs which have a back-group
         val rs = rsd.play( target, setMaps, addToTail, bufs )
//            if( kbusMaps.nonEmpty ) rs.mapn(  true, kbusMaps: _* )
//            if( abusMaps.nonEmpty ) rs.mapan( true, abusMaps: _* )
         accessories.foreach( _.play ) // XXX where's the stop??
         val audioReaders = audioInputs.map(  tup => AudioBusNodeSetter.reader( tup._1, tup._2, rs ).add )
         val audioWriters = audioOutputs.map( tup => AudioBusNodeSetter.reader( tup._1, tup._2, rs ).add )
         rs.onEnd { tx0 =>
            audioReaders.foreach( _.remove )
            audioWriters.foreach( _.remove )
         }
         bufsZipped.foreach( tup => {
            val (b, rb) = tup
            b.disposeWith( rb, rs )        // XXX should also go in RunningGraphImpl
         })
         new RunningGraphImpl( rs, accessories )
      }
   }
}