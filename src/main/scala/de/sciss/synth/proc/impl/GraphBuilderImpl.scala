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

   /*
    *    Since the mappings might create the playGroup,
    *    and Proc will have running = None _during_ this
    *    call, it is crucial not to pass in a target
    *    node, but instead have the graphbuilder determine
    *    the target!
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

         val server     = p.server
         val rsd        = RichSynthDef( server, g )
         val bufSeq     = buffers.toSeq
         val bufs       = bufSeq.map( _.create( server ))

         var setMaps    = Vector.empty[ ControlSetMap ]  // warning: rs.newMsg doesn't support setn style! XXX
//            var kbusMaps   = IQueue.empty[ ControlKBusMap ]
//            var abusMaps   = IQueue.empty[ ControlABusMap ]
         var mappings   = IQueue.empty[ ProcControlMapping ]
//Debug.breakpoint
         usedParams.foreach( _ match {
            case pFloat: ProcParamFloat => {
               val name = pFloat.name
               val ctrl = p.control( name )
               ctrl.mapping.map( m => mappings = mappings.enqueue( m )).getOrElse({
                  setMaps :+= SingleControlSetMap( name, ctrl.value )
               })
            }
            case pAudioBus: ProcParamAudioInput => {
               setMaps :+= SingleControlSetMap(
                  pAudioBus.name, p.audioInput( pAudioBus.name ).bus.get.busOption.get.index )
            }
            case pAudioBus: ProcParamAudioOutput => {
               setMaps :+= SingleControlSetMap(
                  pAudioBus.name, p.audioOutput( pAudioBus.name ).bus.get.busOption.get.index )
            }
            case x => println( "Ooops. what parameter is this? " + x ) // scalac doesn't check exhaustion...
         })
         // crucial here to ensure that group is
         // created and used, if we expect mappings.
         val target = if( mappings.nonEmpty ) {
//               p.playGroup
            p.playGroupOption.getOrElse( p.group )
         } else {
            p.playGroupOption.getOrElse( p.groupOption.getOrElse( RichGroup.default( server )))
         }
         val bufsZipped = bufSeq.zip( bufs )
         setMaps ++= bufsZipped.map( tup => SingleControlSetMap( tup._1.controlName, tup._2.buf.id ))
         val rs = rsd.play( target, setMaps, addToHead, bufs )
//            if( kbusMaps.nonEmpty ) rs.mapn(  true, kbusMaps: _* )
//            if( abusMaps.nonEmpty ) rs.mapan( true, abusMaps: _* )
         mappings.foreach( _.play ) // XXX where's the stop??
         bufsZipped.foreach( tup => {
            val (b, rb) = tup
            b.disposeWith( rb, rs )
         })
         new RunningGraphImpl( rs )
      }
   }
}