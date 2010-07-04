package de.sciss.synth.proc.impl

import de.sciss.synth.proc._
import de.sciss.synth._
import ugen.{Clip, In, A2K, Out}

class ControlImpl( val proc: Impl, param: ProcParamFloat, val _rate: Rate )
extends ProcControl {
   ctrl =>

   private var valueRef = Ref( default )

//      private var valueRef = Ref.withObserver( default ) { (oldV, newV) =>
//         if( oldV != newV ) proc.dispatchControlChange( ctrl, newV )
//      }
   private val mappingRef = Ref[ Option[ ProcControlMapping ]]( None )
//      private val mappingRef = Ref.withObserver[ Option[ ProcControlMapping ]]( None ) { (oldM, newM) =>
//         if( oldM != newM ) proc.dispatchMappingChange( ctrl, newM )
//      }

   def rate = Some( _rate )
   def default : Float = param.default // .getOrElse( 0f )
   def spec : ParamSpec = param.spec
   def name : String = param.name
   def value( implicit tx: ProcTxn ) : Float = valueRef()
   def value_=( newValue: Float )( implicit tx: ProcTxn ) {
      val oldValue = valueRef.swap( newValue )
      if( oldValue != newValue ) proc.controlChanged( ctrl, newValue )
   }

   def mapping( implicit tx: ProcTxn ) : Option[ ProcControlMapping ] = mappingRef()

   def map( aout: ProcAudioOutput )( implicit tx: ProcTxn ) : ProcControlAMapping = {
//         require( aout.proc.server == proc.server ) // that should be in ~> hopefully
      require( mapping.isEmpty, "Already mapped" )
      val m = _rate match {
         case `control` => new ControlKBusMapping( aout, ctrl )
         case `audio` => new ControlABusMapping( aout, ctrl )
         case _ => error( "Cannot map rate " + _rate )
      }
      m.init
      val mo = Some( m )
      mappingRef.set( mo )
      proc.controlMapped( ctrl, mo )
//         m.connect
//         this
      m
   }

   def isMapable = true
   def canMap( aout: ProcAudioOutput )( implicit tx: ProcTxn ) : Boolean = !isMapped
}

abstract class ControlBusMapping extends AbstractAudioInputImpl with ProcControlAMapping {
   def source: ProcAudioOutput
   def target: ControlImpl
   def name    = target.name + "#map"
   def proc    = target.proc
//      def connect( implicit tx: ProcTxn ) { source ~> this } // XXX NO WE DON'T NEED TO ENFORCE TOPOLOGY !!!

   lazy val edge = ProcEdge( source, this )

   val synth   = Ref[ Option[ RichSynth ]]( None )

//      protected val edges = Ref( Set.empty[ ProcEdge ])

//      def input : ProcAudioInput = this

   def init( implicit tx: ProcTxn ) {
      addEdge( edge )
   }

   /**
    *    That means the mapping source bus changed.
    *    If numChannels changes we need to rebuild.
    *    Otherwise the mapping synth's "in" param
    *    needs update.
    */
   def busChanged( bus: AudioBus )( implicit tx: ProcTxn ) {
//      if( verbose ) println( "IN INDEX " + proc.name + " / " + bus )
      // XXX check numChannels
      synth().foreach( _.set( true, "in" -> bus.index ))
//         indexRef.set( bus.index )
//         proc.busMapChanged( this, bus )
   }

   def play( implicit tx: ProcTxn ) {
//         require( synth().isEmpty, "Already playing" )

      val inBus   = bus.get.busOption.get
      val g       = graph( inBus )
      val rsd     = RichSynthDef( inBus.server, g )
      val rs      = rsd.play( source.proc.playGroup, List( "in" -> inBus.index ), addAfter )

      val oldSynth = synth.swap( Some( rs ))
      addOutputConsumers   // requires that synth has been assigned!
      oldSynth.foreach( _.free( true ))
   }

   def stop( implicit tx: ProcTxn ) {
      // we set synth to None first, so
      // the removeReader calls don't produce
      // unnecessary n_set messages
      synth.swap( None ).foreach( _.free( true ))
      removeOutputConsumers
   }

   protected def graph( inBus: AudioBus ) : SynthGraph
   protected def addOutputConsumers( implicit tx: ProcTxn ) : Unit
   protected def removeOutputConsumers( implicit tx: ProcTxn ) : Unit
}

object ControlKBusMapping {
   // important to choose a control name that won't conflict
   // with the proc's controls, since we now call map(a)n
   // on the main group, to allow lazy playGroup creation!
   val ctrlInName    = "$i"
   val ctrlOutName   = "$ko"
}
class ControlKBusMapping( val source: ProcAudioOutput, val target: ControlImpl )
extends ControlBusMapping {
   import ControlKBusMapping._

   override def toString = "aIn(" + proc.name + " @ " + name + ")"

   private var outputRef = Ref[ Option[ RichControlBus ]]( None )

   def output( implicit tx: ProcTxn ) = {
      outputRef().getOrElse({
         val inBus   = bus.get.busOption.get
         val res     = RichBus.control( inBus.server, inBus.numChannels )
         outputRef.set( Some( res ))
         res
      })
   }

   private val outputReader = new RichControlBus.User {
      def busChanged( bus: ControlBus )( implicit tx: ProcTxn ) {
//println( "(r)busChanged " + bus )
//            target.proc.playGroup.mapn( true, target.name -> bus )
         target.proc.group.mapn( true, target.name -> bus )
      }
   }

   private val outputWriter = new RichControlBus.User {
      def busChanged( bus: ControlBus )( implicit tx: ProcTxn ) {
//println( "(w)busChanged " + bus )
         synth().foreach( rs => {
//println( "--> " + rs )
            rs.set( true, ctrlOutName -> bus.index )
         })
      }
   }

   protected def graph( inBus: AudioBus ) = SynthGraph {
      val in      = A2K.kr( In.ar( ctrlInName.kr, inBus.numChannels ))
      val clipped = Clip.kr( in, -1, 1 )
      val out     = target.spec.map( clipped.madd( 0.5f, 0.5f ))
      Out.kr( ctrlOutName.kr, out )
   }

   protected def addOutputConsumers( implicit tx: ProcTxn ) {
      output.addReader( outputReader )
      output.addWriter( outputWriter )
   }

   protected def removeOutputConsumers( implicit tx: ProcTxn ) {
      output.removeReader( outputReader )
      output.removeWriter( outputWriter )
   }
}

object ControlABusMapping {
   // important to choose a control name that won't conflict
   // with the proc's controls, since we now call map(a)n
   // on the main group, to allow lazy playGroup creation!
   val ctrlInName    = "$i"
   val ctrlOutName   = "$o"
}
class ControlABusMapping( val source: ProcAudioOutput, val target: ControlImpl )
extends ControlBusMapping {
   import ControlABusMapping._

   override def toString = "aIn(" + proc.name + " @ " + name + ")"

   private var outputRef = Ref[ Option[ RichAudioBus ]]( None )

   def output( implicit tx: ProcTxn ) = {
      outputRef().getOrElse({
         val inBus   = bus.get.busOption.get
         val res     = RichBus.audio( inBus.server, inBus.numChannels )
         outputRef.set( Some( res ))
         res
      })
   }

   private val outputReader = new RichAudioBus.User {
      def busChanged( bus: AudioBus )( implicit tx: ProcTxn ) {
//            target.proc.playGroup.mapan( true, target.name -> bus )
         target.proc.group.mapan( true, target.name -> bus )
      }
   }

   private val outputWriter = new RichAudioBus.User {
      def busChanged( bus: AudioBus )( implicit tx: ProcTxn ) {
         synth().foreach( _.set( true, ctrlOutName -> bus.index ))
      }
   }

   protected def graph( inBus: AudioBus ) = SynthGraph {
      val in      = In.ar( ctrlInName.kr, inBus.numChannels )
      val clipped = Clip.ar( in, -1, 1 )
      val out     = target.spec.map( clipped.madd( 0.5f, 0.5f ))
      Out.ar( ctrlOutName.kr, out )
   }

   protected def addOutputConsumers( implicit tx: ProcTxn ) {
      output.addReader( outputReader )
      output.addWriter( outputWriter )
   }

   protected def removeOutputConsumers( implicit tx: ProcTxn ) {
      output.removeReader( outputReader )
      output.removeWriter( outputWriter )
   }
}
