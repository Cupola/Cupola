package de.sciss.synth.proc.impl

import de.sciss.synth.proc._
import de.sciss.synth.Server
import collection.immutable.{ IndexedSeq => IIdxSeq }

class FactoryImpl( val name: String, val entry: ProcEntry,
                           val paramMap: Map[ String, ProcParam ],
                           val paramSeq: IIdxSeq[ ProcParam ],
                           val pAudioIns: IIdxSeq[ ProcParamAudioInput ],
                           val pAudioOuts: IIdxSeq[ ProcParamAudioOutput ])
extends ProcFactory {
   def make( implicit tx: ProcTxn ) : Proc = {
      val res = new Impl( this, Server.default, name )
      ProcDemiurg.addVertex( res )
      res
   }

   override def toString = "gen(" + name + ")"
}
