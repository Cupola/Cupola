package de.sciss.nuages

import de.sciss.synth.proc.{DSL => PDSL, ProcTxn, ProcFactory}

object DSL {
   def ngen( name: String )( thunk: => Unit )( implicit tx: ProcTxn ) : ProcFactory = {
      val res = PDSL.gen( name )( thunk )
      Wolkenpumpe.add( res )
      res
   }
}