//package de.sciss.nuages
//
//import de.sciss.synth.proc.{DSL => PDSL, ProcTxn, ProcFactory}
//
///**
// *    @version 0.11, 12-Jul-10
// */
//object DSL {
//   def nproc( name: String )( thunk: => Unit )( implicit tx: ProcTxn ) : ProcFactory = {
//      val res = PDSL.proc( name )( thunk )
//      Wolkenpumpe.add( res )
//      res
//   }
//}