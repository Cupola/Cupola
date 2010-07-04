package de.sciss.synth.proc.impl

import de.sciss.synth.GE
import de.sciss.synth.proc.{ProcRunning, ProcTxn, ProcGraph}

class GraphImpl( thunk: => GE ) extends ProcGraph {
   def fun = thunk
   def play( implicit tx: ProcTxn ) : ProcRunning =
      new GraphBuilderImpl( this, tx ).play
}