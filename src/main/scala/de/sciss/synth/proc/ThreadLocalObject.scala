package de.sciss.synth.proc

trait ThreadLocalObject[ T <: AnyRef ] {
   private val tl = new ThreadLocal[ T ]

   def local : T = {
      val res = tl.get
      require( res != null, "Out of context access" )
      res
   }

   def use[ U ]( obj: T )( thunk: => U ) : U = {
      tl.set( obj )
      try {
         thunk
      } finally {
         tl.set( null.asInstanceOf[ T ])
      }
   }
}