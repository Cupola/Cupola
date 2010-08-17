package de.sciss.cupola

import collection.breakOut

object Filters {
   val color = Vector(
      SoundContext( "fl_filt",
         SimpleFilterSettings,
      // scale   weight conc     dur         fade
         0.2, 0.8, 1.5, 1, 2, 20.0, 60.0, 10.0, 20.0, ColorField, Set.empty
      )
   )

   val all = color // ++ text
   val map: Map[ String, SoundContext ] = all.map( c => (c.name -> c) )( breakOut )
}