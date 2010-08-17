/*
 *  Filters.scala
 *  (Cupola)
 *
 *  Copyright (c) 2010 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 *
 *
 *  Changelog:
 */

package de.sciss.cupola

import collection.breakOut

/**
 * @version 0.10, 17-Aug-10
 */
object Filters {
   val color = Vector(
      SoundContext( "fl_filtm",
         SimpleFilterSettings,
      // scale   weight conc     dur         fade
         0.2, 0.7, 1.0, 0, 2, 20.0, 60.0, 10.0, 20.0, ColorField, Set.empty
      ),
      SoundContext( "fl_abovem",
         SimpleFilterSettings,
      // scale   weight conc     dur         fade
         0.3, 0.8, 1.0, 0, 2, 20.0, 60.0, 10.0, 20.0, ColorField, Set.empty
      ),
      SoundContext( "fl_filts",
         SimpleFilterSettings,
      // scale   weight conc     dur         fade
         0.7, 1.0, 1.0, 0, 2, 20.0, 60.0, 10.0, 20.0, ColorField, Set.empty
      ),
      SoundContext( "fl_aboves",
         SimpleFilterSettings,
      // scale   weight conc     dur         fade
         0.8, 1.0, 1.0, 0, 2, 20.0, 60.0, 10.0, 20.0, ColorField, Set.empty
      )
   )

   val text = Vector(
      SoundContext( "fl_verb",
         VerbFilterSettings,
      // scale   weight conc     dur         fade
         0.0, 0.5, 1.0, 0, 2, 20.0, 60.0, 10.0, 20.0, TextureField, Set.empty
      ),
      SoundContext( "fl_ahilbm",
         SimpleFilterSettings,
      // scale   weight conc     dur         fade
         0.15, 0.75, 1.0, 0, 2, 20.0, 60.0, 10.0, 20.0, TextureField, Set.empty
      ),
      SoundContext( "fl_ahilbs",
         SimpleFilterSettings,
      // scale   weight conc     dur         fade
         0.75, 1.0, 1.0, 0, 2, 20.0, 60.0, 10.0, 20.0, TextureField, Set.empty
      )
   )

   val all = color ++ text
   val map: Map[ String, SoundContext ] = all.map( c => (c.name -> c) )( breakOut )
}