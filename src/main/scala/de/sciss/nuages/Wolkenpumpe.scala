/*
 *  Wolkenpumpe.scala
 *  (Wolkenpumpe)
 *
 *  Copyright (c) 2008-2010 Hanns Holger Rutz. All rights reserved.
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

package de.sciss.nuages

import java.awt.Font
import de.sciss.synth.Model
import de.sciss.synth.proc._
import collection.immutable.{ Set => ISet }

//case class NuagesUpdate( gensAdded: ISet[ ProcFactory ], gensRemoved: ISet[ ProcFactory ],
//                         filtersAdded: ISet[ ProcFactory ], filtersRemoved: ISet[ ProcFactory ])

/**
 *    @version 0.11, 13-Jul-10
 */
object Wolkenpumpe /* extends TxnModel[ NuagesUpdate ]*/ {
//   type Update    = NuagesUpdate
//   type Listener  = TxnModel.Listener[ Update ]

//   val gens    = Ref( Set.empty[ ProcFactory ])
//   val filters = Ref( Set.empty[ ProcFactory ])
//
//   protected def fullUpdate( implicit tx: ProcTxn ) = NuagesUpdate( gens(), Set.empty, filters(), Set.empty )
//   protected def emptyUpdate = NuagesUpdate( Set.empty, Set.empty, Set.empty, Set.empty )
//
//   def add( pf: ProcFactory )( implicit tx: ProcTxn ) {
//      touch
//      // we currently use a heuristic to determine
//      // the kind of proc factory...
//      pf.params.find( _.name == "in" ) match {
//         case Some( _: ProcParamAudioInput ) => {
//            filters.transform( _ + pf )
//            updateRef.transform( u => if( u.filtersRemoved.contains( pf )) {
//                u.copy( filtersRemoved = u.filtersRemoved - pf )
//            } else {
//                u.copy( filtersAdded = u.filtersAdded + pf )
//            })
//         }
//         case _ => {
//            gens.transform( _ + pf )
//            updateRef.transform( u => if( u.gensRemoved.contains( pf )) {
//                u.copy( gensRemoved = u.gensRemoved - pf )
//            } else {
//                u.copy( gensAdded = u.gensAdded + pf )
//            })
//         }
//      }
//   }
//
//   def remove( pf: ProcFactory )( implicit tx: ProcTxn ) {
//      val f = filters()
//      if( f.contains( pf )) {
//         touch
//         filters.set( f - pf )
//         updateRef.transform( u => if( u.filtersAdded.contains( pf )) {
//             u.copy( filtersAdded = u.filtersAdded - pf )
//         } else {
//             u.copy( filtersRemoved = u.filtersRemoved + pf )
//         })
//      } else {
//         val g = gens()
//         if( !g.contains( pf )) return
//         touch
//         gens.set( g - pf )
//         updateRef.transform( u => if( u.gensAdded.contains( pf )) {
//             u.copy( gensAdded = u.gensAdded - pf )
//         } else {
//             u.copy( gensRemoved = u.gensRemoved + pf )
//         })
//      }
//   }

   /**
    *    A condensed font for GUI usage. This is in 12 pt size,
    *    so consumers must rescale.
    */
   /*lazy val*/ var condensedFont : Font = {
// createFont doesn't properly create the spacing. fucking hell...
//      val is   = Wolkenpumpe.getClass.getResourceAsStream( "BellySansCondensed.ttf" )
//      val res  = Font.createFont( Font.TRUETYPE_FONT, is )
//      is.close
//      res
      // "SF Movie Poster Condensed"
      new Font( "BellySansCondensed", Font.PLAIN, 12 )
   }
}