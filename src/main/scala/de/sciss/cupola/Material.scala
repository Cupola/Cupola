/*
 *  Material.scala
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
 * @version 0.11, 17-Aug-10
 */
object Material {
   val color = Vector(
      SoundContext( "tp_2aside",
         TapeSoundSettings( "2A-SideBlossCon2A-SideBloss.aif", 0.0, 1.0 ),
         0.0, 0.5, 1.5, 2, 6, 45.0, 120.0, 20.0, 40.0, ColorField, Set.empty
      ),
      SoundContext( "tp_tstdrv",
         TapeSoundSettings( "TstDrv5ConEnt'Ct2Wrp2RsmpRvsLoop.aif", -1.5, 1.0 ),
         0.125, 0.625, 1.5, 2, 6, 30.0, 120.0, 20.0, 40.0, ColorField, Set.empty
      ),
      SoundContext( "tp_1conlp",
         TapeSoundSettings( "AT_1Cut1Conlp1kLoop.aif", 2.0, 1.0 ),
         0.25, 0.75, 1.5, 2, 6, 30.0, 90.0, 15.0, 30.0, ColorField, Set.empty
      ),
      SoundContext( "tp_magma",
         TapeSoundSettings( "Magma1BackConMgm1F'pOpFTMagAboveIchStCutLoop.aif", 3.5, 1.0 ),
         0.5, 1.0, 1.2, 2, 6, 30.0, 120.0, 14.0, 21.0, ColorField, Set.empty
      ),
      SoundContext( "tp_rc60",
         TapeSoundSettings( "rc060'nlp2kOprc060'2kWrpCutLoopWhite.aif", -8.0, 1.0 ),
         0.5, 1.0, 1.0, 2, 6, 30.0, 120.0, 7.0, 21.0, ColorField, Set.empty
      )
   )

   val text = Vector(
      SoundContext( "tp_saatchi",
         TapeSoundSettings( "London100304_173556OKM_SaatchiGalleryCut.aif", 1.0, 1.0 ),
         0.0, 0.5, 1.0, 3, 6, 90.0, 180.0, 7.0, 21.0, TextureField, Set.empty
      ),
      SoundContext( "tp_chicago",
         TapeSoundSettings( "ChicagoAirportGate2_090605Cut.aif", 2.0, 1.0 ),
         0.0, 0.5, 1.0, 3, 6, 30.0, 120.0, 7.0, 21.0, TextureField, Set.empty
      ),
      SoundContext( "tp_hmssjena",
         TapeSoundSettings( "HMSS_Jena_ScissOnly_081107Cut1Loop.aif", 1.5, 1.0 ),
         0.25, 0.75, 1.0, 3, 6, 90.0, 180.0, 7.0, 21.0, TextureField, Set.empty
      ),
      SoundContext( "tp_lala",
         TapeSoundSettings( "lalaConlalaQuadNoAtkSt.aif", 0.0, 1.0 ),
         0.25, 0.75, 1.0, 3, 6, 30.0, 120.0, 7.0, 21.0, TextureField, Set.empty
      ),
      SoundContext( "tp_geevor",
         TapeSoundSettings( "GeevorTinMineBirds100808c.aif", -1.5, 0.5 ),
         0.375, 0.875, 0.8, 2, 6, 30.0, 120.0, 7.0, 21.0, TextureField, Set.empty
      ),
      SoundContext( "tp_phylet",
         TapeSoundSettings( "PhyletischesMuseumGlass080929HPFLoop.aif", 2.0, 0.25 ),
         0.5, 1.0, 1.0, 3, 6, 90.0, 180.0, 7.0, 21.0, TextureField, Set.empty
      )
   )

   val all = color ++ text
   val map: Map[ String, SoundContext ] = all.map( c => (c.name -> c) )( breakOut )
}