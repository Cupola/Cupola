package de.sciss.cupola

import collection.breakOut

object Material {
   val all = Vector(
      SoundContext( "tp_2aside",
         TapeSoundSettings( "2A-SideBlossCon2A-SideBloss.aif", 0.0, 1.0 ),
         0.0, 0.5, 1.0, 2, 6, Set.empty
      ),
      SoundContext( "tp_saatchi",
         TapeSoundSettings( "London100304_173556OKM_SaatchiGalleryCut.aif", 1.0, 1.0 ),
         0.0, 0.5, 1.0, 2, 6, Set.empty
      ),
      SoundContext( "tp_chicago",
         TapeSoundSettings( "ChicagoAirportGate2_090605Cut.aif", 2.0, 1.0 ),
         0.0, 0.5, 1.0, 2, 6, Set.empty
      ),
      SoundContext( "tp_1conlp",
         TapeSoundSettings( "AT_1Cut1Conlp1kLoop.aif", 2.0, 1.0 ),
         0.25, 0.75, 1.0, 2, 6, Set.empty
      ),
      SoundContext( "tp_hmssjena",
         TapeSoundSettings( "HMSS_Jena_ScissOnly_081107Cut1Loop.aif", 0.0, 1.0 ),
         0.25, 0.75, 1.0, 2, 6, Set.empty
      ),
      SoundContext( "tp_lala",
         TapeSoundSettings( "lalaConlalaQuadNoAtkSt.aif", 0.0, 1.0 ),
         0.25, 0.75, 1.0, 2, 6, Set.empty
      ),
      SoundContext( "tp_magma",
         TapeSoundSettings( "Magma1BackConMgm1F'pOpFTMagAboveIchStCutLoop.aif", 3.5, 1.0 ),
         0.5, 1.0, 1.0, 2, 6, Set.empty
      ),
      SoundContext( "tp_rc60",
         TapeSoundSettings( "rc060'nlp2kOprc060'2kWrpCutLoop.aif", -2.0, 1.0 ),
         0.5, 1.0, 1.0, 2, 6, Set.empty
      ),
      SoundContext( "tp_phylet",
         TapeSoundSettings( "PhyletischesMuseumGlass080929HPFLoop.aif", 2.0, 0.25 ),
         0.5, 1.0, 1.0, 2, 6, Set.empty
      )
   )

   val map: Map[ String, SoundContext ] = all.map( c => (c.name -> c) )( breakOut )
}