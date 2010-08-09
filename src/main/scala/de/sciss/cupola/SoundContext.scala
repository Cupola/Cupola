package de.sciss.cupola

import collection.{ Set => ISet }

case class SoundContext( proc: String,
                         scaleStart: Double, scaleStop: Double, weight: Double,
                         minConc: Int, maxConc: Int,
                         mutex: ISet[ String ])
                         