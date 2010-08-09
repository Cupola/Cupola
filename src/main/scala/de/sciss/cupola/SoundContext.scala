package de.sciss.cupola

import collection.{ Set => ISet }
import xml.Node

case class SoundContext( proc: String, settings: SoundSettings,
                         scaleStart: Double, scaleStop: Double, weight: Double,
                         minConc: Int, maxConc: Int,
                         mutex: ISet[ String ]) {
   def toXML : Node =
<context>
   <proc>{proc}</proc>
   <settings>{settings.toXML}</settings>
   <scaleStart>{scaleStart}</scaleStart>
   <scaleStop>{scaleStop}</scaleStop>
   <weight>{weight}</weight>
   <minConc>{minConc}</minConc>
   <maxConc>{maxConc}</maxConc>
   <mutex>{mutex.map( s => <entry>{s}</entry> )}</mutex>
</context>
}

sealed abstract class SoundSettings {
   def toXML : Node
}

case class TapeSoundSettings( file: String, gain: Double, speed: Double )
extends SoundSettings {
   def toXML : Node =
<tape>
   <file>{file}</file>
   <gain>{gain}</gain>
   <speed>{speed}</speed>
</tape>
}

object SoundContext {

}