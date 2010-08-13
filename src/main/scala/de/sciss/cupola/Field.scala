package de.sciss.cupola

object Field {
   def all = List( ColorField, TextureField, SenseField, MasterField )
}
sealed abstract class Field { def name : String }
case object ColorField extends Field   { val name = "color" }
case object TextureField extends Field { val name = "text" }
case object SenseField extends Field   { val name = "sense" }
case object MasterField extends Field  { val name = "master" }
