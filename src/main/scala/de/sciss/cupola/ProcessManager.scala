package de.sciss.cupola

import actors.Actor
import Cupola._

class ProcessManager extends Actor {
   private var level:   Level    = UnknownLevel
   private var section: Section  = Section1

   def act = {
      Cupola ! AddListener
      Cupola ! QueryLevel
      loop { react {
         case LevelChanged( newLevel, newSection ) => {
//            println( "newLevel : " + newLevel + " / section : " + newSection )
            if( level != newLevel ) {
               levelChange( newLevel, newSection )
            } else if( section != newSection ) {
               sectionChange( newSection )
            }
         }
         case x => println( "GUI: Unknown actor message  : " + x)
      }}
   }

   private def levelChange( newLevel: Level, newSection: Section ) {
      level    = newLevel
      section  = newSection
   }

   private def sectionChange( newSection: Section ) {
      section  = newSection
   }
}