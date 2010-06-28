package de.sciss.nuages

import javax.swing.plaf.metal.MetalLabelUI
import javax.swing.{SwingConstants, Icon, JLabel}

/**
 *    @version 0.12, 28-Jun-10
 */
class Label( text: String, icon: Icon, horizontalAlignment: Int )
extends JLabel( text, icon, horizontalAlignment ) {
   // ---- constructor ----
   {
      setOpaque( false );
      setUI( new MetalLabelUI() )
   }

   def this( text: String ) {
      this( text, null, SwingConstants.LEADING )
   }

   def this( icon: Icon ) {
      this( "", icon, SwingConstants.LEADING )
   }

   def this() {
       this( "", null, SwingConstants.LEADING )
   }
}