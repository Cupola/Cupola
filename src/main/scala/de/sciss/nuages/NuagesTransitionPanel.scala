package de.sciss.nuages

import javax.swing._
import event.{ChangeEvent, ChangeListener}
import java.awt.Color
import plaf.basic.{BasicSliderUI, BasicPanelUI}
import de.sciss.synth.proc._
import java.awt.event.{ActionListener, ActionEvent}

/**
 *    @version 0.11, 12-Jul-10
 */
class NuagesTransitionPanel( main: NuagesPanel ) extends JPanel {
   panel =>

   // ---- constructor ----
   {
      val font       = Wolkenpumpe.condensedFont.deriveFont( 15f ) // WARNING: use float argument
      val bg         = new ButtonGroup()
      val uiToggle   = new SimpleToggleButtonUI
      val box        = Box.createHorizontalBox()

      def addButton( b: AbstractButton ) {
         b.setUI( uiToggle )
         b.setFont( font )
         b.setBackground( Color.black )
         b.setForeground( Color.white )
         bg.add( b )
         box.add( b )
      }

      setUI( new BasicPanelUI() )
      setBackground( Color.black )
      val ggTypeInstant = new JToggleButton( "Inst" )
      val ggTypeGlide   = new JToggleButton( "Glide" )
      val ggTypeXFade   = new JToggleButton( "XFade" )
      addButton( ggTypeInstant )
      addButton( ggTypeGlide )
      addButton( ggTypeXFade )
      panel.setLayout( new BoxLayout( panel, BoxLayout.Y_AXIS ))
      panel.add( box )
      val ggSlider      = new JSlider( 0, 0x10000 )
      val specSlider    = ParamSpec( ggSlider.getMinimum(), ggSlider.getMaximum() )
      val specTime      = ParamSpec( 0.06, 60.0, ExpWarp )
      ggSlider.setUI( new BasicSliderUI( ggSlider ))
      ggSlider.setBackground( Color.black )
      ggSlider.setForeground( Color.white )
//      val dim           = ggSlider.getPreferredSize()
//      dim.width         = 64
//      ggSlider.setPreferredSize( dim )
      panel.add( ggSlider )

      def dispatchTransition {
         main.transition = if( ggTypeInstant.isSelected() ) {
            (_) => Instant
         } else {
            val fdt = specTime.map( specSlider.unmap( ggSlider.getValue() ))
            if( ggTypeXFade.isSelected() ) {
               XFade( _, fdt )
            } else {
               Glide( _, fdt )
            }
         }
      }

      ggSlider.addChangeListener( new ChangeListener {
         def stateChanged( e: ChangeEvent ) {
            if( !ggTypeInstant.isSelected() ) dispatchTransition
         }
      })
      val actionListener = new ActionListener {
         def actionPerformed( e: ActionEvent ) {
            dispatchTransition
         }
      }
      ggTypeInstant.addActionListener( actionListener )
      ggTypeGlide.addActionListener( actionListener )
      ggTypeXFade.addActionListener( actionListener )
   }
}