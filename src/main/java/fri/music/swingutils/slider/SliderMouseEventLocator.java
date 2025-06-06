package fri.music.swingutils.slider;

import java.awt.event.MouseEvent;
import javax.swing.JSlider;
import javax.swing.SwingConstants;
import javax.swing.plaf.basic.BasicSliderUI;

/**
 * Calculates the slider value according to a mouse-event of any kind.
 */
public class SliderMouseEventLocator
{
    /** Result of construction. */
    public final int sliderValue;
    
    public SliderMouseEventLocator(MouseEvent e) {
        final JSlider slider = (JSlider) e.getSource();
        final BasicSliderUI ui = (BasicSliderUI) slider.getUI();
        final boolean horizontal = (slider.getOrientation() != SwingConstants.VERTICAL);
        this.sliderValue = horizontal ? ui.valueForXPosition(e.getX()) : ui.valueForYPosition(e.getY());
    }
}