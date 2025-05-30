package fri.music.instrument.swing.slider;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import javax.swing.JSlider;
import javax.swing.SwingUtilities;

/**
 * Moves the slider knob to the current mouse click location
 * when it was a right mouse click.
 */
public class SliderRightClickMouseAdapter extends MouseAdapter
{
    @Override
    public void mouseClicked(MouseEvent event) {
        final JSlider slider = (JSlider) event.getSource();
        if (slider.isEnabled() && SwingUtilities.isRightMouseButton(event))
            slider.setValue(new SliderMouseEventLocator(event).sliderValue);
    }
}