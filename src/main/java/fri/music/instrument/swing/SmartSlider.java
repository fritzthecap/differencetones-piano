package fri.music.instrument.swing;

import java.awt.Dimension;
import javax.swing.JSlider;

public class SmartSlider extends JSlider
{
    public SmartSlider() {
        super();
    }
    
    @Override
    public Dimension getMaximumSize() {
        return getPreferredSize();
    }
}