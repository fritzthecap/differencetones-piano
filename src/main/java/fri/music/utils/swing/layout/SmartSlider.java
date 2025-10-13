package fri.music.utils.swing.layout;

import java.awt.Dimension;
import javax.swing.JSlider;

public class SmartSlider extends JSlider
{
    public SmartSlider() {
        super();
    }
    public SmartSlider(int minimum, int maximum, int current) {
        super(minimum, maximum, current);
    }
    
    @Override
    public Dimension getMaximumSize() {
        return getPreferredSize();
    }
}