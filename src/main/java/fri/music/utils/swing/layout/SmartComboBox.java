package fri.music.utils.swing.layout;

import java.awt.Dimension;
import javax.swing.JComboBox;

public class SmartComboBox extends JComboBox<String>
{
    public SmartComboBox() {
        super();
    }
    public SmartComboBox(String[] items) {
        super(items);
    }
    
    @Override
    public Dimension getMaximumSize() {
        return getPreferredSize();
    }
}