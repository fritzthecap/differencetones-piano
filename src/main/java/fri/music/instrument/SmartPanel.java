package fri.music.instrument;

import java.awt.Dimension;
import javax.swing.JPanel;

public class SmartPanel extends JPanel
{
    public SmartPanel() {
        super();
    }
    
    @Override
    public Dimension getMaximumSize() {
        return getPreferredSize();
    }
}