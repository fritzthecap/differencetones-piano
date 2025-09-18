package fri.music.swingutils.layout;

import java.awt.Dimension;
import java.awt.LayoutManager2;
import javax.swing.JPanel;

public class SmartPanel extends JPanel
{
    public SmartPanel() {
        super();
    }
    public SmartPanel(LayoutManager2 layout) {
        super(layout);
    }
    
    @Override
    public Dimension getMaximumSize() {
        return getPreferredSize();
    }
}