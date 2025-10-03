package fri.music.swingutils.layout;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JToolBar;

public final class ToolBarUtil
{
    /**
     * All "Help" buttons should look the same. 
     * JToolBar button look is uncontrollable, so style all Help buttons
     * (except those that are in a toolbar) like JToolBar does. 
     * @param help the button to wrap into an invisible toolbar.
     * @return the toolbar wrapper for given "Help" button.
     */
    public static JComponent getHelpButtonLookWrapper(JButton help) {
        final JToolBar helpBar = new JToolBar();
        helpBar.setFloatable(false);
        helpBar.setBorder(null);
        helpBar.add(help);
        return helpBar;
    }
    
    private ToolBarUtil() {} // do not instantiate
}
