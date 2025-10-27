package fri.music.utils.swing.text;

import java.awt.event.ActionEvent;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JButton;
import javax.swing.JToolBar;

/** Toolbar with basic navigation actions. */
public class HtmlHistoryToolbar extends JToolBar
{
    /** The AbstractAction key to a label for actions to be used in menus. */
    static final String MENU_ACTION_LABEL = "menuActionLabel";
    
    public final Action back;
    public final Action up;
    public final Action forward;
    public final Action reload;
    
    public HtmlHistoryToolbar(final HtmlHistoryBrowser browser) {
        this.back = new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                navigate(browser, Boolean.TRUE);
            }
        };
        back.putValue(Action.NAME, "  \u25C0  ");
        back.putValue(MENU_ACTION_LABEL, "Back");
        back.putValue(Action.SHORT_DESCRIPTION, "Go Backward in History");
        back.setEnabled(false);
        
        this.up = new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                navigate(browser, null);
            }
        };
        up.putValue(Action.NAME, "  \u25B2  ");
        up.putValue(MENU_ACTION_LABEL, "Up");
        up.putValue(Action.SHORT_DESCRIPTION, "Back to Top Page");
        up.setEnabled(false);
        
        this.forward = new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                navigate(browser, Boolean.FALSE);
            }
        };
        forward.putValue(Action.NAME, "  \u25B6  ");
        forward.putValue(MENU_ACTION_LABEL, "Forward");
        forward.putValue(Action.SHORT_DESCRIPTION, "Go Forward in History");
        forward.setEnabled(false);

        this.reload = new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                browser.reload();
            }
        };
        reload.putValue(Action.NAME, "  \u21BB  "); // 21BB 27F3
        reload.putValue(MENU_ACTION_LABEL, "Reload");
        reload.putValue(Action.SHORT_DESCRIPTION, "Reload Current Page");
        reload.setEnabled(true); // reload does not depend on history
        
        add(new JButton(back));
        add(new JButton(up));
        add(new JButton(forward));
        add(new JButton(reload));
    }
    
    public void updateOnLinkNavigation(HtmlHistoryBrowser browser)  {
        back.setEnabled(browser.canGoBack());
        forward.setEnabled(browser.canGoForward());
        up.setEnabled(browser.canGoBack());
    }

    private void navigate(HtmlHistoryBrowser browser, Boolean goBack) {
        if (goBack == Boolean.TRUE)
            browser.back();
        else if (goBack == Boolean.FALSE)
            browser.forward();
        else
            browser.up();
        
        updateOnLinkNavigation(browser);
    }
}