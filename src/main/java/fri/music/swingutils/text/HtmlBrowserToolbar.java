package fri.music.swingutils.text;

import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ItemListener;
import java.util.List;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Box;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JToolBar;

/**
 * The toolbar for HTML browser.
 */
public class HtmlBrowserToolbar extends JToolBar
{
    /** The key to another label (not unicode-icon) for actions to be used in menus. */
    public static final String MENU_ACTION_LABEL = "menuActionLabel";
    
    public final Action back;
    public final Action up;
    public final Action forward;
    public final Action reload;
    
    private JComboBox<HtmlViewScanningHeaders.HeaderElement> headersChoice;
    
    public HtmlBrowserToolbar(final HtmlBrowser browser) {
        this.back = new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                navigate(browser, Boolean.TRUE);
            }
        };
        back.putValue(Action.NAME, "    \u25C0    ");
        back.putValue(MENU_ACTION_LABEL, "Back");
        back.putValue(Action.SHORT_DESCRIPTION, "Go Backward in History");
        back.setEnabled(false);
        
        this.up = new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                navigate(browser, null);
            }
        };
        up.putValue(Action.NAME, "    \u25B2    ");
        up.putValue(MENU_ACTION_LABEL, "Up");
        up.putValue(Action.SHORT_DESCRIPTION, "Back to Top Page");
        up.setEnabled(false);
        
        this.forward = new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                navigate(browser, Boolean.FALSE);
            }
        };
        forward.putValue(Action.NAME, "    \u25B6    ");
        forward.putValue(MENU_ACTION_LABEL, "Forward");
        forward.putValue(Action.SHORT_DESCRIPTION, "Go Forward in History");
        forward.setEnabled(false);

        this.reload = new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                browser.reload();
            }
        };
        reload.putValue(Action.NAME, "    \u21BB    "); // 21BB 27F3
        reload.putValue(MENU_ACTION_LABEL, "Reload");
        reload.putValue(Action.SHORT_DESCRIPTION, "Reload Current Page");
        reload.setEnabled(true); // reload does not depend on history
        
        add(newButton(back, 6));
        add(newButton(up, 8));
        add(newButton(forward, 6));
        add(newButton(reload, 6));
    }
    
    public void updateOnLinkNavigation(HtmlBrowser browser)  {
        back.setEnabled(browser.canGoBack());
        forward.setEnabled(browser.canGoForward());
        up.setEnabled(browser.canGoBack());
    }

    public void setHeaders(List<HtmlViewScanningHeaders.HeaderElement> headers, ItemListener itemListener) {
        if (headersChoice == null) {
            headersChoice = new JComboBox<HtmlViewScanningHeaders.HeaderElement>();
            headersChoice.setMaximumRowCount(20);
            headersChoice.addItemListener(itemListener);
            add(Box.createHorizontalGlue());
            add(new JLabel(" Overview: "));
            add(headersChoice);
        }
        
        final DefaultComboBoxModel<HtmlViewScanningHeaders.HeaderElement> model = new DefaultComboBoxModel<>();
        for (HtmlViewScanningHeaders.HeaderElement header : headers)
            model.addElement(header);
        
        headersChoice.setModel(model);
    }
    
    private void navigate(HtmlBrowser browser, Boolean goBack) {
        if (goBack == Boolean.TRUE)
            browser.back();
        else if (goBack == Boolean.FALSE)
            browser.forward();
        else
            browser.up();
        
        updateOnLinkNavigation(browser);
    }
    
    private JButton newButton(Action action, int bottomMargin) {
        final JButton button = new JButton(action);
        button.setFocusPainted(false);
        button.setMargin(new Insets(1, 4, bottomMargin, 4));
        return button;
    }
}