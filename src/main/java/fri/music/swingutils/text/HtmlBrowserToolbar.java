package fri.music.swingutils.text;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
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
import fri.music.swingutils.window.DialogStarter;

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
    
    private JComboBox<HtmlViewWithHeaders.HeaderElement> headersChoice;
    
    public HtmlBrowserToolbar(final HtmlBrowser browser, ItemListener referenceItemListener) {
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
        
        add(newButton(back));
        add(newButton(up));
        add(newButton(forward));
        add(newButton(reload));
        
        final JButton help = new JButton("Help");
        help.setToolTipText(HelpForBrowser.TITLE);
        help.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                DialogStarter.showModelessHtmlDialog(
                        HelpForBrowser.TITLE, 
                        help, 
                        HelpForBrowser.URL,
                        null);
            }
        });
        add(help);
        
        add(Box.createHorizontalGlue());
        add(new JLabel(" Chapters: "));
        headersChoice = new JComboBox<HtmlViewWithHeaders.HeaderElement>();
        headersChoice.setMaximumRowCount(25);
        headersChoice.addItemListener(referenceItemListener);
        add(headersChoice);
    }
    
    public void updateOnLinkNavigation(HtmlBrowser browser)  {
        back.setEnabled(browser.canGoBack());
        forward.setEnabled(browser.canGoForward());
        up.setEnabled(browser.canGoBack());
    }

    public void setHeaders(List<HtmlViewWithHeaders.HeaderElement> headers, ItemListener itemListener) {
        if (headersChoice == null) {
            headersChoice = new JComboBox<HtmlViewWithHeaders.HeaderElement>();
            headersChoice.setMaximumRowCount(25);
            headersChoice.addItemListener(itemListener);
            add(Box.createHorizontalGlue());
            add(new JLabel(" Chapters: "));
            add(headersChoice);
        }
        
        final DefaultComboBoxModel<HtmlViewWithHeaders.HeaderElement> model = new DefaultComboBoxModel<>();
        for (HtmlViewWithHeaders.HeaderElement header : headers)
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
    
    private JButton newButton(Action action) {
        final JButton button = new JButton(action);
        //button.setVerticalAlignment(SwingConstants.TOP);
        //button.setVerticalTextPosition(SwingConstants.TOP);
        return button;
    }
}