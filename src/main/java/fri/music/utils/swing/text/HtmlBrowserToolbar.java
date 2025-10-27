package fri.music.utils.swing.text;

import java.awt.Color;
import java.awt.event.ItemListener;
import java.util.List;
import javax.swing.Box;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;

/** Toolbar with navigable HTML h1-h6 chapters overview. */
public class HtmlBrowserToolbar extends HtmlHistoryToolbar
{
    private JComboBox<HtmlViewWithHeaders.HeaderElement> headersChoice;

    public HtmlBrowserToolbar(final HtmlBrowser browser, ItemListener referenceItemListener) {
        super(browser);
        
        final JButton help = new JButton("Help");
        help.setToolTipText(HelpForBrowser.TITLE);
        help.addActionListener(event -> HelpWindowSingleton.start(browser, HelpForBrowser.TITLE, HelpForBrowser.URL));
        add(help);
        
        add(Box.createHorizontalGlue());
        add(new JLabel(" Chapters: "));
        headersChoice = new JComboBox<HtmlViewWithHeaders.HeaderElement>();
        headersChoice.setMaximumRowCount(25);
        headersChoice.setBackground(Color.WHITE);
        headersChoice.addItemListener(referenceItemListener);
        add(headersChoice);
    }
    
    public void setHeaders(List<HtmlViewWithHeaders.HeaderElement> headers) {
        final DefaultComboBoxModel<HtmlViewWithHeaders.HeaderElement> model = new DefaultComboBoxModel<>();
        for (HtmlViewWithHeaders.HeaderElement header : headers)
            model.addElement(header);
        
        headersChoice.setModel(model);
    }
}