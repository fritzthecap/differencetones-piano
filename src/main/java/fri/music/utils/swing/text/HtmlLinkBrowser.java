package fri.music.utils.swing.text;

import java.awt.BorderLayout;
import java.awt.Desktop;
import java.io.IOException;
import java.net.URL;
import javax.swing.JEditorPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;

/**
 * HTML browser that can load hyperlinks but can not navigate in history ("Back", "Forward").
 * HTTP links will be opened in an external desktop browser, relative links in this view.
 */
public class HtmlLinkBrowser extends JPanel implements HyperlinkListener
{
    protected final JEditorPane htmlView;
    protected final HtmlViewActions htmlViewActions;
    
    /** @param url optional, the initial URL to load. */
    public HtmlLinkBrowser(URL url) {
        super(new BorderLayout());
        
        this.htmlView = newHtmlView(url);
        
        htmlView.addHyperlinkListener(this);
        
        this.htmlViewActions = new HtmlViewActions(htmlView); // font popup menu
        
        add(new JScrollPane(htmlView), BorderLayout.CENTER);
    }
    
    /** Factory method for HTML view, called from constructor. */
    protected JEditorPane newHtmlView(URL url) {
        return new HtmlView(url);
    }

    /** Called when user hovers or clicks a hyperlink in HTML-document. */
    @Override
    public final void hyperlinkUpdate(HyperlinkEvent event) {
        final URL url = event.getURL();
        final String protocol = url.getProtocol();
        
        if (event.getEventType() == HyperlinkEvent.EventType.ACTIVATED)
            if (protocol != null && protocol.toLowerCase().startsWith("http"))
                gotoExternalHyperlink(url); // open in external web-browser
            else
                gotoInternalHyperlink(url); // open in this view
        else if (event.getEventType() == HyperlinkEvent.EventType.ENTERED)
            htmlView.setToolTipText(url.toExternalForm());
        else if (event.getEventType() == HyperlinkEvent.EventType.EXITED)
            htmlView.setToolTipText(null);
    }

    /** Called on internal hyperlink click, sets a new page. */
    protected final void gotoUrl(URL url) {
        try {
            htmlView.setPage(url);
        }
        catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
    
    /** Called on internal hyperlink click, does nothing, to be overridden by history manager (navigator). */
    protected void manageHistory(URL url) {
    }
    
    
    private void gotoExternalHyperlink(URL url) {
        try {
            Desktop.getDesktop().browse(url.toURI());
        }
        catch (Exception e) {
            e.printStackTrace(); // may happen in environments that do not support Java/Swing Desktop
        }
    }

    private void gotoInternalHyperlink(URL url) {
        gotoUrl(url);
        manageHistory(url);
    }
}