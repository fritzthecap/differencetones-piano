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
 * HTML documentation browser that can load hyperlinks but can not navigate.
 * HTTP links are opened in an external desktop browser, relative links in this view.
 * Such internal hyperlinks must be written relative to <code>HtmlResources.class</code>
 * which is in "fri/music/", so the link mostly will start with "instrument/wave/" or similar.
 * Its intent is to load HTML documents from the application's JAR file.
 * Dotted relative links, like "../xxx.html", are not allowed!
 */
public class HtmlBrowserBase extends JPanel implements HyperlinkListener
{
    protected final JEditorPane htmlView;
    protected final HtmlViewActions htmlViewActions;
    
    /** @param url required, the initial URL to load. */
    public HtmlBrowserBase(URL url) {
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
    public void hyperlinkUpdate(HyperlinkEvent event) {
        final URL url = event.getURL();
        final String protocol = url.getProtocol();
        
        if (event.getEventType() == HyperlinkEvent.EventType.ACTIVATED)
            if (protocol.startsWith("http")) // open hyperlink in external browser
                gotoExternalHyperlink(url);
            else
                gotoInternalHyperlink(url); // go to this page
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