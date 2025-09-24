package fri.music.swingutils.text;

import java.awt.BorderLayout;
import java.awt.Desktop;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
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
    
    private final Class<?> htmlResourcesClass;
    
    /**
     * @param url required, the initial URL to load.
     * @param htmlResourcesClass optional, a class that is the resource-loader
     *      for all HTML resources, will be this class when null.
     */
    public HtmlBrowserBase(URL url, Class<?> htmlResourcesClass) {
        super(new BorderLayout());
        
        this.htmlResourcesClass = (htmlResourcesClass != null) ? htmlResourcesClass : this.getClass();
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
        final String linkDescription = event.getDescription();
        final String[] fileAndReference = splitFileAndReference(linkDescription);
        final String file = fileAndReference[0];
        final String anchorRef = fileAndReference[1];
        final boolean hasAnchorRef = (anchorRef.length() > 0);
        
        if (event.getEventType() == HyperlinkEvent.EventType.ACTIVATED) {
            if (file.length() <= 0 && hasAnchorRef) { // url is an anchor-reference, stay on page
                gotoAnchorReference(anchorRef);
            }
            else if (file.startsWith("http")) { // open hyperlink in external browser
                gotoExternalHyperlink(linkDescription);
            }
            else {
                final URL gotoUrl = htmlResourcesClass.getResource(file);
                // mind that all links must be relative to htmlResourcesClass for this to work!
                final URL gotoUrlWithRef = (hasAnchorRef && gotoUrl != null)
                        ? toUrl(gotoUrl, anchorRef)
                        : gotoUrl;
                gotoInternalHyperlink(gotoUrlWithRef, file);
            }
        }
        else if (event.getEventType() == HyperlinkEvent.EventType.ENTERED) {
            htmlView.setToolTipText(linkDescription);
        }
        else if (event.getEventType() == HyperlinkEvent.EventType.EXITED) {
            htmlView.setToolTipText(null);
        }
    }

    /** Called on hyperlink click onto an anchor-reference, or navigation via header list. */
    protected void gotoAnchorReference(String anchorRef) {
        htmlView.scrollToReference(anchorRef);
        manageHistory(toUrl(null, anchorRef));
    }
    
    /** Called on internal (relative) hyperlink click. */
    protected final void gotoUrl(URL url) {
        try {
            htmlView.setPage(url);
        }
        catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
    
    private void gotoInternalHyperlink(URL url, String relativeFileName) {
        if (url != null) {
            gotoUrl(url);
            manageHistory(url);
        }
        else
            System.err.println("URL is not resolvable from "+htmlResourcesClass.getName()+": "+relativeFileName);
    }
    
    private void gotoExternalHyperlink(String link) {
        try {
            Desktop.getDesktop().browse(URI.create(link));
        }
        catch (IOException e) {
            e.printStackTrace(); // may happen in environments that do not support Java/Swing Desktop
        }
    }

    /** Called on hyperlink click, does nothing, to be overridden by history manager (navigator). */
    protected void manageHistory(URL url) {
    }
    
    protected URL toUrl(URL url, String anchorRef) {
        if (anchorRef == null)
            return url;
        
        try {
            final String externalForm = (url == null)
                ? splitFileAndReference(htmlView.getPage().toString())[0]
                : url.toExternalForm();
            
            return new URI(externalForm+"#"+anchorRef).toURL();
        }
        catch (MalformedURLException | URISyntaxException e) {
            throw new RuntimeException(e);
        }
    }

    private String[] splitFileAndReference(String url) {
        final int hashIndex = url.indexOf('#');
        final String reference = (hashIndex >= 0) ? url.substring(hashIndex + 1) : "";
        final String filename = (hashIndex >= 0) ? url.substring(0, hashIndex) : url;
        return new String[] { filename, reference };
    }
}