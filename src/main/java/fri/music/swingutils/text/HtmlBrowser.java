package fri.music.swingutils.text;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Desktop;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import javax.swing.Action;
import javax.swing.JEditorPane;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;

/**
 * HTML documentation browser that can load hyperlinks, 
 * but not HTTP links, just relative ones. This is NOT a web browser!
 * All hyperlinks must be written relative to <code>HtmlResources.class</code>
 * which is in "fri/music/", so they mostly will start with "instrument/wave" or similar.
 * This is intended for loading HTML documents from the application's JAR file.
 * Mind that no dotted hyperlinks like "../xxx.html" are allowed!
 */
public class HtmlBrowser extends JPanel implements HyperlinkListener, HtmlViewScanningHeaders.HeaderListener
{
    private final Class<?> htmlResourcesClass;
    private final HtmlBrowserToolbar toolbar;
    private final JEditorPane htmlView;
    
    private final List<URL> history = new ArrayList<>();
    private int currentHistoryIndex = 0;
    
    /**
     * @param url required, the initial URL to load.
     * @param htmlResourcesClass optional, a class that is the resource-loader
     *      for all HTML resources, will be this class when null.
     */
    public HtmlBrowser(URL url, Class<?> htmlResourcesClass) {
        super(new BorderLayout());
        
        this.htmlResourcesClass = (htmlResourcesClass != null) ? htmlResourcesClass : this.getClass();
        this.toolbar = new HtmlBrowserToolbar(this); // navigation
        this.htmlView = new HtmlViewScanningHeaders(url, this);
        history.add(url);
        
        htmlView.addHyperlinkListener(this);
        
        final HtmlViewActions htmlViewActions = new HtmlViewActions(htmlView); // popup menu
        int index = 0;
        htmlViewActions.contextMenu.add(newJMenuItem(toolbar.back), index++);
        htmlViewActions.contextMenu.add(newJMenuItem(toolbar.up), index++);
        htmlViewActions.contextMenu.add(newJMenuItem(toolbar.forward), index++);
        htmlViewActions.contextMenu.add(new JPopupMenu.Separator(), index++);
        
        add(new JScrollPane(htmlView), BorderLayout.CENTER);
        add(toolbar, BorderLayout.NORTH);
    }
    
    /** Implements HtmlView.HeaderListener to render click-able headers in tool-bar choice. */
    @Override
    public void headersAvailable(List<HtmlViewScanningHeaders.HeaderElement> headers) {
        toolbar.setHeaders(
            headers,
            new ItemListener() {
                @Override
                public void itemStateChanged(ItemEvent event) {
                    if (event.getStateChange() == ItemEvent.SELECTED) {
                        final HtmlViewScanningHeaders.HeaderElement header = 
                                (HtmlViewScanningHeaders.HeaderElement) event.getItem();
                        htmlView.scrollToReference(header.id());
                    }
                }
            }
        );
        htmlView.requestFocus(); // else "Ctrl +" font command would not work immediately
    }

    /** Toolbar navigation callback. */
    public void up() {
        currentHistoryIndex = 0;
        gotoCurrentIndex();
    }

    /** Toolbar navigation callback. */
    public void back() {
        currentHistoryIndex--;
        gotoCurrentIndex();
    }

    /** Toolbar navigation callback, also covers canGoUp(). */
    public boolean canGoBack() {
        return currentHistoryIndex > 0;
    }
    
    /** Toolbar navigation callback. */
    public void forward() {
        currentHistoryIndex++;
        gotoCurrentIndex();
    }

    /** Toolbar navigation callback. */
    public boolean canGoForward() {
        return currentHistoryIndex < (history.size() - 1);
    }
    
    /** Called when user hovers or clicks a hyperlink in HTML-document. */
    @Override
    public void hyperlinkUpdate(HyperlinkEvent event) {
        final String[] fileNameAndReference = splitFilenameAndReference(event.getDescription());
        final String relativeFileName = fileNameAndReference[0];
        final String anchorRef = fileNameAndReference[1];
        final boolean hasAnchorRef = (anchorRef.length() > 0);
        
        if (event.getEventType() == HyperlinkEvent.EventType.ACTIVATED) {
            if (relativeFileName.length() <= 0 && hasAnchorRef) { // url is an anchor-reference, stay on page
                htmlView.scrollToReference(anchorRef);
                manageHistory(toUrl(null, anchorRef));
            }
            else if (relativeFileName.startsWith("http")) { // open hyperlink in external browser
                try {
                    Desktop.getDesktop().browse(URI.create(event.getDescription()));
                }
                catch (IOException e) {
                    throw new RuntimeException(e);
                }
            }
            else {
                final URL gotoUrl = htmlResourcesClass.getResource(relativeFileName);
                // mind that all links must be relative to htmlResourcesClass for this to work!
                final URL gotoUrlWithRef = (hasAnchorRef && gotoUrl != null)
                        ? toUrl(gotoUrl, anchorRef)
                        : gotoUrl;
                
                if (gotoUrlWithRef != null) {
                    try {
                        htmlView.setPage(gotoUrlWithRef);
                        manageHistory(gotoUrlWithRef);
                    }
                    catch (IOException e) {
                        throw new RuntimeException(e);
                    }
                }
                else
                    System.err.println("URL is not resolvable from "+htmlResourcesClass.getName()+": "+relativeFileName);
            }
        }
        else if (event.getEventType() == HyperlinkEvent.EventType.ENTERED) {
            htmlView.setToolTipText(event.getDescription());
        }
        else if (event.getEventType() == HyperlinkEvent.EventType.EXITED) {
            htmlView.setToolTipText(null);
        }
    }
    
    
    private Component newJMenuItem(Action action) {
        final JMenuItem item = new JMenuItem(action);
        item.setText((String) action.getValue(HtmlBrowserToolbar.MENU_ACTION_LABEL));
        return item;
    }

    private void gotoCurrentIndex() {
        try {
            htmlView.setPage(history.get(currentHistoryIndex));
        }
        catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
    
    private URL toUrl(URL url, String anchorRef) {
        if (anchorRef == null)
            return url;
        
        try {
            final String externalForm = (url == null)
                ? splitFilenameAndReference(history.get(currentHistoryIndex).toString())[0]
                : url.toExternalForm();
            
            return new URI(externalForm+"#"+anchorRef).toURL();
        }
        catch (MalformedURLException | URISyntaxException e) {
            throw new RuntimeException(e);
        }
    }

    /** Called when hyperlink clicked, not on navigation. */
    private void manageHistory(URL url) {
        if (canGoForward()) // navigating forward while different forward path exists, remove it
            for (int i = history.size() - 1; i > currentHistoryIndex; i--)
                history.remove(i);
        
        history.add(url);
        currentHistoryIndex++;
        
        toolbar.updateOnLinkNavigation(this);
    }

    private String[] splitFilenameAndReference(String url) {
        final int hashIndex = url.indexOf('#');
        final String reference = (hashIndex >= 0) ? url.substring(hashIndex + 1) : "";
        final String filename = (hashIndex >= 0) ? url.substring(0, hashIndex) : url;
        return new String[] { filename, reference };
    }
    
    
    public static void main(String[] args) {
        javax.swing.SwingUtilities.invokeLater(() -> {
            final Class<?> htmlResourcesClass = HtmlBrowser.class;
            final URL url = htmlResourcesClass.getResource("mainBrowseTest.html");
            try {
                final URL urlWithRef = new URI(url.toExternalForm()+"#Three").toURL();
                final HtmlBrowser htmlBrowser = new HtmlBrowser(urlWithRef, htmlResourcesClass);
                
                final javax.swing.JFrame frame = new javax.swing.JFrame("HTML Browse Test");
                frame.getContentPane().add(htmlBrowser);
                frame.setDefaultCloseOperation(javax.swing.JFrame.EXIT_ON_CLOSE);
                frame.setSize(new java.awt.Dimension(600, 400));
                frame.setLocationRelativeTo(null);
                frame.setVisible(true);
            }
            catch (MalformedURLException | URISyntaxException e) {
                throw new RuntimeException(e);
            }
        });
    }
}