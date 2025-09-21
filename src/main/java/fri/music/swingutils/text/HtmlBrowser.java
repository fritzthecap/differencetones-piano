package fri.music.swingutils.text;

import java.awt.BorderLayout;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import javax.swing.JEditorPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;

/**
 * HTML documentation browser that can load hyperlinks, 
 * but not HTTP links, just relative ones.
 * All hyperlinks must be written as relative to a given resource-class.
 * Intended for loading HTML documents from the application's JAR file.
 * No dotted hyperlinks like "../xxx.html" are allowed!
 */
public class HtmlBrowser extends JPanel implements HyperlinkListener
{
    private final JEditorPane htmlView;
    private final Class<?> htmlResourcesClass;
    
    /**
     * @param htmlUrl required, the initial URL to load.
     * @param htmlResourcesClass optional, a class that is the resource-loader
     *      for all HTML resources, will be this class when null.
     */
    public HtmlBrowser(URL htmlUrl, Class<?> htmlResourcesClass) {
        super(new BorderLayout());
        
        try {
            this.htmlView = new EditorPane(htmlUrl);
        }
        catch (Exception e) {
            throw new RuntimeException(e);
        }
        this.htmlResourcesClass = (htmlResourcesClass != null) ? htmlResourcesClass : this.getClass();
        
        htmlView.addHyperlinkListener(this);
        
        new HtmlViewerActions(htmlView);
        
        // TODO: back + forward buttons, URL text disabled
        
        add(new JScrollPane(htmlView), BorderLayout.CENTER);
    }

    /** Called when user clicks a hyperlink in HTML-document. */
    @Override
    public void hyperlinkUpdate(HyperlinkEvent event) {
        if (event.getEventType() == HyperlinkEvent.EventType.ACTIVATED) {
            final String[] fileNameAndReference = splitHrefTextToFilenameAndReference(event.getDescription());
            final String relativeFileName = fileNameAndReference[0];
            final String anchorRef = fileNameAndReference[1];
            final boolean hasAnchorRef = (anchorRef.length() > 0);
            
            if (relativeFileName.length() <= 0 && hasAnchorRef) { // is an anchor URL, stay on page
                htmlView.scrollToReference(anchorRef);
            }
            else {
                final URL gotoUrl = htmlResourcesClass.getResource(relativeFileName);
                // mind that all links must be relative to htmlResourcesClass for this to work!
                try {
                    final URL gotoUrlWithRef = (hasAnchorRef && gotoUrl != null)
                            ? new URI(gotoUrl.toExternalForm()+"#"+anchorRef).toURL()
                            : gotoUrl;
                    if (gotoUrlWithRef != null)
                        htmlView.setPage(gotoUrlWithRef);
                    else
                        System.err.println("URL is not resolvable from "+htmlResourcesClass.getName()+": "+relativeFileName);
                }
                catch (Exception e1) {
                    throw new RuntimeException(e1);
                }
            }
        }
    }
    
    private String[] splitHrefTextToFilenameAndReference(String hrefAttributeText) {
        final int hashIndex = hrefAttributeText.indexOf('#');
        final String reference = (hashIndex >= 0) ? hrefAttributeText.substring(hashIndex + 1) : "";
        String filename = (hashIndex >= 0) ? hrefAttributeText.substring(0, hashIndex) : hrefAttributeText;
        while (filename.startsWith("/"))
            filename = filename.substring(1);
        return new String[] { filename, reference };
    }
    
    /*
    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
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
    }*/
}