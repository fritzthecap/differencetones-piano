package fri.music.utils.swing.text;

import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.List;
import javax.swing.JEditorPane;

/** HTML documentation browser with history navigation and chapters overview.  */
public class HtmlBrowser extends HtmlHistoryBrowser implements HtmlViewWithHeaders.HeaderListener
{
    /** @param url optional, the initial URL to load. */
    public HtmlBrowser(URL url) {
        super(url);
    }
    
    /** Factory-method override, returns a HtmlBrowserToolbar instance. */
    @Override
    protected HtmlHistoryToolbar newHtmlToolbar() {
        return new HtmlBrowserToolbar(this, new ItemListener() {
            @Override
            public void itemStateChanged(ItemEvent event) {
                if (event.getStateChange() == ItemEvent.SELECTED) {
                    final HtmlViewWithHeaders.HeaderElement header = 
                            (HtmlViewWithHeaders.HeaderElement) event.getItem();
                    gotoAnchorReference(header.id(), header.startOffset());
                }
            }
        });
    }
    
    /** Factory-method override, returns a HtmlViewWithHeaders instance. */
    @Override
    protected JEditorPane newHtmlView(URL url) {
        return new HtmlViewWithHeaders(url, this);
    }
    
    
    /** Implements HeaderListener to set reload action disabled. */
    @Override
    public void startLoadingPage() {
        getToolbar().reload.setEnabled(false);
    }
    
    /** Implements HeaderListener to render click-able headers in tool-bar choice. */
    @Override
    public void endLoadingPage(List<HtmlViewWithHeaders.HeaderElement> headers) {
        getToolbar().reload.setEnabled(true);
        getToolbar().setHeaders(headers);
        htmlView.requestFocus(); // else "Ctrl +" font command would not work immediately
    }

    
    private HtmlBrowserToolbar getToolbar() {
        return (HtmlBrowserToolbar) toolbar;
    }
    
    /** Called when a header (h1-h6) is selected in combo-box on top. */
    private void gotoAnchorReference(String id, int startOffset) {
        if (id != null)
            gotoAnchorReference(id);
        else // forgot to give the header an id?
            ((HtmlView) htmlView).scrollToStartOffset(startOffset);
    }
    
    private void gotoAnchorReference(String anchorRef) {
        htmlView.scrollToReference(anchorRef);
        manageHistory(toUrl(anchorRef));
    }
    
    private final URL toUrl(String anchorRef) {
        final String urlString = htmlView.getPage().toExternalForm();
        final int hashIndex = urlString.indexOf('#');
        final String baseUrl = (hashIndex >= 0) ? urlString.substring(0, hashIndex) : urlString;
        try {
            return new URI(baseUrl+"#"+anchorRef).toURL();
        }
        catch (MalformedURLException | URISyntaxException e) {
            throw new RuntimeException(e);
        }
    }
}