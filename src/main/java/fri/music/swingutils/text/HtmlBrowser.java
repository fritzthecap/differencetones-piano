package fri.music.swingutils.text;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import javax.swing.Action;
import javax.swing.JEditorPane;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.text.Document;
import javax.swing.text.html.HTMLDocument;

/**
 * HTML documentation browser with navigation toolbar.
 */
public class HtmlBrowser extends HtmlBrowserBase implements HtmlViewScanningHeaders.HeaderListener
{
    private final HtmlBrowserToolbar toolbar;
    
    private final List<URL> history = new ArrayList<>();
    private int currentHistoryIndex = 0;
    
    /**
     * @param url required, the initial URL to load.
     * @param htmlResourcesClass optional, a class that is the resource-loader
     *      for all HTML resources, will be this class when null.
     */
    public HtmlBrowser(URL url, Class<?> htmlResourcesClass) {
        super(url, htmlResourcesClass);
        
        this.toolbar = new HtmlBrowserToolbar(this); // navigation
        history.add(url);
        
        int index = 0;
        htmlViewActions.contextMenu.add(newJMenuItem(toolbar.back), index++);
        htmlViewActions.contextMenu.add(newJMenuItem(toolbar.up), index++);
        htmlViewActions.contextMenu.add(newJMenuItem(toolbar.forward), index++);
        htmlViewActions.contextMenu.add(newJMenuItem(toolbar.reload), index++);
        htmlViewActions.contextMenu.add(new JPopupMenu.Separator(), index++);
        
        add(toolbar, BorderLayout.NORTH);
    }
    
    @Override
    protected JEditorPane newHtmlView(URL url) {
        return new HtmlViewScanningHeaders(url, this);
    }
    
    /** Implements HtmlView.HeaderListener to set reload action disabled. */
    @Override
    public void startLoadingPage() {
        toolbar.reload.setEnabled(false);
    }
    
    /** Implements HtmlView.HeaderListener to render click-able headers in tool-bar choice. */
    @Override
    public void endLoadingPage(List<HtmlViewScanningHeaders.HeaderElement> headers) {
        toolbar.reload.setEnabled(true);
        
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
    
    /** Toolbar navigation callback. */
    public void reload() {
        try {
            final URL url = history.get(currentHistoryIndex);
            // clear JEditorPane page cache, else nothing happens on setPage()
            htmlView.getDocument().putProperty(Document.StreamDescriptionProperty, null);
            // set same page again
            htmlView.setPage(history.get(currentHistoryIndex));
        }
        catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    /** Called when hyperlink clicked, not on navigation. */
    @Override
    protected void manageHistory(URL url) {
        if (canGoForward()) // navigating forward while different forward path exists, remove it
            for (int i = history.size() - 1; i > currentHistoryIndex; i--)
                history.remove(i);
        
        history.add(url);
        currentHistoryIndex++;
        
        toolbar.updateOnLinkNavigation(this);
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
    
   
    /*public static void main(String[] args) {
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
    }*/
}