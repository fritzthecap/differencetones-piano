package fri.music.swingutils.text;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Rectangle;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import javax.swing.Action;
import javax.swing.JEditorPane;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.text.Document;

/**
 * HTML documentation browser with navigation toolbar.
 */
public class HtmlBrowser extends HtmlBrowserBase implements HtmlViewWithHeaders.HeaderListener
{
    private final HtmlBrowserToolbar toolbar;
    
    private final List<URL> history = new ArrayList<>();
    private int currentHistoryIndex = 0;
    
    private ItemListener referenceItemListener;
    
    /**
     * @param url required, the initial URL to load.
     * @param htmlResourcesClass optional, a class that is the resource-loader
     *      for all HTML resources, will be this class when null.
     */
    public HtmlBrowser(URL url, Class<?> htmlResourcesClass) {
        super(url, htmlResourcesClass);
        
        this.toolbar = new HtmlBrowserToolbar(this); // navigation bar
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
        return new HtmlViewWithHeaders(url, this);
    }
    
    /** Implements HtmlView.HeaderListener to set reload action disabled. */
    @Override
    public void startLoadingPage() {
        toolbar.reload.setEnabled(false);
    }
    
    /** Implements HtmlView.HeaderListener to render click-able headers in tool-bar choice. */
    @Override
    public void endLoadingPage(List<HtmlViewWithHeaders.HeaderElement> headers) {
        toolbar.reload.setEnabled(true);
        
        if (referenceItemListener == null)
            referenceItemListener = new ItemListener() {
                @Override
                public void itemStateChanged(ItemEvent event) {
                    if (event.getStateChange() == ItemEvent.SELECTED) {
                        final HtmlViewWithHeaders.HeaderElement header = 
                                (HtmlViewWithHeaders.HeaderElement) event.getItem();
                        gotoAnchorReference(header.id());
                    }
                }
            };
        
        toolbar.setHeaders(headers, referenceItemListener);
        
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
        
        // Fix for JEditorPane not scrolling to top when going from ref back to non-ref
        final URL oldUrl = history.get(currentHistoryIndex + 1);
        final URL newUrl = history.get(currentHistoryIndex);
        if (Objects.equals(oldUrl.getPath(), newUrl.getPath()) == true &&
                Objects.equals(oldUrl.getRef(), newUrl.getRef()) == false)
            htmlView.scrollRectToVisible(new Rectangle(0, 0, 1, 1));
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
        // clear JEditorPane page cache, else nothing happens on setPage()
        htmlView.getDocument().putProperty(Document.StreamDescriptionProperty, null);
        gotoCurrentIndex();
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
        gotoUrl(history.get(currentHistoryIndex));
    }
    
   
    /*public static void main(String[] args) {
        javax.swing.SwingUtilities.invokeLater(() -> {
            final Class<?> htmlResourcesClass = HtmlBrowser.class;
            final URL url = htmlResourcesClass.getResource("mainBrowseTest.html");
            try {
                final URL urlWithRef = new java.net.URI(url.toExternalForm()+"#Three").toURL();
                final HtmlBrowser htmlBrowser = new HtmlBrowser(urlWithRef, htmlResourcesClass);
                
                final javax.swing.JFrame frame = new javax.swing.JFrame("HTML Browse Test");
                frame.getContentPane().add(htmlBrowser);
                frame.setDefaultCloseOperation(javax.swing.JFrame.EXIT_ON_CLOSE);
                frame.setSize(new java.awt.Dimension(600, 400));
                frame.setLocationRelativeTo(null);
                frame.setVisible(true);
            }
            catch (java.net.MalformedURLException | java.net.URISyntaxException e) {
                throw new RuntimeException(e);
            }
        });
    }*/
}