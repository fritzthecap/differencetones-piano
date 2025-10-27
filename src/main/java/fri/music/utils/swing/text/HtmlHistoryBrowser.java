package fri.music.utils.swing.text;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Rectangle;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import javax.swing.Action;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.text.Document;

/** HTML documentation browser with history navigation. */
public class HtmlHistoryBrowser extends HtmlLinkBrowser
{
    protected final HtmlHistoryToolbar toolbar;
    
    private final List<URL> history = new ArrayList<>();
    private int currentHistoryIndex = 0;
    
    /** @param url optional, the initial URL to load. */
    public HtmlHistoryBrowser(URL url) {
        super(url);
        
        this.toolbar = newHtmlToolbar(); // navigation bar
        
        if (url != null)
            history.add(url);
        
        int index = 0; // put actions into context-menu
        htmlViewActions.contextMenu.add(newJMenuItem(toolbar.back), index++);
        htmlViewActions.contextMenu.add(newJMenuItem(toolbar.up), index++);
        htmlViewActions.contextMenu.add(newJMenuItem(toolbar.forward), index++);
        htmlViewActions.contextMenu.add(newJMenuItem(toolbar.reload), index++);
        htmlViewActions.contextMenu.add(new JPopupMenu.Separator(), index++);
        
        add(toolbar, BorderLayout.NORTH);
    }
    
    /** Factory-method for the navigation-toolbar. */
    protected HtmlHistoryToolbar newHtmlToolbar() {
        return new HtmlHistoryToolbar(this);
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
        htmlViewActions.updateFontSizes(htmlView); // may have been altered by another view
    }

    
    /** Called on internal hyperlink click, not on navigation via "Back" and "Forward". */
    @Override
    protected final void manageHistory(URL url) {
        if (canGoForward()) // navigating forward while different forward path exists, remove it
            for (int i = history.size() - 1; i > currentHistoryIndex; i--)
                history.remove(i);
        
        history.add(url);
        currentHistoryIndex++;
        
        toolbar.updateOnLinkNavigation(this);
    }


    private void gotoCurrentIndex() {
        gotoUrl(history.get(currentHistoryIndex));
    }
    
    private Component newJMenuItem(Action action) {
        final JMenuItem item = new JMenuItem(action);
        item.setText((String) action.getValue(HtmlHistoryToolbar.MENU_ACTION_LABEL));
        return item;
    }
}