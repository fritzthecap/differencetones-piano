package fri.music.utils.swing.text;

import java.awt.Rectangle;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import javax.swing.JEditorPane;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.text.BadLocationException;
import javax.swing.text.Element;
import javax.swing.text.html.HTMLDocument;
import javax.swing.text.html.HTMLEditorKit;
import javax.swing.text.html.StyleSheet;

/**
 * JEditorPane bug fix concerning URLs with "ref".
 * This view is HTML only, no RTF or other.
 * Please do NOT move or rename this class unless you also move or rename 
 * <code>HtmlView.css</code> in <code>src/main/resources/fri/music/utils/swing/text</code>.
 */
public class HtmlView extends JEditorPane
{
    /** @param url optional, HTML location to render. */
    public HtmlView(URL url) {
        super(); // do not pass URL before constructor is done
        
        setEditable(false); // no editing support
        
        // do not share CSS with all other HTML views like JLabel that will also use the global HtmlEditorKit
        final HtmlEditorKitWithLocalStyles editorKit = new HtmlEditorKitWithLocalStyles();
        final StyleSheet localStyleSheet = new StyleSheet();
        localStyleSheet.addStyleSheet(editorKit.getGlobalStyleSheet()); // merge JDK default styles into empty sheet
        editorKit.setStyleSheet(localStyleSheet);
        
        setEditorKit(editorKit); // replaces setContentType("text/html"), do not leave this to setPage!
        
        // load CSS styles from file with same name
        final Class<?> clazz = HtmlView.class; // do NOT use getClass() here, there may be sub-classes!
        final String cssFile = clazz.getSimpleName()+".css";
        final InputStream css = clazz.getResourceAsStream(cssFile);
        try {
            localStyleSheet.loadRules(new InputStreamReader(css), null);
        }
        catch (Exception e) {
            throw new IllegalArgumentException("Can not load "+cssFile+" as resource of "+clazz+": "+e);
        }
        
        if (url != null) // set the page
            SwingUtilities.invokeLater(()-> { // let sub-classes finish their constructors before
                try {
                    setPage(url);
                }
                catch (IOException e) {
                    throw new RuntimeException(e);
                }
            });
    }
    
    /** Fix: the JDK implementation is wrongly searching for hyperlink elements. */
    @Override
    public void scrollToReference(String referenceId) {
        final Element element = getHtmlDocument().getElement(referenceId);
        if (element != null)
            scrollToStartOffset(element.getStartOffset());
    }
    
    /** Scrolls to an element's startOffset. */
    public void scrollToStartOffset(int elementStartOffset) {
        try {
            final Rectangle elementRectangle = (Rectangle) modelToView2D(elementStartOffset);
            if (elementRectangle != null) {
                final Rectangle visibleRectangle = getVisibleRect();
                final Rectangle scrollToRectangle = new Rectangle(
                        0,
                        elementRectangle.y,
                        visibleRectangle.width,
                        visibleRectangle.height);
                // seems to be necessary to defer the scroll to make it work
                SwingUtilities.invokeLater(() -> scrollRectToVisible(scrollToRectangle));
            }
        }
        catch (BadLocationException e) {
            UIManager.getLookAndFeel().provideErrorFeedback(this);
        }
    }
    
    /** Overridden to catch event when page was fully loaded. */
    @Override
    public void setPage(URL url) throws IOException {
        startLoadingPage();
        
        final PropertyChangeListener loadFinishedListener = new PropertyChangeListener() {
            @Override
            public void propertyChange(PropertyChangeEvent event)   {
                if (event.getPropertyName().equals("page")) { // no constant exists for this!
                    removePropertyChangeListener(this); // stop listening
                    endLoadingPage();
                }
            }
        };
        addPropertyChangeListener(loadFinishedListener);

        super.setPage(url);
    }

    /** Called from event dispatch thread when setPage() starts. Does nothing, to be overridden. */
    protected void startLoadingPage() {
    }

    /** Called from event dispatch thread when setPage() has loaded all HTML. Does nothing, to be overridden. */
    protected void endLoadingPage() {
    }

    /** @return the rendered HTML-document, applying the cast-operator. */
    protected final HTMLDocument getHtmlDocument() {
        return (HTMLDocument) getDocument();
    }
    
    
    /**
     * HTMLEditorKit with private local CSS styles.
     * Bugfix for global styles in AppContext that affect even JLabel HTML texts.
     * @see https://stackoverflow.com/questions/43408539/how-does-one-properly-initialize-a-jtextpane-stylesheet-so-no-other-html-enable
     */
    private static class HtmlEditorKitWithLocalStyles extends HTMLEditorKit
    {
        private StyleSheet styleSheet;
        
        /** Overridden to return the private local style-sheet. */
        @Override
        public StyleSheet getStyleSheet() {
            return styleSheet;
        }
        
        /** Overridden to set the private local style-sheet. */
        @Override
        public void setStyleSheet(StyleSheet styleSheet) {
            this.styleSheet = styleSheet;
        }
        
        /** Delivers static global JDK styles. */
        StyleSheet getGlobalStyleSheet() {
            return super.getStyleSheet();
        }
    }
}