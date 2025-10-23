package fri.music.utils.swing.text;

import java.awt.Rectangle;
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
 * Please do NOT rename this class unless you also rename 
 * <code>HtmlView.css</code> in <code>src/main/resources</code>.
 */
public class HtmlView extends JEditorPane
{
    public HtmlView(URL url) {
        super(); // do not pass URL before constructor is done
        
        setEditable(false);
        
        // do not share CSS with all other HTML views like JLabel
        final HtmlEditorKitWithLocalStyles editorKit = new HtmlEditorKitWithLocalStyles();
        final StyleSheet localStyleSheet = new StyleSheet();
        localStyleSheet.addStyleSheet(editorKit.getGlobalStyleSheet()); // merge JDK default styles into empty sheet
        editorKit.setStyleSheet(localStyleSheet);
        
        setEditorKit(editorKit); // this replaces setContentType("text/html"); do not let setPage() do this!
        
        final InputStream css = getClass().getResourceAsStream(HtmlView.class.getSimpleName()+".css");
        final StyleSheet styleSheet = ((HTMLEditorKit) getEditorKit()).getStyleSheet();
        try {
            styleSheet.loadRules(new InputStreamReader(css), null);
            
            SwingUtilities.invokeLater(()-> { // let sub-classes finish their constructors before setPage()
                try {
                    setPage(url);
                }
                catch (IOException e) {
                    throw new RuntimeException(e);
                }
            });
        }
        catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
    
    protected final HTMLDocument getHtmlDocument() {
        return (HTMLDocument) getDocument();
    }
    
    /** Fix: the JDK implementation is wrongly searching for hyperlink elements. */
    @Override
    public void scrollToReference(String reference) {
        final Element element = getHtmlDocument().getElement(reference);
        if (element != null)
            scrollToStartOffset(element.getStartOffset());
    }
    
    /** Scrolls to an element's startOffset. */
    public void scrollToStartOffset(int startOffset) {
        try {
            final Rectangle viewRectangle = (Rectangle) modelToView2D(startOffset);
            if (viewRectangle != null) {
                final Rectangle visibleRectangle = getVisibleRect();
                viewRectangle.height = visibleRectangle.height;
                scrollRectToVisible(viewRectangle);
                setCaretPosition(startOffset);
            }
        }
        catch (BadLocationException e) {
            UIManager.getLookAndFeel().provideErrorFeedback(this);
        }
    }
    

    /**
     * Bugfix for global styles that affect even JLabel HTML texts, see
     * https://stackoverflow.com/questions/43408539/how-does-one-properly-initialize-a-jtextpane-stylesheet-so-no-other-html-enable
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