package fri.music.swingutils.text;

import java.awt.Rectangle;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.net.URL;
import javax.swing.JEditorPane;
import javax.swing.UIManager;
import javax.swing.text.BadLocationException;
import javax.swing.text.Element;
import javax.swing.text.html.HTMLDocument;
import javax.swing.text.html.HTMLEditorKit;
import javax.swing.text.html.StyleSheet;

/**
 * HTML only!
 * JEditorPane improvements concerning URLs with "ref" (HTML anchors).
 */
public class EditorPane extends JEditorPane
{
    public EditorPane(URL url) throws IOException {
        super(); // do not pass URL (setPage) before constructor is done
        
        setContentType("text/html"); // do not let setPage() do this
        
        // some style corrections
        final HTMLEditorKit kit = (HTMLEditorKit) getEditorKit();
        final StyleSheet css = kit.getStyleSheet();
        css.addRule("h1 { text-decoration: underline; }");
        css.addRule("p { margin-top: 0; margin-bottom: 10; }");
        // margin-top 15 for <p> would create a big gap between <h1> and <p>

        setEditable(false);
        
        setPage(url);
    }
    
    /** Listen for page loaded to jump to an optional URL reference (HTML anchor). */
    @Override
    public void setPage(URL url) throws IOException {
        final String anchorRef = url.getRef();
        if (anchorRef != null && anchorRef.length() > 0) {
            final PropertyChangeListener loadFinishedListener = new PropertyChangeListener() {
                @Override
                public void propertyChange(PropertyChangeEvent e)   {
                    if (e.getPropertyName().equals("page")) {
                        //SwingUtilities.invokeLater(() -> scrollToReference(anchorRef)); // does NOT work!
                        scrollToReference(anchorRef);
                        removePropertyChangeListener(this);
                    }
                }
            };
            addPropertyChangeListener(loadFinishedListener);
        }
        
        super.setPage(url);
    }
    
    /** Fix: the super-class implementation is wrongly searching for hyperlink elements. */
    @Override
    public void scrollToReference(String reference) {
        final HTMLDocument document = (HTMLDocument) getDocument();
        final Element element = document.getElement(reference);
        if (element != null) {
            try {
                final int pos = element.getStartOffset();
                final Rectangle viewRectangle = (Rectangle) modelToView2D(pos);
                if (viewRectangle != null) {
                    final Rectangle visibleRectangle = getVisibleRect();
                    viewRectangle.height = visibleRectangle.height;
                    scrollRectToVisible(viewRectangle);
                    setCaretPosition(pos);
                }
            }
            catch (BadLocationException e) {
                UIManager.getLookAndFeel().provideErrorFeedback(this);
            }
        }
    }
}