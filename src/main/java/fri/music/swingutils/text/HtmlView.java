package fri.music.swingutils.text;

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
 * For CSS see <code>HtmlEditorPane.css</code> in <code>src/main/resources</code>.
 */
public class HtmlView extends JEditorPane
{
    public HtmlView(URL url) {
        super(); // do not pass URL before constructor is done
        
        setContentType("text/html"); // do not let setPage() do this
        setEditable(false);
        
        final InputStream css = getClass().getResourceAsStream(HtmlView.class.getSimpleName()+".css");
        final StyleSheet styleSheet = ((HTMLEditorKit) getEditorKit()).getStyleSheet();
        try {
            styleSheet.loadRules(new InputStreamReader(css), null);
            
            SwingUtilities.invokeLater(()-> { // let sub-classes finish constructor
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
    
    /** Fix: the JDK implementation is wrongly searching for hyperlink elements. */
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