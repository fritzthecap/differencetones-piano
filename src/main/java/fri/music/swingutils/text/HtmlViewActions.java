package fri.music.swingutils.text;

import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;
import javax.swing.Action;
import javax.swing.JEditorPane;
import javax.swing.KeyStroke;
import javax.swing.text.DefaultEditorKit;
import javax.swing.text.Document;
import javax.swing.text.JTextComponent;
import javax.swing.text.Keymap;
import javax.swing.text.Style;
import javax.swing.text.html.CSS;
import javax.swing.text.html.HTMLDocument;
import javax.swing.text.html.HTMLEditorKit;
import javax.swing.text.html.StyleSheet;

/**
 * This class relies on the <code>HtmlEView.css</code> stylesheet that
 * should have been loaded initially by the given <code>HtmlView</code>.
 */
public class HtmlViewActions extends FontActions
{
    private static final int MINIMAL_FONT_SIZE = 8;
    private static final int MAXIMAL_FONT_SIZE = 50;
    
    /** The HTML element names whose font-sizes are managed here.*/
    private static final String [] fontElements = new String[] {
          "h1", // header elements
          "h2",
          "h3",
          "h4",
          "h5",
          "p", // paragraph elements
          "li", // list elements
    };
    /** Font size mapping, name = HTML element tag, value = integer font-size of this element. */
    private static final Map<String,Integer> fontSizes = new HashMap<>();
    /** The amount of font magnification or reduction, in percent. */
    private static final int magnifyPercent = 10;
    
    
    private final Action copy;
    
    /**
     * Attaches standard actions to given HTML-viewer.
     * @param htmlView the JEditorPane or JTextPane to add actions to.
     */
    public HtmlViewActions(JEditorPane htmlView) {
        super(htmlView);
        
        contextMenu.add(this.copy = buildCopyAction(htmlView.getKeymap()));
        contextMenu.add(buildFontMenu(htmlView.getKeymap(), htmlView));
        
        // immediately adjust current fonts to fontSizes table
        initializeFontSizes(htmlView);
    }
    
    @Override
    protected void enableActions(JTextComponent textComponent) {
        final boolean textIsSelected = (textComponent.getSelectionStart() != textComponent.getSelectionEnd());
        copy.setEnabled(textIsSelected);
    }
    
    @Override
    public void magnifyFont(boolean bigger, JTextComponent textComponent) {
        int minimalSize = Integer.MAX_VALUE;
        int maximalSize = 0;
        for (final Integer size : fontSizes.values()) {
            if (size < minimalSize)
                minimalSize = size;
            if (size > maximalSize)
                maximalSize = size;
        }
        
        if (minimalSize <= MINIMAL_FONT_SIZE && bigger == false || maximalSize >= MAXIMAL_FONT_SIZE && bigger == true)
            return; // deny smaller or bigger fonts
        
        for (final Map.Entry<String,Integer> fontSize : fontSizes.entrySet()) {
            final int oldSize = fontSize.getValue();
            final int delta = calculatePercent(oldSize);
            final int newSize;
            if (bigger) {
                newSize = oldSize + delta;
            }
            else { // going down, calculate balanced delta from an approximated lower value
                final int lowerSize = oldSize - delta;
                final int lowerDelta = calculatePercent(lowerSize);
                newSize = oldSize - lowerDelta;
            }
            fontSize.setValue(newSize);
        }
        
        // bring fonts to screen
        updateFontSizes((JEditorPane) textComponent);
    }


    private int calculatePercent(int size) {
        return (int) Math.round((double) size * (double) magnifyPercent / (double) 100);
    }
    
    private Action buildCopyAction(Keymap keymap)   {
        final KeyStroke key = KeyStroke.getKeyStroke(KeyEvent.VK_C, InputEvent.CTRL_DOWN_MASK);
        // EditorPane has no default copy-action in its actionMap, so create a new one
        final Action copyAction = new DefaultEditorKit.CopyAction();
        copyAction.putValue(Action.NAME, "Copy (Ctrl-C)");
        keymap.addActionForKeyStroke(key, copyAction);
        return copyAction;
    }

    private void initializeFontSizes(JEditorPane htmlView) {
        if (fontSizes.isEmpty()) { // will happen just once in application lifetime
            final HTMLEditorKit editorKit = (HTMLEditorKit) htmlView.getEditorKit();
            final StyleSheet styleSheet = editorKit.getStyleSheet();
            
            for (final String elementName : fontElements) {
                final Style rule = styleSheet.getRule(elementName);
                final Object attribute = rule.getAttribute(CSS.Attribute.FONT_SIZE);
                try {
                    final Integer fontSize = Integer.valueOf(attribute.toString());
                    fontSizes.put(elementName, fontSize);
                    //System.out.println(elementName+" "+CSS.Attribute.FONT_SIZE+"="+fontSize);
                }
                catch (Exception e) { // NumberFormatException, NullPointerException
                    System.err.println("Could not decode font-size of element "+elementName+": "+e);
                }
            }
        }
    }

    /**
     * Call this to set current font-sizes in given HTML view.
     * @param htmlView the HTML-area where to set mapped font-sizes.
     */
    private void updateFontSizes(JEditorPane htmlView) {
        final HTMLEditorKit editorKit = (HTMLEditorKit) htmlView.getEditorKit();
        final StyleSheet styleSheet = editorKit.getStyleSheet();
        
        for (final Map.Entry<String,Integer> fontSize : fontSizes.entrySet()) {
            final String htmlElementName = fontSize.getKey();
            final int size = fontSize.getValue();
            
            final String rule = htmlElementName+" { font-size : "+size+" }";
            styleSheet.addRule(rule);
        }
        
        final String html = htmlView.getText();
        final int caretPosition = htmlView.getCaretPosition();
        final URL url = htmlView.getPage();
        
        final HTMLDocument newDocument = (HTMLDocument) editorKit.createDefaultDocument();
        htmlView.setDocument(newDocument);
        
        // Fix for image loading bug after font resize
        if (url != null) { // add base and stream-desc, else image loading will fail next time
            newDocument.setBase(url);
            newDocument.putProperty(Document.StreamDescriptionProperty, url);
        }
        
        htmlView.setText(html);
        htmlView.setCaretPosition(caretPosition);
    }
}