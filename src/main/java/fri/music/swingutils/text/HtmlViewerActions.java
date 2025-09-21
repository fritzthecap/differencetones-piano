package fri.music.swingutils.text;

import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.util.HashMap;
import java.util.Map;
import javax.swing.Action;
import javax.swing.JEditorPane;
import javax.swing.KeyStroke;
import javax.swing.text.DefaultEditorKit;
import javax.swing.text.Document;
import javax.swing.text.JTextComponent;
import javax.swing.text.Keymap;
import javax.swing.text.html.HTMLEditorKit;
import javax.swing.text.html.StyleSheet;

public class HtmlViewerActions extends TextFontActions
{
    private static final int MINIMAL_FONT_SIZE = 8;
    private static final int MAXIMAL_FONT_SIZE = 50;
    
    /** Font size mapping, name = HTML element tag, value = font-size of this element. */
    private static final Map<String,Integer> fontSizes = new HashMap<>();
    /** The amount of font magnification or reduction, in percent. */
    private static final int magnifyPercent = 10;
    /** Add all used HTML elements to style with their font size. */
    static {
        fontSizes.put("h1", 28); // header elements
        fontSizes.put("h2", 25);
        fontSizes.put("h3", 21);
        fontSizes.put("h4", 18);
        fontSizes.put("h5", 16);
        fontSizes.put("p",  14); // paragraph elements
        fontSizes.put("li", 14); // list elements
    }
    
    /**
     * Call this to set current font-sizes in given HTML viewer.
     * @param htmlViewer the HTML-area where to set mapped font-sizes.
     */
    private static void updateFontSizes(JEditorPane htmlViewer) {
        final HTMLEditorKit editorKit = (HTMLEditorKit) htmlViewer.getEditorKit();
        final StyleSheet styleSheet = editorKit.getStyleSheet();
        
        for (final Map.Entry<String,Integer> fontSize : fontSizes.entrySet()) {
            final String htmlElementName = fontSize.getKey();
            final int size = fontSize.getValue();
            
            final String rule = htmlElementName+" { font-size : "+size+" }";
            styleSheet.addRule(rule);
        }
        
        final String html = htmlViewer.getText();
        final int caretPosition = htmlViewer.getCaretPosition();
        
        final Document newDocument = editorKit.createDefaultDocument();
        htmlViewer.setDocument(newDocument);
        
        htmlViewer.setText(html);
        htmlViewer.setCaretPosition(caretPosition);
    }
    
    
    private final Action copy;
    
    /**
     * Attaches standard actions to given HTML-viewer.
     * @param htmlViewer the JEditorPane or JTextPane to add actions to.
     */
    public HtmlViewerActions(JEditorPane htmlViewer) {
        super(htmlViewer);
        
        contextMenu.add(this.copy = buildCopyAction(htmlViewer.getKeymap()));
        contextMenu.add(buildFontMenu(htmlViewer.getKeymap(), htmlViewer));
        
        // immediately adjust current fonts to fontSizes table
        HtmlViewerActions.updateFontSizes(htmlViewer);
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
}