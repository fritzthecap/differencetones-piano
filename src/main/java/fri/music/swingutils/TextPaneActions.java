package fri.music.swingutils;

import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import javax.swing.Action;
import javax.swing.JComponent;
import javax.swing.JTextPane;
import javax.swing.KeyStroke;
import javax.swing.text.DefaultEditorKit;
import javax.swing.text.JTextComponent;
import javax.swing.text.Keymap;
import javax.swing.text.MutableAttributeSet;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyledDocument;

public class TextPaneActions extends TextFontActions
{
    private int fontSize;
    
    public TextPaneActions(JTextPane textPane) {
        contextMenu.add(buildCopyAction(textPane, textPane.getKeymap()));
        contextMenu.add(buildFontMenu(textPane, textPane.getKeymap()));
        
        textPane.addMouseListener(new MouseAdapter() {
            @Override
            public void mousePressed(MouseEvent e) {
                showContextMenu(e);
            }
            @Override
            public void mouseReleased(MouseEvent e) {
                showContextMenu(e);
            }
        });
        
        magnifyFont(true, textPane); // initial font-size is not correct, so jump to 13
    }

    private void showContextMenu(MouseEvent e) {
        if (e.isPopupTrigger())
            contextMenu.show((JComponent) e.getSource(), e.getX(), e.getY());
    }
    
    private Action buildCopyAction(final JTextPane textPane, Keymap keymap)   {
        final KeyStroke key = KeyStroke.getKeyStroke(KeyEvent.VK_C, InputEvent.CTRL_DOWN_MASK);
        final Action copy = new DefaultEditorKit.CopyAction();
        copy.putValue(Action.NAME, "Copy (Ctrl-C)");
        keymap.addActionForKeyStroke(key, copy);
        
        return copy;
    }
    
    @Override
    protected void magnifyFont(boolean bigger, JTextComponent textComponent) {
        final JTextPane textPane = (JTextPane) textComponent;
        final MutableAttributeSet attributeSet = textPane.getInputAttributes();
        
        if (fontSize <= 0)
            fontSize = StyleConstants.getFontSize(attributeSet);
        else if (fontSize <= 8 && bigger == false || fontSize >= 28 && bigger == true)
            return; // is no more readable
        
        fontSize = nextFontSize(bigger, fontSize);
        
        StyleConstants.setFontSize(attributeSet, fontSize);
        final StyledDocument styledDocument = textPane.getStyledDocument();
        styledDocument.setCharacterAttributes(0, styledDocument.getLength() + 1, attributeSet, false);
    }
    
    private int nextFontSize(boolean bigger, int fontSize) {
        // -> 9/8 10/11 12/13 14/15 18/19 24/25
        if (bigger) {
            if (fontSize <= 8)
                return 9;
            if (fontSize <= 10)
                return 11;
            if (fontSize <= 12)
                return 13;
            if (fontSize <= 14)
                return 15;
            if (fontSize <= 18)
                return 19;
            return 25;
        }
        else {
            if (fontSize >= 25)
                return 24;
            if (fontSize >= 19)
                return 18;
            if (fontSize >= 15)
                return 14;
            if (fontSize >= 13)
                return 12;
            if (fontSize >= 11)
                return 10;
            return 8;
        }
    }
}