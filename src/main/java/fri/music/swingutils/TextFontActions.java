package fri.music.swingutils;

import java.awt.event.ActionEvent;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JComponent;
import javax.swing.JMenu;
import javax.swing.JPopupMenu;
import javax.swing.KeyStroke;
import javax.swing.text.JTextComponent;
import javax.swing.text.Keymap;

/**
 * Base class for both JTextArea and JTextPane to make bigger or smaller fonts.
 */
public abstract class TextFontActions
{
    /** An empty context menu for text components. */
    public final JPopupMenu contextMenu = new JPopupMenu();
    
    /**
     * Actually makes font bigger or smaller. To be implemented by sub-casses.
     * @param bigger true for bigger, false for smaller.
     * @param textComponent either JTextArea / JTextField or JTextPane.
     */
    public abstract void magnifyFont(boolean bigger, JTextComponent textComponent);
    
    /** To be called by sub-classes that want a font menu. */
    protected final JMenu buildFontMenu(final JTextComponent textComponent, Keymap keymap)  {
        final Action fontBigger = new AbstractAction("+ (Ctrl-Plus)") {
            @Override
            public void actionPerformed(ActionEvent e) {
                magnifyFont(true, textComponent);
            }
        };
        KeyStroke key = KeyStroke.getKeyStroke(KeyEvent.VK_PLUS, InputEvent.CTRL_DOWN_MASK);
        keymap.addActionForKeyStroke(key, fontBigger);
        
        final Action fontSmaller = new AbstractAction("- (Ctrl-Minus)") {
            @Override
            public void actionPerformed(ActionEvent e) {
                magnifyFont(false, textComponent);
            }
        };
        key = KeyStroke.getKeyStroke(KeyEvent.VK_MINUS, InputEvent.CTRL_DOWN_MASK);
        keymap.addActionForKeyStroke(key, fontSmaller);
        
        final JMenu fontMenu = new JMenu("Font");
        fontMenu.add(fontBigger);
        fontMenu.add(fontSmaller);
        
        return fontMenu;
    }
    
    /** Called before showing popup menu. Does nothing, to be overridden. */
    protected void enableActions(JTextComponent textComponent) {
    }
    
    protected boolean showContextMenu(MouseEvent e) {
        final boolean isPopupTrigger = e.isPopupTrigger();
        if (isPopupTrigger) {
            enableActions((JTextComponent) e.getSource());
            contextMenu.show((JComponent) e.getSource(), e.getX(), e.getY());
        }
        return isPopupTrigger;
    }
}