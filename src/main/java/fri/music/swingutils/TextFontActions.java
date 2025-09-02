package fri.music.swingutils;

import java.awt.event.ActionEvent;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JMenu;
import javax.swing.JPopupMenu;
import javax.swing.KeyStroke;
import javax.swing.text.JTextComponent;
import javax.swing.text.Keymap;

public abstract class TextFontActions
{
    public final JPopupMenu contextMenu = new JPopupMenu();
    
    protected abstract void magnifyFont(boolean bigger, JTextComponent textComponent);
    
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
}