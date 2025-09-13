package fri.music.swingutils;

import java.awt.event.ActionEvent;
import java.awt.event.InputEvent;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JComponent;
import javax.swing.JMenu;
import javax.swing.JPopupMenu;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.text.JTextComponent;
import javax.swing.text.Keymap;

/**
 * Base class for both JTextArea and JEditorPane to make bigger or smaller fonts.
 */
public abstract class TextFontActions
{
    /** An empty context menu to be filled with text-actions. */
    public final JPopupMenu contextMenu = new JPopupMenu();
    
    /**
     * Provides an empty context menu and font actions.
     * @param textComponent the text-compoinent to listen to for mouse- and key -events.
     */
    public TextFontActions(JTextComponent textComponent) {
        // we need to listen to any key event for enabling actions
        textComponent.addKeyListener(new KeyAdapter() {
            @Override
            public void keyReleased(KeyEvent e) {
                enableActions((JTextComponent) e.getSource());
            }
        });
        
        // we need to listen to any mouse event for enabling actions
        textComponent.addMouseListener(new MouseAdapter() {
            @Override
            public void mousePressed(MouseEvent e) {
                showContextMenu(e);
            }
            @Override
            public void mouseReleased(MouseEvent e) {
                if (showContextMenu(e) == false) // listen to text-selection via mouse-drag
                    enableActions((JTextComponent) e.getSource());
            }
        });
    }
    
    /**
     * Makes font bigger or smaller. To be implemented by sub-classes.
     * @param bigger true for bigger, false for smaller.
     * @param textComponent the text-component where to magnify or reduce font.
     */
    public abstract void magnifyFont(boolean bigger, JTextComponent textComponent);
    
    /** To be called by sub-classes that want a font menu. */
    protected final JMenu buildFontMenu(Keymap keymap)  {
        final Action fontBigger = new AbstractAction("+ (Ctrl-Plus)") {
            @Override
            public void actionPerformed(ActionEvent e) {
                magnifyFont(true, (JTextComponent) e.getSource());
            }
        };
        KeyStroke key = KeyStroke.getKeyStroke(KeyEvent.VK_PLUS, InputEvent.CTRL_DOWN_MASK);
        keymap.addActionForKeyStroke(key, fontBigger);
        
        final Action fontSmaller = new AbstractAction("- (Ctrl-Minus)") {
            @Override
            public void actionPerformed(ActionEvent e) {
                magnifyFont(false, (JTextComponent) e.getSource());
            }
        };
        key = KeyStroke.getKeyStroke(KeyEvent.VK_MINUS, InputEvent.CTRL_DOWN_MASK);
        keymap.addActionForKeyStroke(key, fontSmaller);
        
        final JMenu fontMenu = new JMenu("Font");
        fontMenu.add(fontBigger);
        fontMenu.add(fontSmaller);
        
        return fontMenu;
    }
    
    /**
     * A default implementation to be called from
     * <code>MouseListener.mousePressed()</code> AND <code>MouseListener.mouseReleased()</code>.
     * This method calls <code>enableActions()</code> before showing the popup-menu.
     * @param e the MouseEvent on some text area this was installed upon.
     * @return true when mouse event was a popup-trigger, else false.
     */
    protected boolean showContextMenu(MouseEvent e) {
        if (SwingUtilities.isRightMouseButton(e))
            ((JComponent) e.getSource()).requestFocus(); // else right click would not focus text-area
        
        final boolean isPopupTrigger = e.isPopupTrigger();
        if (isPopupTrigger) {
            enableActions((JTextComponent) e.getSource()); // before showing popup menu
            contextMenu.show((JComponent) e.getSource(), e.getX(), e.getY());
        }
        
        return isPopupTrigger;
    }
    
    /** Called before showing popup menu. Does nothing, to be overridden. */
    protected void enableActions(JTextComponent textComponent) {
    }
}