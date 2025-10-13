package fri.music.utils.swing.text;

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
import javax.swing.SwingUtilities;
import javax.swing.text.JTextComponent;
import fri.music.utils.swing.KeyStrokeUtil;

/**
 * Base class for any <code>JTextComponent</code> to make bigger or smaller fonts.
 * Mind that the font menu will not be automatically in provided context-menu,
 * call <code>buildFontMenu()</code> to get it to the intended menu position.
 */
public abstract class FontActions
{
    /** An empty context menu to be filled with text-actions. */
    public final JPopupMenu contextMenu = new JPopupMenu();
    
    /**
     * Provides an empty context menu and font actions.
     * @param textComponent the text-compoinent to listen to for mouse- and key -events.
     */
    public FontActions(JTextComponent textComponent) {
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
     * Makes font bigger or smaller. 
     * As this is specific to <code>JTextArea</code> and <code>EditorPane</code>,
     * this has to be implemented by sub-classes.
     * @param bigger true for bigger, false for smaller.
     * @param textComponent the text-component where to magnify or reduce font.
     */
    public abstract void magnifyFont(boolean bigger, JTextComponent textComponent);
    
    /**
     * To be called by sub-classes that want a font menu.
     * @param keymap the keyboard mapping of target JTextComponent.
     */
    protected final JMenu buildFontMenu(final JTextComponent textComponent)  {
        final Action fontBigger = new AbstractAction("+ (Ctrl-Plus)") {
            @Override
            public void actionPerformed(ActionEvent e) {
                magnifyFont(true, textComponent);
            }
        };
        KeyStrokeUtil.install(
                textComponent, 
                JComponent.WHEN_FOCUSED,
                "magnifyFont", 
                KeyEvent.VK_PLUS,
                InputEvent.CTRL_DOWN_MASK,
                fontBigger);
        
        final Action fontSmaller = new AbstractAction("- (Ctrl-Minus)") {
            @Override
            public void actionPerformed(ActionEvent e) {
                magnifyFont(false, textComponent);
            }
        };
        KeyStrokeUtil.install(
                textComponent, 
                JComponent.WHEN_FOCUSED,
                "reduceFont", 
                KeyEvent.VK_MINUS,
                InputEvent.CTRL_DOWN_MASK,
                fontSmaller);
        
        final JMenu fontMenu = new JMenu("Font Size");
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
    
    /**
     * Called on mouse-pressed and -released, and on key-released. Does nothing, to be overridden.
     * @param textComponent the component where the event happened.
     */
    protected void enableActions(JTextComponent textComponent) {
    }
}