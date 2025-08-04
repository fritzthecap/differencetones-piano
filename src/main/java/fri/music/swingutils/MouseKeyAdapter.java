package fri.music.swingutils;

import java.awt.Component;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

/**
 * Handles both mouse- and key-events in one class.
 * Only "pressed" and "released" key events are managed, not "typed",
 * and only ENTER, SPACE, and the popup-triggers F4 and Shift-F10.
 * These are redirected to the according MouseListener methods with
 * a generated MouseEvent.
 * <p/>
 * For mouse-events, popup-trigger can be true on either "pressed"
 * or "released", this is platform-specific (WINDOWS, UNIX, ...).
 * Implementations must be flexible to catch both cases.
 * <p/>
 * Key-events repeat "pressed" as long as the key is down,
 * followed by a single "released" when it goes up.
 * This effect was removed here, thus there is only the first "pressed"
 * with a single "released" event afterwards.
 */
public class MouseKeyAdapter extends MouseAdapter implements KeyListener
{
    private boolean keyIsDown; // = false by default
    private int keyCode = -1;
    
    // interface KeyListener
    
    /** Delegates to mousePressed() with a generated MouseEvent. */
    @Override
    public final void keyPressed(KeyEvent e) {
        if (isIgnorableKey(e) == false && isRepeatedKeyPress(e) == false)
            mousePressed(convertToMouseEvent(e));
    }
    
    /** Delegates to mouseReleased() with a generated MouseEvent. */
    @Override
    public final void keyReleased(KeyEvent e) {
        if (isIgnorableKey(e) == false && keyCode == e.getKeyCode()) {
            keyIsDown = false;
            keyCode = -1;
            mouseReleased(convertToMouseEvent(e));
        }
    }
    
    /** Does nothing, to be overridden. Mind: this may be called before or after release! */
    @Override
    public void keyTyped(KeyEvent e) {
    }
    
    // helpers
    
    /** @return true if given key is Shift-F10 or F4, else false. */
    protected boolean isPopupTrigger(KeyEvent e) {
        return
            (e.getKeyCode() == KeyEvent.VK_F4 && e.getModifiersEx() == 0) ||
            (e.getKeyCode() == KeyEvent.VK_F10 && (e.getModifiersEx() & KeyEvent.SHIFT_DOWN_MASK) != 0); 
    }
    
    private boolean isIgnorableKey(KeyEvent e) {
        if (isPopupTrigger(e))
            return false;
        
        switch(e.getKeyCode()) {
            case KeyEvent.VK_ENTER:
            case KeyEvent.VK_SPACE: // play tone
                return e.getModifiersEx() != 0; // ignore SPACE and ENTER when modifiers
        }
        return true;
    }
    
    private boolean isRepeatedKeyPress(KeyEvent e) {
        if (keyIsDown == false) {
            keyIsDown = true;
            keyCode = e.getKeyCode();
            return false;
        }
        return true;
    }
    
    private MouseEvent convertToMouseEvent(KeyEvent event) {
        if (event.getID() != KeyEvent.KEY_PRESSED && event.getID() != KeyEvent.KEY_RELEASED)
            throw new IllegalArgumentException("Only KEY_PRESSED and KEY_RELEASED KeyEvent are supported: "+event.paramString());
        
        final boolean popupTrigger = 
                (isPopupTrigger(event) && event.getID() == KeyEvent.KEY_RELEASED);
        
        return new MouseEvent(
                (Component) event.getSource(),
                (event.getID() == KeyEvent.KEY_PRESSED ? MouseEvent.MOUSE_PRESSED : MouseEvent.MOUSE_RELEASED),
                event.getWhen(),
                event.getModifiersEx(),
                2, 2, /// x, y coordinates
                1, // click count
                popupTrigger,
                (popupTrigger ? MouseEvent.BUTTON3 : MouseEvent.BUTTON1));
    }
}