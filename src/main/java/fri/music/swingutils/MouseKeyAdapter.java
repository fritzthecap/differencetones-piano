package fri.music.swingutils;

import java.awt.Component;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

/**
 * Handles both mouse- and key-events in one class.
 * Only "pressed" and "released" key-events are managed, not "typed",
 * and only SPACE and the popup-triggers F4 and Shift-F10 are accepted.
 * These keys are redirected to the according <code>MouseListener</code> methods
 * via a generated <code>MouseEvent</code>.
 * <p/>
 * For mouse-events, popup-trigger can be true on either "pressed"
 * or "released", this is platform-specific (WINDOWS, UNIX, ...),
 * implementations must be flexible to catch both cases.
 * <p/>
 * The keyboard sends repeated "pressed" events as long as the key is down,
 * followed by a single "released" when it goes up.
 * This effect <b>was removed here</b>, thus there is only the first "pressed"
 * with a single "released" event afterwards. All press- or release-events
 * of other keys in-between are ignored.
 */
public class MouseKeyAdapter extends MouseAdapter implements KeyListener
{
    private boolean keyIsDown; // = false by default
    private int keyCode = -1;
    
    // interface KeyListener
    
    /** Delegates to mousePressed() with a generated MouseEvent when no other key is down. */
    @Override
    public final void keyPressed(KeyEvent e) {
        if (isIgnorableKey(e) == false && isRepeatedKeyPress() == false) {
            mousePressed(convertToMouseEvent(e));
            setPressedState(e);
        }
    }
    
    /** Delegates to mouseReleased() with a generated MouseEvent. */
    @Override
    public final void keyReleased(KeyEvent e) {
        if (isIgnorableKey(e) == false && isMatchingKeyRelease(e) == true) {
            setPressedState(null);
            mouseReleased(convertToMouseEvent(e));
        }
    }

    /** Does nothing, to be overridden. Mind: this may be called before or after release! */
    @Override
    public void keyTyped(KeyEvent e) {
    }
    
    
    /** @return true if given key is Shift-F10 or F4, else false. */
    protected boolean isPopupTrigger(KeyEvent e) {
        return
            (e.getKeyCode() == KeyEvent.VK_F4 && e.getModifiersEx() == 0) ||
            (e.getKeyCode() == KeyEvent.VK_F10 && (e.getModifiersEx() & KeyEvent.SHIFT_DOWN_MASK) != 0); 
    }
    
    /** @return whether given key should be ignored. Only ENTER, SPACE and popup-triggers are accepted. */
    protected boolean isIgnorableKey(KeyEvent e) {
        switch (e.getKeyCode()) {
            // case KeyEvent.VK_ENTER:
            case KeyEvent.VK_SPACE: // play tone
                return e.getModifiersEx() != 0; // ignore SPACE and ENTER when modifiers
        }
        return isPopupTrigger(e) == false;
    }
    
    /** @return a MouseEvent generated from given KeyEvent. */
    protected final MouseEvent convertToMouseEvent(KeyEvent event) {
        final boolean isKeyPress   = (event.getID() == KeyEvent.KEY_PRESSED);
        final boolean isKeyRelease = (event.getID() == KeyEvent.KEY_RELEASED);
        
        final boolean popupTrigger = (isPopupTrigger(event) && isKeyRelease);
                // when doing this on PRESSED, the RELEASED event would
                // go to the popup-menu, which would ignore it,
                // and losing the event would lock the keyIsDown state forever
        
        return new MouseEvent(
                (Component) event.getSource(), // source
                isKeyPress ? MouseEvent.MOUSE_PRESSED : isKeyRelease ? MouseEvent.MOUSE_RELEASED : MouseEvent.MOUSE_CLICKED, // ID
                event.getWhen(), // time
                event.getModifiersEx(),
                2, 2, /// x, y coordinates
                1, // click count
                popupTrigger,
                popupTrigger ? MouseEvent.BUTTON3 : MouseEvent.BUTTON1);
    }
    
    
    private boolean isRepeatedKeyPress() {
        return keyIsDown; // ignore any key-press as long as keyIsDown
    }
    
    private boolean isMatchingKeyRelease(KeyEvent e) {
        return keyIsDown && keyCode == e.getKeyCode();
    }
    
    private void setPressedState(KeyEvent event) {
        keyIsDown = (event != null);
        keyCode = keyIsDown ? event.getKeyCode() : -1;
    }
}