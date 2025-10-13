package fri.music.utils.swing;

import java.util.Objects;
import javax.swing.Action;
import javax.swing.JComponent;
import javax.swing.KeyStroke;

/**
 * Due to the long-lasting chaos with Swing keybord support,
 * here is a reliable way to install key presses.
 * Take care of the <code>condition</code>, this is crucial in some cases!
 */
public final class KeyStrokeUtil
{
    /**
     * Install a keyboard key action with no modifiers (Ctrl, Shift, ..).
     * @param component the component that will carry the key-stroke.
     * @param actionId a component-wide unique id for this action.
     * @param action the action to execute when key is pressed.
     * @param keyEvent one of KeyEvent.*, e.g. KeyEvent.ESCAPE.
     * @param condition one of JComponent.WHEN_*, e.g. JComponent.WHEN_FOCUSED.
     */
    public static void install(JComponent component, int condition, String actionId, int keyEvent, Action action) {
        install(component, condition, actionId, keyEvent, 0, action);
    }
    
    /**
     * Install a keyboard key action.
     * @param component the component that will carry the key-stroke.
     * @param actionId a component-wide unique id for this action.
     * @param action the action to execute when key is pressed.
     * @param keyEvent one of KeyEvent.*, e.g. KeyEvent.ESCAPE.
     * @param modifiers one or several combined of InputEvent.*, e.g. InputEvent.CTRL_MASK.
     * @param condition one of JComponent.WHEN_*, e.g. JComponent.WHEN_IN_FOCUSED_WINDOW.
     */
    public static void install(JComponent component, int condition, String actionId, int keyEvent, int modifiers, Action action) {
        final KeyStroke key = KeyStroke.getKeyStroke(keyEvent, modifiers);
        component.getInputMap(condition).put(Objects.requireNonNull(key), Objects.requireNonNull(actionId));
        component.getActionMap().put(actionId, Objects.requireNonNull(action));
    }
    
    private KeyStrokeUtil() {} // do not instantiate
}
