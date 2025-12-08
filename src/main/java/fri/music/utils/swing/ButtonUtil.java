package fri.music.utils.swing;

import javax.swing.JButton;

/** Utilities for Swing JButton. */
public final class ButtonUtil
{
    /** Clicks given button with a zero millisecond press-time. Default would be 68 millis. */
    public static void doClick(JButton button) {
        button.doClick(0);
    }

    /** Sets given button visually pressed and paints it, but triggers no mouse-click. */
    public static void press(JButton button) {
        visualSelect(button, true);
        button.paintImmediately(button.getVisibleRect());
    }
    
    /** Sets given button visually released and paints it, but triggers no mouse-click. */
    public static void release(JButton button) {
        visualSelect(button, false);
        button.paintImmediately(button.getVisibleRect());
    }
    
    /** Presses or releases given button visually but doesn't paint it, and triggers no mouse-click. */
    public static void visualSelect(JButton button, boolean pressed) {
        // START keep order: this should not trigger an action!
        // See AbstractButton.doClick() for necessary order of calls to trigger an action
        if (pressed) {
            button.getModel().setPressed(pressed);
            button.getModel().setArmed(pressed);
        }
        else {
            button.getModel().setArmed(pressed);
            button.getModel().setPressed(pressed);
        }
        // END keep order
    }

    private ButtonUtil() {} // do not instantiate
}
