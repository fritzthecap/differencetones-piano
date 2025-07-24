package fri.music.swingutils;

import javax.swing.JButton;

/** Utilities for Swing JButton. */
public final class ButtonUtil
{
    /** Clicks given button with a zero millisecond delay. Default would be 68 millis. */
    public static void doClick(JButton button) {
        button.doClick(0); // click without delay
    }

    /** Sets given button visually pressed and paints it, but triggers no mouse-click. */
    public static void press(JButton button) {
        button.getModel().setPressed(true);
        button.getModel().setArmed(true);
        button.paintImmediately(button.getVisibleRect());
    }
    
    /** Sets given button visually released and paints it, but triggers no mouse-click. */
    public static void release(JButton button) {
        button.getModel().setPressed(false);
        button.getModel().setArmed(false);
        button.paintImmediately(button.getVisibleRect());
    }

    private ButtonUtil() {} // do not instantiate
}
