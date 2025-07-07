package fri.music.swingutils;

import java.awt.Dimension;
import java.awt.Rectangle;
import javax.swing.JButton;

/** Utilities for Swing JButton. */
public final class ButtonUtil
{
    /** Clicks given button with a zero millisecond delay. Default would be 68 millis. */
    public static void doClick(JButton button) {
        button.doClick(0); // click without delay
    }

    /** Sets given button visually pressed, but triggers no mouse-click. */
    public static void press(JButton button) {
        Dimension size = button.getSize();
        button.getModel().setPressed(true);
        button.getModel().setArmed(true);
        button.paintImmediately(new Rectangle(0, 0, size.width, size.height));
    }
    
    /** Sets given button visually released, but triggers no mouse-click. */
    public static void release(JButton button) {
        Dimension size = button.getSize();
        button.getModel().setPressed(false);
        button.getModel().setArmed(false);
        button.paintImmediately(new Rectangle(0, 0, size.width, size.height));
    }

    private ButtonUtil() {} // do not instantiate
}
