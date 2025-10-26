package fri.music.utils.swing.layout;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.Window;
import javax.swing.SwingUtilities;

public class LocationUtil
{
    /**
     * Places given dialog window relative to given component
     * which must be contained in parentWindow.
     * The dialog will be shown horizontally in the middle of component.
     * @param dialog the window to place.
     * @param parentWindow the parent window of component.
     * @param component the component where to place relative to.
     * @param over true for placing over, false for placing under component.
     */
    public static void locateRelativeToComponent(Window dialog, Window parentWindow, Component component, boolean over)    {
        if (component == null) // setText() was never called
            return;
        
        final Point parentLocationOnScreen = parentWindow.getLocation();
        final Point componentPoint = SwingUtilities.convertPoint(
                component.getParent(), 
                component.getLocation(), 
                parentWindow);
        final Point topLeftCornerOnScreen = new Point(
                componentPoint.x + parentLocationOnScreen.x,
                componentPoint.y + parentLocationOnScreen.y);
        
        final Dimension dialogSize = dialog.getSize();
        final Dimension componentSize = component.getSize();
        
        final Point dialogPoint = over
                ? new Point(
                        topLeftCornerOnScreen.x + componentSize.width / 2 - dialogSize.width / 2,
                        topLeftCornerOnScreen.y - dialogSize.height)
                : new Point(
                        topLeftCornerOnScreen.x + componentSize.width / 2 - dialogSize.width / 2,
                        topLeftCornerOnScreen.y + componentSize.height);
        dialog.setLocation(dialogPoint);
    }
}