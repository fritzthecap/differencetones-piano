package fri.music.utils.swing.window;

import java.awt.Window;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.util.ArrayList;
import java.util.List;

/**
 * A window-listener that can aggregate windows that all
 * get closed as soon as the listener receives <code>windowClosing()</code>.
 */
public class WindowClosingManager extends WindowAdapter
{
    private List<Window> windows = new ArrayList<>();
    
    /**
     * Adds the given listener to given window,
     * but also will call the listener when this' windowClosing() gets called,
     * in case the window is visible then.
     * @param window
     * @param windowListener
     */
    public void add(Window window, WindowListener windowListener) {
        window.addWindowListener(windowListener);
        windows.add(window);
    }
    
    @Override
    public void windowClosing(WindowEvent e) {
        for (Window window : windows)
            if (window.isVisible()) // has not called windowClosing by itself
                window.dispatchEvent(new WindowEvent(window, WindowEvent.WINDOW_CLOSING));
        
        windows.clear(); // be reusable for new starts
    }
}