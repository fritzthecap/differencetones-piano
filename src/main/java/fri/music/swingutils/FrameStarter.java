package fri.music.swingutils;

import java.awt.Dimension;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import javax.swing.JComponent;
import javax.swing.JFrame;

/**
 * Do not use <code>new JFrame()</code> because closing frames
 * can not be controlled that way and sound resources may remain
 * occupied in operating system.
 */
public class FrameStarter
{
    private static Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
    private static int titlebarHeight = 32;
    
    private static List<JFrame> frames = new ArrayList<>();
    private static boolean exiting = false;
    
    /** Runs an EXIT_ON_CLOSE frame. */
    public static JFrame start(String title, JComponent content) {
        return start(title, true, content);
    }
    
    /** Runs an EXIT_ON_CLOSE frame. */
    public static JFrame start(String title, JComponent content, Dimension dimension) {
        return start(title, true, content, null, dimension);
    }
    
    /** Runs a frame configured by given parameters. */
    public static JFrame start(String title, boolean exitOnClose, JComponent content) {
        return start(title, exitOnClose, content, null, null);
    }
    
    /** Runs a frame configured by given parameters. */
    public static JFrame start(
            String title, 
            boolean exitOnClose, 
            JComponent content, 
            WindowListener windowListener,
            Dimension dimension)
    {
        final JFrame frame = new JFrame(title);
        frame.getContentPane().add(Objects.requireNonNull(content));
        frame.setDefaultCloseOperation(exitOnClose ? JFrame.EXIT_ON_CLOSE : JFrame.DISPOSE_ON_CLOSE);
        
        if (windowListener != null)
            frame.addWindowListener(windowListener);
        
        // EXIT_ON_CLOSE will not call windowClosing() on still running frames, 
        // so manage this here to correctly release all sound resources
        frame.addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                final JFrame closingFrame = (JFrame) e.getSource();
                frames.remove(closingFrame);
                
                final boolean wasExiting = exiting;
                if (exiting == false)
                    exiting = (closingFrame.getDefaultCloseOperation() == JFrame.EXIT_ON_CLOSE);
                
                if (wasExiting == false && exiting == true) // must close open ones
                    for (JFrame frame : new ArrayList<>(frames)) // avoid concurrent access of list
                        for (WindowListener windowListener : frame.getWindowListeners())
                            if (windowListener != this)
                                windowListener.windowClosing(new WindowEvent(frame, WindowEvent.WINDOW_CLOSING));
            }
        });
        
        if (dimension == null)
            frame.pack();
        else
            frame.setSize(dimension);
        
        ensureVisibleSize(frame); // else tiny frame may be hard to find on screen
        
        final JFrame previousFrame = (frames.size() > 0) ? frames.get(frames.size() - 1) : null;
        frame.setLocation(nextCascadingPoint(previousFrame, frame));
        frame.setVisible(true);
        
        frames.add(frame);
        return frame;
    }

    private static void ensureVisibleSize(JFrame frame) {
        final Dimension size = frame.getSize();
        if (size.width < 30)
            size.width = 200;
        if (size.height < 30 + titlebarHeight) // title-bar height counts
            size.height = 200;
        frame.setSize(size);
    }

    private static Point nextCascadingPoint(JFrame previousFrame, JFrame newFrame)    {
        final Dimension d = newFrame.getSize();
        final Point p;
        if (previousFrame == null) { // go to middle of the screen
            p = new Point(
                    screenSize.width / 2 - d.width / 2,
                    screenSize.height / 2 - d.height / 2);
        }
        else { // cascade to last location
            final Point previous = previousFrame.getLocation();
            p = new Point(
                    previous.x + titlebarHeight,
                    previous.y + titlebarHeight);
        }
        return ensureWithinScreen(p, d);
    }
    
    private static Point ensureWithinScreen(Point p, Dimension d)   {
        if (p.x > screenSize.width - d.width || p.x < 0)
            p.x = 0;
        if (p.y > screenSize.height - d.height || p.y < 0)
            p.y = 0;
        return p;
    }

    
    /*public static void main(String[] args) {
        final JPanel content = new JPanel();
        final WindowListener closeListener = new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                System.out.println("My windowClosing was called on "+((JFrame) e.getSource()).getTitle());
            }
        };
        FrameStarter.start("DISPOSE Frame", false, content, closeListener, null);
        FrameStarter.start("EXIT 1 Frame", true, content, closeListener, null);
        FrameStarter.start("EXIT 2 Frame", true, content, closeListener, null);
    }*/
}