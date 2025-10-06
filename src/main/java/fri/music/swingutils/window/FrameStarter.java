package fri.music.swingutils.window;

import java.awt.Dimension;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.Window;
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
    private static final Dimension SCREEN_SIZE = Toolkit.getDefaultToolkit().getScreenSize();
    private static final Dimension MINIMUM_WINDOW_DIMENSION = new Dimension(150, 100);
    private static Integer TITLEBAR_HEIGHT; // 37
    
    private static final List<JFrame> frames = new ArrayList<>();
    private static final List<JFrame> nonLayoutRelevantFrames = new ArrayList<>();
    
    private static boolean exiting = false;
    
    /** Runs an EXIT_ON_CLOSE frame. */
    public static JFrame start(String title, JComponent content, Dimension dimension) {
        return start(title, true, content, null, dimension);
    }
    
    /** Runs a DISPOSE_ON_CLOSE frame configured by given parameters. */
    public static JFrame start(String title, JComponent content) {
        return start(title, false, content);
    }
    
    /** Runs a DISPOSE_ON_CLOSE frame configured by given parameters. */
    public static JFrame start(String title, JComponent content, WindowListener windowListener) {
        return start(title, false, content, windowListener);
    }
    
    /** Runs a frame configured by given parameters. */
    public static JFrame start(String title, boolean exitOnClose, JComponent content) {
        return start(title, exitOnClose, content, null);
    }
    
    /** Runs a frame configured by given parameters. */
    public static JFrame start(String title, boolean exitOnClose, JComponent content, WindowListener windowListener) {
        return start(title, exitOnClose, content, windowListener, null);
    }
    
    /** Runs a parent-dependent frame configured by given parameters. */
    public static JFrame start(JFrame parentLauncher, String title, JComponent content, WindowListener windowListener) {
        final JFrame frame = start(title, content, windowListener);
        if (parentLauncher != null)
            parentLauncher.addWindowListener(new WindowAdapter() {
                @Override
                public void windowClosing(WindowEvent e) {
                    frame.dispatchEvent(new WindowEvent(frame, WindowEvent.WINDOW_CLOSING));
                    frame.dispatchEvent(new WindowEvent(frame, WindowEvent.WINDOW_CLOSED));
                }
            });
        return frame;
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
        // so do this here to correctly release all sound resources
        frame.addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                final JFrame closingFrame = (JFrame) e.getSource();
                frames.remove(closingFrame);
                nonLayoutRelevantFrames.remove(closingFrame);
                
                final boolean wasExiting = exiting;
                if (exiting == false)
                    exiting = (closingFrame.getDefaultCloseOperation() == JFrame.EXIT_ON_CLOSE);
                
                if (wasExiting == false && exiting == true) // close all open frames and call their window-listeners
                    for (JFrame frame : new ArrayList<>(frames)) // avoid concurrent access of list
                        if (frame.isVisible())
                            frame.dispatchEvent(new WindowEvent(frame, WindowEvent.WINDOW_CLOSING));
            }
        });
        
        setSize(frame, content, dimension);
        
        final Window layoutRelevantFrame = findLayoutRelevantFrame();
        setLocation(frame, (layoutRelevantFrame != null) ? layoutRelevantFrame.getLocation() : null);
        
        frame.setVisible(true);
        
        frames.add(frame);
        return frame;
    }
    

    /** Call this to avoid that subsequent frames cascade to given frame. */
    public static void setNonLayoutRelevant(JFrame frame) {
        nonLayoutRelevantFrames.add(frame);
    }
    

    // methods reused by DialogStarter
    
    static Dimension setSize(Window window, JComponent content, Dimension dimension) {
        if (dimension == null)
            //window.pack(); // unreliable!
            window.setSize(addTitleBarHeight(content.getPreferredSize()));
        else
            window.setSize(dimension);
        
        return ensureVisibleSize(window); // else tiny frame may be hard to find on screen
    }
    
    static Point nextCascadingPoint(Point previousWindowLocation, Window newWindow)    {
        final Dimension sizeOfNewWindow = newWindow.getSize();
        final Point cascadingPoint;
        if (previousWindowLocation == null) // go to middle of the screen
            cascadingPoint = new Point(
                    SCREEN_SIZE.width / 2 - sizeOfNewWindow.width / 2,
                    SCREEN_SIZE.height / 2 - sizeOfNewWindow.height / 2);
        else // cascade to last location
            cascadingPoint = new Point(
                    previousWindowLocation.x + getTitlebarHeight(),
                    previousWindowLocation.y + getTitlebarHeight());

        return ensureWithinScreen(cascadingPoint, sizeOfNewWindow);
    }
    
    
    // private methods
    
    private static Point setLocation(Window newWindow, Point previousWindowLocation) {
        final Point point = nextCascadingPoint(previousWindowLocation, newWindow);
        newWindow.setLocation(point);
        return point;
    }

    private static Point ensureWithinScreen(Point point, Dimension windowSize)   {
        if (point.x > SCREEN_SIZE.width - windowSize.width || point.x < 0)
            point.x = 0;
        if (point.y > SCREEN_SIZE.height - windowSize.height || point.y < 0)
            point.y = 0;
        return point;
    }

    private static Dimension addTitleBarHeight(Dimension preferredSize) {
        return new Dimension(preferredSize.width, preferredSize.height + getTitlebarHeight());
    }

    private static Dimension ensureVisibleSize(Window window) {
        final Dimension size = window.getSize();
        Dimension adjustedSize = new Dimension(size); // use a clone object to adjust
        
        if (size.width < MINIMUM_WINDOW_DIMENSION.width)
            adjustedSize.width = MINIMUM_WINDOW_DIMENSION.width;
        
        if (size.height < MINIMUM_WINDOW_DIMENSION.height)
            adjustedSize.height = MINIMUM_WINDOW_DIMENSION.height;
        
        if (adjustedSize.width != size.width || adjustedSize.height != size.height)
            window.setSize(adjustedSize);
        
        return window.getSize();
    }

    private static Window findLayoutRelevantFrame() {
        for (int i = frames.size() - 1; i >= 0; i--) {
            final JFrame frame = frames.get(i);
            if (nonLayoutRelevantFrames.contains(frame) == false)
                return frame;
        }
        return null;
    }

    private static int getTitlebarHeight()   {
        if (TITLEBAR_HEIGHT == null)   {
            JFrame f = new JFrame();
            f.pack();
            TITLEBAR_HEIGHT = f.getRootPane().getLocation().y;
        }
        return TITLEBAR_HEIGHT;
    }
    
    
    /* public static void main(String[] args) {
        final javax.swing.JPanel content = new javax.swing.JPanel();
        final WindowListener closeListener = new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                System.out.println("My windowClosing was called on "+((JFrame) e.getSource()).getTitle());
            }
        };
        FrameStarter.start("DISPOSE Frame", false, content, closeListener, null);
        FrameStarter.start("EXIT 1 Frame", true, content, closeListener, null);
        FrameStarter.start("EXIT 2 Frame", true, content, closeListener, null);
    } */
}