package fri.music.utils.swing.window;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.WindowEvent;
import java.net.URL;
import java.util.Hashtable;
import java.util.Map;
import java.util.Objects;
import javax.swing.AbstractAction;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.SwingUtilities;
import fri.music.HtmlResources;
import fri.music.utils.StringUtil;
import fri.music.utils.swing.KeyStrokeUtil;
import fri.music.utils.swing.text.HtmlBrowser;

/**
 * Shows text in non-modal dialogs.
 */
public class DialogStarter
{
    private static Map<String,Point> dialogMap = new Hashtable<>(); // cascade dialogs
    private static Map<Point,Point> locationMap = new Hashtable<>();
    
    /**
     * Opens a non-modal dialog showing given HTML text in a scroll-pane.
     * @param title the text to show in the dialog's title bar.
     * @param htmlUrl the address of the HTML document to render.
     * @param parent required, a parent Component in same Window where to locate dialog relatively.
     * @param size optional, the wanted dimension.
     */
    public static JDialog htmlDialog(String title, Component parent, URL htmlUrl, Dimension size) {
        return start(title, parent, buildHtmlView(htmlUrl), size, false);
    }

    /**
     * Opens a non-modal dialog showing given plain text in a scroll-pane.
     * @param title the text to show in the dialog's title bar.
     * @param plainText the text to render.
     * @param parent required, a parent Component in same Window where to locate dialog relatively.
     * @param size optional, the dimension when different from 600 x 460.
     */
    public static JDialog textDialog(String title, Component parent, String plainText, Dimension size) {
        return start(title, parent, buildPlainTextArea(plainText), size, true);
    }

    /**
     * Opens a non-modal dialog showing given component.
     * @param title text for title-bar.
     * @param componentToShow the panel to render, expected to already have a scoll-pane when needed.
     * @param parent the parent Component to show over.
     * @param relativeToParent true for showing dialog at location relative to parent (not window).
     * @return the created dialog window.
     */
    public static JDialog start(String title, Component parent, JComponent componentToShow, boolean relativeToParent) {
        return start(title, parent, componentToShow, null, relativeToParent);
    }
    
    /**
     * Opens a non-modal dialog showing given component.
     * @param title text for title-bar.
     * @param componentToShow the panel to render, expected to already have a scoll-pane when needed.
     * @param parent the parent Component to show over.
     * @param size the wanted size of the dialog.
     * @param relativeToParent true for showing dialog at location relative to parent (not window).
     * @return the created dialog window.
     */
    public static JDialog start(String title, Component parent, JComponent componentToShow, Dimension size, boolean relativeToParent) {
        return start(title, parent, componentToShow, size, null, relativeToParent);
    }
    
    /**
     * Opens a non-modal dialog showing given component.
     * @param title text for title-bar.
     * @param componentToShow the panel to render, expected to already have a scoll-pane when needed.
     * @param parent the parent Component to show over.
     * @param size the wanted size of the dialog.
     * @param location the wanted location of the dialog.
     * @param relativeToParent true for showing dialog at location relative to parent (not window).
     * @return the created dialog window.
     */
    public static JDialog start(
            String title, 
            Component parent, 
            JComponent componentToShow, 
            Dimension size, 
            Point location, 
            boolean relativeToParent)
    {
        final Window window = (parent instanceof Window) ? (Window) parent : SwingUtilities.windowForComponent(Objects.requireNonNull(parent));
        final JDialog dialog = new JDialog(Objects.requireNonNull(window), title);
        dialog.getContentPane().add(componentToShow);
        
        KeyStrokeUtil.install(
                componentToShow, 
                JComponent.WHEN_IN_FOCUSED_WINDOW,
                "closeDialogAction", 
                KeyEvent.VK_ESCAPE,
                new AbstractAction() {
                    @Override
                    public void actionPerformed(ActionEvent e) {
                        dialog.dispatchEvent(new WindowEvent(dialog, WindowEvent.WINDOW_CLOSING));
                    }
                });
        
        FrameStarter.setSize(dialog, componentToShow, size);
        setLocation(dialog, location, relativeToParent ? parent : window);
        
        dialog.setVisible(true);
        componentToShow.requestFocusInWindow(); // for immediately sizing font by keyboard
        
        return dialog;
    }

    
    private static void setLocation(JDialog dialog, Point location, Component relativeToComponent) {
        if (location != null) {
            dialog.setLocation(location);
        }
        else {
            dialog.setLocationRelativeTo(relativeToComponent);
            
            final String dialogTitle = dialog.getTitle();
            if (dialogTitle != null && dialogTitle.trim().length() > 0) { // check for cascading
                // cut off trailing numbers
                final String titleWithoutNumber = StringUtil.getUntilLastNumber(dialogTitle.trim());
                final Point locationForTitle = dialogMap.get(titleWithoutNumber);
                
                if (locationForTitle != null) { // cascade window to next point
                    Point previousLocation = locationMap.get(locationForTitle);
                    final Point cascadingPoint = FrameStarter.nextCascadingPoint(previousLocation, dialog);
                    dialog.setLocation(cascadingPoint);
                    locationMap.put(locationForTitle, cascadingPoint);
                }
                else { // initialize cascading
                    Point currentLocation = dialog.getLocation();
                    dialogMap.put(titleWithoutNumber, currentLocation);
                    locationMap.put(currentLocation, currentLocation);
                }
            }
        }
    }
    
    /**
     * Builds a panel containing given HTML.
     * @param htmlUrl the HTML resource to render in returned component.
     * @return a scroll-able navigation-able panel containing given HTML resource.
     */
    private static JComponent buildHtmlView(URL htmlUrl) {
        return new HtmlBrowser(htmlUrl, HtmlResources.class);
    }

    /**
     * Builds a text-area containing given plain text.
     * @param text the plain text to render in returned component.
     * @return a scroll-able text-area containing given HTML text.
     */
    private static JComponent buildPlainTextArea(String text) {
        final JTextArea textArea = new JTextArea();
        textArea.setToolTipText("Use Ctrl-A and Ctrl-C to Copy the Whole Text");
        textArea.setEditable(false);
        textArea.setText(text);
        textArea.setCaretPosition(0); // scroll back to top
        return new JScrollPane(textArea);
    }
}