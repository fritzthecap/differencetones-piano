package fri.music.swingutils.window;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.net.URL;
import java.util.Objects;
import javax.swing.AbstractAction;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.SwingUtilities;
import fri.music.HtmlResources;
import fri.music.swingutils.KeyStrokeUtil;
import fri.music.swingutils.text.HtmlBrowser;

/**
 * Shows text in non-modal dialogs.
 */
public class DialogStarter
{
    /**
     * Opens a non-modal dialog showing given HTML text in a scroll-pane.
     * @param title the text to show in the dialog's title bar.
     * @param htmlText the text to render in returned JTextPane.
     * @param parent required, a parent Component in same Window where to locate dialog relatively.
     * @param size optional, the dimension when different from 600 x 460.
     */
    public static void showModelessHtmlDialog(String title, Component parent, URL htmlUrl, Dimension size) {
        showModelessDialog(title, parent, buildHtmlView(htmlUrl), size, null, false);
    }

    /**
     * Opens a non-modal dialog showing given plain text in a scroll-pane.
     * @param title the text to show in the dialog's title bar.
     * @param plainText the text to render in returned JTextPane.
     * @param parent required, a parent Component in same Window where to locate dialog relatively.
     * @param size optional, the dimension when different from 600 x 460.
     */
    public static void showModelessTextDialog(String title, Component parent, String plainText, Dimension size) {
        showModelessDialog(title, parent, buildPlainTextArea(plainText), size, null, true);
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
    public static JDialog showModelessDialog(String title, Component parent, JComponent componentToShow, Dimension size, Point location, boolean relativeToParent) {
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
                        dialog.setVisible(false);
                        try { dialog.dispose(); } catch (Exception ex) {}
                    }
                });
        
        setSize(dialog, componentToShow, size);
        setLocation(dialog, location, relativeToParent ? parent : window);
        dialog.setVisible(true);
        componentToShow.requestFocusInWindow(); // for immediately sizing font by keyboard
        
        return dialog;
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
    
    private static void setSize(JDialog dialog, JComponent componentToShow, Dimension size) {
        if (size != null) {
            dialog.setSize(size);
        }
        else {
            final Dimension sizeToSet;
            final Dimension preferredContentSize = componentToShow.getPreferredSize();
            if (preferredContentSize.width < 30 || preferredContentSize.height < (30 + FrameStarter.titlebarHeight)) {
                sizeToSet = new Dimension(760, 580);
            }
            else {
                sizeToSet = new Dimension(
                        preferredContentSize.width,
                        preferredContentSize.height + FrameStarter.titlebarHeight);
            }
            dialog.setSize(sizeToSet);
        }
    }
    private static void setLocation(JDialog dialog, Point location, Component relativeToComponent) {
        if (location != null)
            dialog.setLocation(location);
        else
            dialog.setLocationRelativeTo(relativeToComponent);
    }
}