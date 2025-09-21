package fri.music.swingutils;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.Window;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.net.URL;
import java.util.Objects;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.SwingUtilities;
import fri.music.HtmlResources;
import fri.music.swingutils.text.HtmlBrowser;

/**
 * Shows text in non-modal dialogs.
 */
public class DialogUtil
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
        final Window window = SwingUtilities.windowForComponent(Objects.requireNonNull(parent));
        final JDialog dialog = new JDialog(Objects.requireNonNull(window), title);
        dialog.getContentPane().add(componentToShow);
        
        final KeyListener escapeListener = new KeyAdapter()   {
            @Override
            public void keyPressed(KeyEvent e) {
                if (e.getKeyCode() == KeyEvent.VK_ESCAPE) {
                    dialog.setVisible(false);
                    try { dialog.dispose(); } catch (Exception ex) {}
                }
            }
        };
        componentToShow.addKeyListener(escapeListener);
        componentToShow.setFocusable(true); // else no ESCAPE
        dialog.addKeyListener(escapeListener);
        dialog.setFocusable(true); // else no ESCAPE
        
        dialog.setSize(size != null ? size : new Dimension(760, 580));
        
        if (location == null)
            dialog.setLocationRelativeTo(relativeToParent ? parent : window);
        else
            dialog.setLocation(location);
        
        dialog.setVisible(true);
        
        componentToShow.requestFocus(); // for immediately sizing font by keyboard
        
        return dialog;
    }


    /**
     * Builds a panel containing given HTML.
     * @param htmlUrl the HTML resource to render in returned JTextPane.
     * @return a scroll-able navigation-able panel containing given HTML resource.
     */
    private static JComponent buildHtmlView(URL htmlUrl) {
        return new HtmlBrowser(htmlUrl, HtmlResources.class);
    }

    /**
     * Builds a TextArea containing given plain text.
     * @param text the text to render in returned JTextArea.
     * @return a JTextArea containing given HTML text.
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