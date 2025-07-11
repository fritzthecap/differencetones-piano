package fri.music.swingutils;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Window;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.util.Objects;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextPane;
import javax.swing.SwingUtilities;
import javax.swing.text.JTextComponent;

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
    public static void showModelessHtmlDialog(String title, Component parent, String htmlText, Dimension size) {
        showModelessDialog(title, parent, buildHtmlTextPane(htmlText), size);
    }

    /**
     * Opens a non-modal dialog showing given plain text in a scroll-pane.
     * @param title the text to show in the dialog's title bar.
     * @param text the text to render in returned JTextPane.
     * @param parent required, a parent Component in same Window where to locate dialog relatively.
     * @param size optional, the dimension when different from 600 x 460.
     */
    public static void showModelessTextDialog(String title, Component parent, String text, Dimension size) {
        showModelessDialog(title, parent, buildPlainTextArea(text), size);
    }

    /**
     * Builds a TextPane containing given HTML text. Has no JScrollPane yet!
     * @param htmlText the text to render in returned JTextPane.
     * @return a JTextPane containing given HTML text.
     */
    private static JComponent buildHtmlTextPane(String htmlText) {
        final JTextPane textPane = new JTextPane();
        textPane.setContentType("text/html");
        configureTextComponent(htmlText, textPane);
        return textPane;
    }

    /**
     * Builds a TextArea containing given plain text. Has no JScrollPane yet!
     * @param text the text to render in returned JTextArea.
     * @return a JTextArea containing given HTML text.
     */
    private static JComponent buildPlainTextArea(String text) {
        final JTextArea textArea = new JTextArea();
        textArea.setToolTipText("Use Ctrl-A and Ctrl-C to Copy the Whole Text");
        configureTextComponent(text, textArea);
        return textArea;
    }

    private static void configureTextComponent(String text, JTextComponent textComponent) {
        textComponent.setEditable(false);
        textComponent.setText(text);
        textComponent.setCaretPosition(0); // scroll back to top
    }

    private static void showModelessDialog(String title, Component parent, JComponent textComponent, Dimension size) {
        final Window window = SwingUtilities.windowForComponent(Objects.requireNonNull(parent));
        final JDialog dialog = new JDialog(Objects.requireNonNull(window), title);
        dialog.getContentPane().add(new JScrollPane(textComponent));
        
        final KeyListener escapeListener = new KeyAdapter()   {
            @Override
            public void keyPressed(KeyEvent e) {
                if (e.getKeyCode() == KeyEvent.VK_ESCAPE) {
                    dialog.setVisible(false);
                    try { dialog.dispose(); } catch (Exception ex) {}
                }
            }
        };
        textComponent.addKeyListener(escapeListener);
        textComponent.setFocusable(true); // else no ESCAPE
        dialog.addKeyListener(escapeListener);
        dialog.setFocusable(true); // else no ESCAPE
        
        if (size == null)
            size = new Dimension(600, 460);
        
        dialog.setSize(size);
        dialog.setLocationRelativeTo(parent);
        dialog.setVisible(true);
    }
}