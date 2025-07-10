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
import javax.swing.JTextPane;
import javax.swing.SwingUtilities;

public class DialogUtil
{
    /**
     * Opens a non-modal dialog showing given HTML text in a scroll-pane.
     * @param textAsHtml the text to render in returned JTextPane.
     * @param parent a parent Component in same Window where to locate dialog relatively.
     */
    public static void showModelessHtmlDialog(Component parent, String textAsHtml, Dimension size) {
        final Window window = SwingUtilities.windowForComponent(Objects.requireNonNull(parent));
        final JDialog dialog = new JDialog(Objects.requireNonNull(window), "Notes Syntax Help");
        final JComponent helpText = buildHtmlTextPane(textAsHtml);
        dialog.getContentPane().add(new JScrollPane(helpText));
        
        final KeyListener escapeListener = new KeyAdapter()   {
            @Override
            public void keyPressed(KeyEvent e) {
                if (e.getKeyCode() == KeyEvent.VK_ESCAPE) {
                    dialog.setVisible(false);
                    try { dialog.dispose(); } catch (Exception ex) {}
                }
            }
        };
        helpText.addKeyListener(escapeListener);
        dialog.addKeyListener(escapeListener);
        helpText.setFocusable(true); // else no ESCAPE
        dialog.setFocusable(true); // else no ESCAPE
        
        if (size == null)
            size = new Dimension(600, 460);
        
        dialog.setSize(size);
        dialog.setLocationRelativeTo(parent);
        dialog.setVisible(true);
    }

    /**
     * Builds a TextPane containing given HTML text. Has no JScrollPane yet!
     * @param textAsHtml the text to render in returned JTextPane.
     * @return a JTextPane containing given HTML text.
     */
    public static JComponent buildHtmlTextPane(String textAsHtml) {
        final JTextPane textPane = new JTextPane();
        textPane.setEditable(false);
        textPane.setContentType("text/html");
        textPane.setText(textAsHtml);
        textPane.setCaretPosition(0); // scroll back to top
        return textPane;
    }

}
