package fri.music.swingutils;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.Window;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.util.Objects;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JEditorPane;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextPane;
import javax.swing.SwingUtilities;
import javax.swing.text.JTextComponent;
import javax.swing.text.html.HTMLEditorKit;
import javax.swing.text.html.StyleSheet;

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
        showModelessDialog(title, parent, buildHtmlTextPane(htmlText), size, null);
    }

    /**
     * Opens a non-modal dialog showing given plain text in a scroll-pane.
     * @param title the text to show in the dialog's title bar.
     * @param plainText the text to render in returned JTextPane.
     * @param parent required, a parent Component in same Window where to locate dialog relatively.
     * @param size optional, the dimension when different from 600 x 460.
     */
    public static void showModelessTextDialog(String title, Component parent, String plainText, Dimension size) {
        showModelessDialog(title, parent, buildPlainTextArea(plainText), size, null);
    }

    /**
     * Opens a non-modal dialog showing given component.
     * @param title text for title-bar.
     * @param componentToShow the panel to render.
     * @param parent the parent Component to show over.
     * @param size the wanted size of the dialog.
     * @return the created dialog window.
     */
    public static JDialog showModelessDialog(String title, Component parent, JComponent componentToShow, Dimension size, Point location) {
        final Window window = SwingUtilities.windowForComponent(Objects.requireNonNull(parent));
        final JDialog dialog = new JDialog(Objects.requireNonNull(window), title);
        dialog.getContentPane().add(new JScrollPane(componentToShow));
        
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
        
        dialog.setSize(size != null ? size : new Dimension(660, 460));
        
        if (location == null)
            dialog.setLocationRelativeTo(parent);
        else
            dialog.setLocation(location);
        
        dialog.setVisible(true);
        
        componentToShow.requestFocus();
        
        return dialog;
    }


    /**
     * Builds a TextPane containing given HTML text. Has no JScrollPane yet!
     * @param htmlText the text to render in returned JTextPane.
     * @return a JTextPane containing given HTML text.
     */
    private static JComponent buildHtmlTextPane(String htmlText) {
        final JTextPane htmlViewer = new JTextPane();
        htmlViewer.setContentType("text/html");
        
        configureHtmlView(htmlViewer);
        
        configureTextComponent(htmlText, htmlViewer);
        
        new HtmlViewerActions(htmlViewer); // font magnifier, works just with JTextPane, not with JEditorPane!
        
        return htmlViewer;
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
    
    private static void configureHtmlView(JEditorPane textPane) {
        final HTMLEditorKit kit = (HTMLEditorKit) textPane.getEditorKitForContentType("text/html");
        final StyleSheet css = kit.getStyleSheet();
        css.addRule("h1 { text-decoration: underline; color: blue; }");
        //css.addRule("h2, h3, h4 { color: green; }");
        css.addRule("p { margin-top: 0; padding-top: 0; padding-bottom: 10; }");
    }
}