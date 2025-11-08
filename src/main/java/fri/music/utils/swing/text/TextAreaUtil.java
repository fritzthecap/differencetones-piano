package fri.music.utils.swing.text;

import java.awt.Font;
import javax.swing.JComponent;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import fri.music.utils.swing.window.DialogStarter;

public final class TextAreaUtil
{
    /**
     * Overwrites any existing text in text-area.
     * The text-area gets input focus.
     * @param textArea the text-area where to set given text into.
     * @param text the text to set into text-area.
     * @param preserveCaretPosition when true, the caret position should be nearly the same after 
     */
    public static void setText(JTextArea textArea, String text, boolean preserveCaretPosition) {
        final int caretPosition = preserveCaretPosition
            ? Math.max(textArea.getCaretPosition(), 0)
            : 0;
        textArea.setText(text);
        textArea.setCaretPosition(Math.min(caretPosition, textArea.getDocument().getLength()));
        textArea.requestFocusInWindow();
    }
    
    /**
     * Shows given text in a non-modal dialog.
     * @param parent required, the dialog parent.
     * @param title required, the text to put into dialog title bar.
     * @param text required, the text to render in text-area.
     * @param font optional, the text-area font to use.
     */
    public static void showResultInTextDialog(JComponent parent, String title, String text, String font) {
        final JTextArea textArea = new JTextArea(text);
        textArea.setTabSize(2);
        textArea.setEditable(false);
        if (font != null)
            textArea.setFont(Font.decode(font).deriveFont(Font.BOLD, 14f));
        else
            textArea.setFont(textArea.getFont().deriveFont(Font.BOLD, 14f));
        
        final TextAreaActions fontActions = new TextAreaActions(textArea); // adds context menu
        
        DialogStarter.start(title, parent, new JScrollPane(textArea), null, true);
    }

    
    private TextAreaUtil() {} // do not instantiate
}
