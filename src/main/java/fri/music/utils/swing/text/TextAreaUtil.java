package fri.music.utils.swing.text;

import javax.swing.JTextArea;

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
    
    private TextAreaUtil() {} // do not instantiate
}
