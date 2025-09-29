package fri.music.swingutils.text;

import javax.swing.JTextArea;

public final class TextAreaUtil
{
    /**
     * Overwrites any existing text in text-area.
     * Tries to keep caret at its current position.
     * When text-area is empty, caret would go to text start.
     * The text-area gets input focus.
     * @param textArea the text-area where to set in given text.
     * @param text the text to set into text-area.
     */
    public static void setText(JTextArea textArea, String text) {
        final int caretPosition = textArea.getCaretPosition();
        textArea.setText(text);
        textArea.setCaretPosition(Math.min(caretPosition, textArea.getDocument().getLength()));
        textArea.requestFocusInWindow();
    }
    
    private TextAreaUtil() {} // do not instantiate
}
