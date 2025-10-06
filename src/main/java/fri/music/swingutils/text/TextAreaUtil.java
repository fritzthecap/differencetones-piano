package fri.music.swingutils.text;

import javax.swing.JTextArea;

public final class TextAreaUtil
{
    /**
     * Overwrites any existing text in text-area.
     * The text-area gets input focus.
     * @param textArea the text-area where to set given text into.
     * @param text the text to set into text-area.
     */
    public static void setText(JTextArea textArea, String text) {
        //final int caretPosition = textArea.getCaretPosition();
        textArea.setText(text);
        //textArea.setCaretPosition(Math.min(caretPosition, textArea.getDocument().getLength()));
        textArea.requestFocusInWindow();
    }
    
    private TextAreaUtil() {} // do not instantiate
}
