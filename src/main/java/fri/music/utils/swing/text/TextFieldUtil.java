package fri.music.utils.swing.text;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import javax.swing.BorderFactory;
import javax.swing.JTextField;
import javax.swing.SwingConstants;

public final class TextFieldUtil
{
    /** Used for creating text fields that display IPN-names of tones. */
    public static JTextField sizedField(int width, String title, boolean editable) {
        final JTextField textField = new JTextField();
        textField.setEditable(editable);
        textField.setFont(new Font(Font.MONOSPACED, Font.BOLD, 16));
        textField.setHorizontalAlignment(SwingConstants.CENTER);
        textField.setBorder(BorderFactory.createTitledBorder(
                BorderFactory.createLineBorder(Color.LIGHT_GRAY, 3, true), 
                title));
        final Dimension size = new Dimension(width, 48);
        textField.setPreferredSize(size);
        textField.setMaximumSize(size);
        return textField;
    }
    
    private TextFieldUtil() {} // do not instantiate
}
