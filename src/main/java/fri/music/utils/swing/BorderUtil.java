package fri.music.utils.swing;

import java.awt.Color;
import java.awt.Font;
import javax.swing.BorderFactory;
import javax.swing.border.Border;
import javax.swing.border.TitledBorder;

public final class BorderUtil
{
    /** @return a TitledBorder with bigger font. */
    public static Border titledBorder(String title, float fontSizeIncrement) {
        final TitledBorder titledBorder = BorderFactory.createTitledBorder(title);
        return configureBorder(fontSizeIncrement, titledBorder);
    }
    
    /** @return a TitledBorder with bigger font and thicker line. */
    public static Border titledBorder(String title, float fontSizeIncrement, int linePixelWidth) {
        final TitledBorder titledBorder = BorderFactory.createTitledBorder(
                BorderFactory.createLineBorder(Color.LIGHT_GRAY, linePixelWidth, true),
                title);
        return configureBorder(fontSizeIncrement, titledBorder);
    }

    private static Border configureBorder(float fontSizeIncrement, TitledBorder titledBorder) {
        final Font titleFont = titledBorder.getTitleFont();
        titledBorder.setTitleFont(titleFont.deriveFont(titleFont.getSize2D() + fontSizeIncrement));
        return titledBorder;
    }
    
    public BorderUtil() {} // do not instantiate
}
