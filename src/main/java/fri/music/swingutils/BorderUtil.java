package fri.music.swingutils;

import java.awt.Font;
import javax.swing.BorderFactory;
import javax.swing.border.Border;
import javax.swing.border.TitledBorder;

public final class BorderUtil
{
    /** @return a TitledBorder with bigger font. */
    public static Border titledBorder(String title, float fontSizeIncrement) {
        final TitledBorder titledBorder = BorderFactory.createTitledBorder(title);
        final Font titleFont = titledBorder.getTitleFont();
        titledBorder.setTitleFont(titleFont.deriveFont(titleFont.getSize2D() + fontSizeIncrement));
        return titledBorder;
    }
    
    public BorderUtil() {} // do not instantiate
}
