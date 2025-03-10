package fri.music.instrument;

import java.awt.Color;

/**
 * See also https://www.flutopedia.com/sound_color.htm
 */
public enum OctaveColor
{
    GRAY_LOW(Color.LIGHT_GRAY),
    
    BROWN("#CFC5BF" /*"#CEC3CC"*/ /*"#CEC3BF"*/ /*"#CEC395"*/ /*"#E5BA8D"*/),
    
    RED("#F4B0B0"),

    ORANGE("#F4CCB0"),

    YELLOW("#F4F4B0"),

    GREEN("#B0F4B0"),

    CYAN("#B0F4F4"),

    BLUE("#B0B0F4"),

    VIOLET("#CCB0F4"),

    GRAY_HIGH(Color.LIGHT_GRAY),
    ;
    
    public final Color rgbColor;
    
    private OctaveColor(String rgbHexCode) {
        this.rgbColor = Color.decode(rgbHexCode);
    }
    private OctaveColor(Color color) {
        this.rgbColor = color;
    }

    public static Color forOctave(int octave) {
        final OctaveColor[] values = OctaveColor.values();
        final int index = Math.abs(octave + 1) % values.length;
        return values[index].rgbColor;
    }
}