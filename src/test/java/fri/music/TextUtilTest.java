package fri.music;

import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

class TextUtilTest
{
    @Test
    void getFirstNumber() {
        assertEquals(4, TextUtil.getFirstNumber("F#4"));
        assertEquals(10, TextUtil.getFirstNumber("A10/4"));
        assertEquals(0, TextUtil.getFirstNumber("C0/16"));
        assertEquals(2, TextUtil.getFirstNumber("C-2/16"));
        assertEquals(55, TextUtil.getFirstNumber("C#55/16"));
    }

    @Test
    void getWithoutFirstNumber() {
        assertEquals("D#", TextUtil.getUntilFirstNumber("D#5"));
        assertEquals("C", TextUtil.getUntilFirstNumber("C10"));
        assertEquals("", TextUtil.getUntilFirstNumber("2C"));
        assertEquals("D", TextUtil.getUntilFirstNumber("D6E4"));
    }

    @Test
    @Disabled // proof of concept, not a test
    void performance() {
        final String ipnName = "G#6";
        
        final long startTimeRegEx = System.currentTimeMillis();
        for (int i = 0; i < 4000; i++) {
            ipnName.replaceAll("[0-9]", "");
            Integer.valueOf(ipnName.replaceAll("[^0-9]", ""));
        }
        final long endTimeRegEx = System.currentTimeMillis();
        
        final long startTime = System.currentTimeMillis();
        for (int i = 0; i < 4000; i++) {
            TextUtil.getUntilFirstNumber(ipnName);
            TextUtil.getFirstNumber(ipnName);
        }
        final long endTime = System.currentTimeMillis();
        
        System.out.println("RegEx millis    = "+(endTimeRegEx - startTimeRegEx));
        System.out.println("TextUtil millis = "+(endTime - startTime));
    }
}