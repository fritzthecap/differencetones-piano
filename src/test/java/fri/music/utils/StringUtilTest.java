package fri.music.utils;

import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

class StringUtilTest
{
    @Test
    void getFirstNumber() {
        assertEquals(4, StringUtil.getFirstNumber("F#4"));
        assertEquals(10, StringUtil.getFirstNumber("A10/4"));
        assertEquals(0, StringUtil.getFirstNumber("C0/16"));
        assertEquals(2, StringUtil.getFirstNumber("C-2/16"));
        assertEquals(55, StringUtil.getFirstNumber("C#55/16"));
    }

    @Test
    void getWithoutFirstNumber() {
        assertEquals("D#", StringUtil.getUntilFirstNumber("D#5"));
        assertEquals("C", StringUtil.getUntilFirstNumber("C10"));
        assertEquals("", StringUtil.getUntilFirstNumber("2C"));
        assertEquals("D", StringUtil.getUntilFirstNumber("D6E4"));
    }

    @Test
    void getWithoutLastNumber() {
        assertEquals("D#", StringUtil.getUntilLastNumber("D#5"));
        assertEquals("C", StringUtil.getUntilLastNumber("C10"));
        assertEquals("2C", StringUtil.getUntilLastNumber("2C"));
        assertEquals("D6E", StringUtil.getUntilLastNumber("D6E4"));
        assertEquals("Piano (", StringUtil.getUntilLastNumber("Piano (3)"));
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
            StringUtil.getUntilFirstNumber(ipnName);
            StringUtil.getFirstNumber(ipnName);
        }
        final long endTime = System.currentTimeMillis();
        
        System.out.println("RegEx millis    = "+(endTimeRegEx - startTimeRegEx));
        System.out.println("TextUtil millis = "+(endTime - startTime));
    }
}