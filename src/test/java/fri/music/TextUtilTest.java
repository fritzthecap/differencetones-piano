package fri.music;

import static org.junit.jupiter.api.Assertions.*;
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
        assertEquals("D#", TextUtil.getWithoutFirstNumber("D#5"));
        assertEquals("C", TextUtil.getWithoutFirstNumber("C10"));
        assertEquals("C10", TextUtil.getWithoutFirstNumber("2C10"));
        assertEquals("DE4", TextUtil.getWithoutFirstNumber("D6E4"));
    }

}
