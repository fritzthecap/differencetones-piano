package fri.music.player.notelanguage;

import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.Test;

class InputTextScannerTest
{
    @Test
    void removeSpacesBetweenNoteAndTieOrSlurSymbols() {
        final String TEST_DATA = 
                "\n (\tA4 B4\r\n) C5 {\n D5 \t E5 \r\n } \r\n\t";
        final String[] EXPECTED_RESULT = new String[] {
                "(A4", "B4)", "C5", "{D5", "E5}"
            };

        assertResult(TEST_DATA, EXPECTED_RESULT);
    }
    
    @Test
    void removeSpacesBetweenNoteAndChordSymbols() {
        final String TEST_DATA = 
                "[ A4 C5 E5 ]";
        final String[] EXPECTED_RESULT = new String[] {
                "[A4", "C5", "E5]"
            };

        assertResult(TEST_DATA, EXPECTED_RESULT);
    }

    private void assertResult(String TEST_DATA, String[] EXPECTED_RESULT) {
        final String[] tokens = new InputTextScanner().toStringArray(TEST_DATA);
        assertArrayEquals(EXPECTED_RESULT, tokens);
    }
}