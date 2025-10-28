package fri.music.player.notelanguage;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import java.util.HashMap;
import java.util.Map;
import org.junit.jupiter.api.Test;

class InputTextScannerTest
{
    @Test
    void removeSpacesBetweenNoteAndTieOrSlurSymbols() {
        final String NOTES_TEXT = 
                "\n (\tA4 B4\r\n) C5 {\n D5 \t E5 \r\n } \r\n\t";
        final String[] EXPECTED_RESULT = new String[] {
                "(A4", "B4)", "C5", "{D5", "E5}"
            };

        assertSpacesRemovalResult(NOTES_TEXT, EXPECTED_RESULT);
    }
    
    @Test
    void removeSpacesBetweenNoteAndChordSymbols() {
        final String NOTES_TEXT = 
                "[ A4 C5 E5 ]";
        final String[] EXPECTED_RESULT = new String[] {
                "[A4", "C5", "E5]"
            };

        assertSpacesRemovalResult(NOTES_TEXT, EXPECTED_RESULT);
    }
    
    private void assertSpacesRemovalResult(String text, String[] expectedResult) {
        final String[] tokens = new InputTextScanner().toStringArray(text);
        assertArrayEquals(expectedResult, tokens);
    }
    
    
    @Test
    void noteIndexForDot() {
        final String NOTES_TEXT = "  A4/8 C5/4  ";
        
        asserNoteIndexForDot(NOTES_TEXT, 1, 0);
        asserNoteIndexForDot(NOTES_TEXT, 2, 0);
        asserNoteIndexForDot(NOTES_TEXT, 4, 0);
        asserNoteIndexForDot(NOTES_TEXT, 6, 0);
        asserNoteIndexForDot(NOTES_TEXT, 13, 1);
    }
    
    @Test
    void noteIndexForDotInEmptyText() {
        final String NOTES_TEXT = " x y z "; // these are no valid note names
        
        asserNoteIndexForDot(NOTES_TEXT, 2, -1);
    }
        
    @Test
    void noteIndexForDotWithTempoAndTime() {
        final String NOTES_TEXT = "144\n3/4\nc4/8 d4/8 e4/8 "; // tempo, time-signature
        
        asserNoteIndexForDot(NOTES_TEXT, 7, 0);
        asserNoteIndexForDot(NOTES_TEXT, 17, 1);
        asserNoteIndexForDot(NOTES_TEXT, 18, 2);
        asserNoteIndexForDot(NOTES_TEXT, 23, 2);
    }
    
    @Test
    void noteIndexForDotOnRest() {
        final String NOTES_TEXT = "-/2 -/4 C4";
        
        asserNoteIndexForDot(NOTES_TEXT, 0, 0);
        asserNoteIndexForDot(NOTES_TEXT, 3, 0);
        asserNoteIndexForDot(NOTES_TEXT, 4, 1);
        asserNoteIndexForDot(NOTES_TEXT, 8, 2);
        asserNoteIndexForDot(NOTES_TEXT, 10, 2);
    }
    
    @Test
    void noteIndexForDotMixed() {
        final String NOTES_TEXT = "-/2 -/4 (c4/4 c4/4) E4/8 F4/8 [G4/2 C4/2] C5/4";
        
        asserNoteIndexForDot(NOTES_TEXT, 3, 0);
        asserNoteIndexForDot(NOTES_TEXT, 8, 2);
        asserNoteIndexForDot(NOTES_TEXT, 19, 2);
        asserNoteIndexForDot(NOTES_TEXT, 20, 3);
        asserNoteIndexForDot(NOTES_TEXT, 29, 4);
        asserNoteIndexForDot(NOTES_TEXT, 30, 5);
        asserNoteIndexForDot(NOTES_TEXT, 38, 5);
        asserNoteIndexForDot(NOTES_TEXT, 42, 6);
    }
    
    @Test
    void noteIndexForDotInTie() {
        final String NOTES_TEXT = "G4/4 ( A4/4 A4/8 ) C5 D5";
        
        asserNoteIndexForDot(NOTES_TEXT, 4, 0);
        asserNoteIndexForDot(NOTES_TEXT, 5, 1);
        asserNoteIndexForDot(NOTES_TEXT, 16, 1);
        asserNoteIndexForDot(NOTES_TEXT, 19, 2);
        asserNoteIndexForDot(NOTES_TEXT, 24, 3);
    }
    
    @Test
    void noteIndexForDotInChord() {
        final String NOTES_TEXT = " [ A4 C5 E5 ] [ A4 C5 E5 ] ";
        
        asserNoteIndexForDot(NOTES_TEXT, 0, 0);
        asserNoteIndexForDot(NOTES_TEXT, 12, 0);
        asserNoteIndexForDot(NOTES_TEXT, 14, 1);
        asserNoteIndexForDot(NOTES_TEXT, 22, 1);
    }
    
    private void asserNoteIndexForDot(String text, int dot, int expectedNoteIndex) {
        final int noteIndex = new InputTextScanner().noteIndexForDot(
                text, 
                dot, 
                null); // new HashMap<Integer,Integer>()
        assertEquals(expectedNoteIndex, noteIndex);
    }
    
    @Test
    void noteIndexForDotMap() {
        final String NOTES_TEXT = " [ A4/4 e5 ] F4/4 ( b4/4 B4 ) g4/4 ";
        
        final Map<Integer,Integer> map = new HashMap<>();
        final int noteIndex = new InputTextScanner().noteIndexForDot(
                NOTES_TEXT, 
                -1, 
                map);
        
        assertEquals(4, map.size()); // 4 notes, because chord notes and tied notes count as one note
        assertEquals(-1, noteIndex); // we gave no valid dot
        
        assertEquals(map.get(0), 1); // noteIndex = dot
        assertEquals(map.get(1), 13);
        assertEquals(map.get(2), 18);
        assertEquals(map.get(3), 30);
    }
}