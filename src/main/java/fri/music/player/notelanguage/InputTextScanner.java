package fri.music.player.notelanguage;

import static fri.music.player.notelanguage.NoteConnections.*;
import java.util.Arrays;
import java.util.Map;
import fri.music.ToneSystem;

/**
 * Reads a melody text and builds a String array for <code>MelodyFactory</code> from it.
 * This removes spaces between opening or closing match-symbols and notes, i.e.
 * ties, slurs and chord brackets. Finally the text is split by whitespace.
 */
public class InputTextScanner
{
    private static final String SPACE_PATTERN = "[\r\n\t ]+";

    /**
     * Splits given text by spaces, after removing spaces between 
     * parentheses/brackets/braces and a note.
     * @param text the text to split into non-space tokens.
     * @return the array of non.space tokens.
     */
    public String[] toStringArray(String text) {
        text = text.trim();
        if (text.length() <= 0)
            return null;
        
        text = text
            .replaceAll("\\"+TIE_START_SYMBOL+SPACE_PATTERN,  TIE_START_SYMBOL)
            .replaceAll(SPACE_PATTERN+"\\"+TIE_END_SYMBOL,    TIE_END_SYMBOL)
            .replaceAll("\\"+SLUR_START_SYMBOL+SPACE_PATTERN, SLUR_START_SYMBOL)
            .replaceAll(SPACE_PATTERN+"\\"+SLUR_END_SYMBOL,   SLUR_END_SYMBOL)
            .replaceAll("\\"+CHORD_START_SYMBOL+SPACE_PATTERN, CHORD_START_SYMBOL)
            .replaceAll(SPACE_PATTERN+"\\"+CHORD_END_SYMBOL,   CHORD_END_SYMBOL);
        
        return text.split("[\r\n\t ]+");
    }
    
    
    /**
     * Finds out the index of the nearest note to the given text dot.
     * Optionally creates a complete mapping of dot positions to note indexes
     * when <code>noteIndexToDotMap</code> parameter is not null.
     * @param text the notes text to process.
     * @param dot the caret position within the given text.
     * @param noteIndexToDotMap optional, the complete note to dot mappings of text,
     * @return the index of the note nearest to the dot, or -1 if no note found. 
     *      Tempo and time-signature do not count as notes.
     *      When the note is in an interval, the index of the
     *      first note of the interval is returned.
     */
    public int noteIndexForDot(String text, int dotInText, Map<Integer,Integer> noteIndexToDotMap) {
        final int length = text.length();
        
        int index = -1;
        int indexOfNote = -1;
        boolean inTie = false;
        boolean inChord = false;
        
        for (int dot = 0; dot < length; dot++) {
            final char c = text.charAt(dot);
            if (equal(c, TIE_START_SYMBOL)) {
                inTie = true;
                index = nextNote(index, dot, noteIndexToDotMap);
            }
            else if (equal(c, TIE_END_SYMBOL)) {
                inTie = false;
            }
            else if (equal(c, CHORD_START_SYMBOL)) {
                inChord = true;
                index = nextNote(index, dot, noteIndexToDotMap);
            }
            else if (equal(c, CHORD_END_SYMBOL)) {
                inChord = false;
            }
            else if (inTie == false && inChord == false && isNoteLetter(c)) {
                index = nextNote(index, dot, noteIndexToDotMap);
            }
            
            if (indexOfNote < 0 && dotInText >= 0 && dot >= dotInText && index >= 0) // landed on a note
                if (noteIndexToDotMap == null) // no complete mapping needed, break when found
                    return index;
                else
                    indexOfNote = index;
        }
        
        return (indexOfNote >= 0) ? indexOfNote : (dotInText >= 0) ? index : -1;
    }
    
    private int nextNote(int index, int dot, Map<Integer,Integer> noteIndexToDot) {
        index++;
        if (noteIndexToDot != null)
            noteIndexToDot.put(index, dot);
        return index;
    }
    
    private boolean isNoteLetter(char c) {
        if (equal(c, ToneSystem.REST_SYMBOL))
            return true;
        
        final char upperChar = Character.toUpperCase(c);
        return Arrays.stream(ToneSystem.IPN_BASE_NAMES)
            .anyMatch(name -> name.length() == 1 && equal(upperChar, name));
    }
    
    private boolean equal(char c, String symbol) {
        return symbol.charAt(0) == c;
    }
}