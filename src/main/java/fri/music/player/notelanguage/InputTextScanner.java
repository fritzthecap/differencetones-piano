package fri.music.player.notelanguage;

import static fri.music.player.notelanguage.NoteConnections.*;

/**
 * Reads a melody text and builds a String array for <code>MelodyFactory</code> from it.
 * This is needed to remove spaces between ties and notes, or slurs and notes.
 */
class InputTextScanner
{
    private static final String SPACE_PATTERN = "[\r\n\t ]+";

    public String[] toStringArray(String text) {
        text = text.trim()
            .replaceAll("\\"+TIE_START_SYMBOL+SPACE_PATTERN,  TIE_START_SYMBOL)
            .replaceAll(SPACE_PATTERN+"\\"+TIE_END_SYMBOL,    TIE_END_SYMBOL)
            .replaceAll("\\"+SLUR_START_SYMBOL+SPACE_PATTERN, SLUR_START_SYMBOL)
            .replaceAll(SPACE_PATTERN+"\\"+SLUR_END_SYMBOL,   SLUR_END_SYMBOL);
        return text.split("[\r\n\t ]+");
    }
}
