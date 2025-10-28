package fri.music.player.notelanguage;

/**
 * Symbols around notes like ties "(A4 (B4) C5)" or slurs "{A4 B4 C5}".
 * A slur is for 2-n notes of different pitch. A tie is for 2-n notes
 * with same pitch. Slurs and ties can be combined, but slur must be the 
 * outer symbol, like "{(A4".
 */
public class NoteConnections
{
    /** The character used to start a tie. */
    public static final String TIE_START_SYMBOL = "(";
    /** The character used to end a tie. */
    public static final String TIE_END_SYMBOL = ")";
    /** The character used to start a slur. */
    public static final String SLUR_START_SYMBOL = "{";
    /** The character used to end a slur. */
    public static final String SLUR_END_SYMBOL = "}";
    /** The character used to start a chord. */
    public static final String CHORD_START_SYMBOL = "[";
    /** The character used to end a chord. */
    public static final String CHORD_END_SYMBOL = "]";
    
    private static class NoteConnection
    {
        /** True when the symbolToDetect has been found on construction. */
        public final boolean exists;
        /** The note without symbolToDetect after construction. */
        public final String editedToken;
        
        NoteConnection(String melodyToken, String symbolToDetect) {
            final String removeResult = removeSymbolFromStartOrEnd(melodyToken, symbolToDetect);
            
            this.exists = (melodyToken != removeResult); // pointer comparison
            this.editedToken = removeResult.trim();
        }
        
        private String removeSymbolFromStartOrEnd(String melodyToken, String symbolToRemove) {
            final int index = melodyToken.indexOf(symbolToRemove);
            if (index < 0)
                return melodyToken;
            
            if (index != 0 && index != melodyToken.length() - 1) // must be at begin or end of note
                throw new IllegalArgumentException("Invalid position of "+symbolToRemove+" in "+melodyToken);
            
            return melodyToken.substring(0, index) + 
                    melodyToken.substring(index + symbolToRemove.length());
        }
    }
    
    /** The processed token, containing no tie or slur symbols any more. */
    public final String melodyToken;
    
    private final boolean slurStart;
    private final boolean slurEnd;
    private final boolean tieStart;
    private final boolean tieEnd;
    private final boolean chordStart;
    private final boolean chordEnd;
    
    NoteConnections(String melodyToken) {
        this(melodyToken, false);
    }
    
    NoteConnections(String melodyToken, boolean disallowChords) {
        NoteConnection noteConnection;
        noteConnection = new NoteConnection(melodyToken, SLUR_START_SYMBOL);
        this.slurStart = noteConnection.exists;
        noteConnection = new NoteConnection(noteConnection.editedToken, SLUR_END_SYMBOL);
        this.slurEnd = noteConnection.exists;
        noteConnection = new NoteConnection(noteConnection.editedToken, TIE_START_SYMBOL);
        this.tieStart = noteConnection.exists;
        noteConnection = new NoteConnection(noteConnection.editedToken, TIE_END_SYMBOL);
        this.tieEnd = noteConnection.exists;
        noteConnection = new NoteConnection(noteConnection.editedToken, CHORD_START_SYMBOL);
        this.chordStart = noteConnection.exists;
        noteConnection = new NoteConnection(noteConnection.editedToken, CHORD_END_SYMBOL);
        this.chordEnd = noteConnection.exists;
        
        if (disallowChords && (chordStart || chordEnd))
            throw new IllegalArgumentException("Chords are not allowed in this area: "+melodyToken);
        
        if (chordStart && chordEnd)
            throw new IllegalArgumentException("A chord must contain more than one note: "+melodyToken);
        
        this.melodyToken = noteConnection.editedToken; // text without symbols
    }
    
    public boolean isSlurStart() {
        return slurStart == true && slurEnd == false;
    }
    public boolean isSlurEnd() {
        return slurStart == false && slurEnd == true;
    }
    
    public boolean isTieStart() {
        return tieStart == true && tieEnd == false;
    }
    public boolean isTieEnd() {
        return tieStart == false && tieEnd == true;
    }
    
    public boolean isChordStart() {
        return chordStart == true && chordEnd == false;
    }
    public boolean isChordEnd() {
        return chordStart == false && chordEnd == true;
    }
}