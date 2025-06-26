package fri.music.player;

import java.util.ArrayList;
import java.util.List;
import fri.music.ToneSystem;
import fri.music.Tones;

/**
 * Reads in a number of notes with length, headed by time-signature and tempo, 
 * and produces a melody (array of Note) from it, playable by a <code>Player</code>.
 * <p/>
 * <b>Input DSL:</b><br/>
 * A note is given as an IPN-name and its length behind a slash, 
 * e.g. "A4/4" for a quarter note on pitch of A4 (440 Hz),
 * or "C#4/8." for a dotted C#4 eighth note,
 * or "B3/8,3" for a B3 triplet eighth note (each note of the triplet must have a ",3").
 * No space must appear inside notes and their length specification.
 * A rest is written as "-".
 * In IPN there is no "Eb" or "Bb", you must give "D#" or "A#",
 * and there is no German "H", such is written as "B".
 * <p/>
 * Notes connected by "tie" are notes of same pitch that are played as single note, even across several bars.
 * Ties are started by "(" and ended by ")", notes in between the start and end note MUST be enclosed in "(...)".
 * No space must be between "(" or ")" and note.
 * <p/>
 * Notes connected by "slur" are notes of different pitch that are phrased together, even across several bars.
 * Slurs are started by "{" and ended by "}", notes in between MUST NOT be enclosed in "{...}".
 * No space must be between "{" or "}" and note.
 */
public class MelodyFactory
{
    /** Factor by which first note in bar should be louder than subsequent notes. */
    private static final double BAR_START_VOLUME_FACTOR = 1.8;
    
    private static final String DEFAULT_NOTE_LENGTH = "4";
    
    /** The separator between IPN-name and note length (duration). */
    private static final char DURATION_SEPARATOR = '/';
    /** The symbol at the end of dotted notes which have duration factor 1.5. */
    private static final String DOTTED_SYMBOL = ".";
    /** The separator character for triplet and other multiplet numbers. */
    private static final char MULTIPLET_SEPARATOR = ',';
    
    /** The character used to start a tie. */
    private static final String TIE_START_SYMBOL = "(";
    /** The character used to end a tie. */
    private static final String TIE_END_SYMBOL = ")";
    /** The character used to start a slur. */
    private static final String SLUR_START_SYMBOL = "{";
    /** The character used to end a slur. */
    private static final String SLUR_END_SYMBOL = "}";
    
    private final Tones toneSystem;
    private final Integer volume;
    private final int beatDurationMilliseconds;
    
    private Integer numberOfBeatsPerBar; // bar-meter, waltz (3) or foxtrot (4) or ...
    private Integer beatType; // eighth (8) or quarter (4) or ... notes in bar

    /** Factory with default settings. */
    public MelodyFactory() {
        this(null, null, null, null, null);
    }
    
    /** Factory with given tone-system and default settings. */
    public MelodyFactory(ToneSystem toneSystem) {
        this(toneSystem, null, null, null, null);
    }
    
    /**
     * The global settings of any melody this factory will produce.
     * @param toneSystem optional, the tuning the notes should be built from, default is EqualTemperament.
     * @param numberOfBeatsPerMinute optional, tempo, null-default is 120 BPM.
     * @param volume optional, lowest loudness, 1..127, null-default is 7.
     * @param numberOfBeatsPerBar optional, dividend of time signature, for 6/8 this would be 6, null-default is 4. 
     * @param beatType optional, divisor of time signature, for 6/8 this would be 8, null-default is 4.
     */
    public MelodyFactory(
            ToneSystem toneSystem,
            Integer numberOfBeatsPerMinute,
            Integer volume,
            Integer numberOfBeatsPerBar,
            Integer beatType)
    {
        this.toneSystem = (toneSystem != null) ? new Tones(toneSystem.tones()) : new Tones();
        this.volume = (volume != null) ? volume : Note.DEFAULT_VOLUME;
        
        this.numberOfBeatsPerBar = (numberOfBeatsPerBar != null) ? numberOfBeatsPerBar : 4;
        this.beatType = (beatType != null) ? beatType : 4;
        
        final int bpm = (numberOfBeatsPerMinute != null) ? numberOfBeatsPerMinute : Note.DEFAULT_TEMPO_BPM;
        this.beatDurationMilliseconds = (int) Math.round(1000.0 * 60.0 / (double) bpm);
    }
    
    /**
     * Main method. Translates given string-inputs to a playable melody.
     * If a note has no "/" with subsequent length, it will be regarded as quarter-note.
     * @param melodyTokens the sequence of IPN-notes with length to scan, e.g. "C4/4", "D4/8", "B3/8",
     *      also possibly containing bar-meter changes like "3/4" or "6/8" that come without a note behind the slash,
     *      also possibly containing parentheses that lengthen notes, like "(C5/8", "(C5/4)", "C5/8)".
     *      Spaces generally should not matter, but parentheses may not arrive without a note.
     * @return a sequence of <code>Note</code> Java-objects representing <code>notesWithLengths</code>.
     */
    public Note[] translate(String[] melodyTokens) {
        final List<MelodyNote> melodyNotes = new ArrayList<>(melodyTokens.length);
        final List<Note> notes = new ArrayList<>(melodyTokens.length);
        buildNotes(melodyTokens, melodyNotes, notes);
        
        final List<Note> tiedAndSlurredNotes = connectSlurredAndTiedNotes(melodyNotes, notes);
        
        return tiedAndSlurredNotes.toArray(new Note[tiedAndSlurredNotes.size()]);
    }

    private void buildNotes(String[] melodyTokens, List<MelodyNote> melodyNotes, List<Note> notes) {
        BarState barState = new BarState(numberOfBeatsPerBar, beatDurationMilliseconds);
        
        for (int i = 0; i < melodyTokens.length; i++) {
            final MelodyToken melodyToken = newMelodyToken(melodyTokens[i].trim());
            
            if (melodyToken instanceof BarMeterChange) {
                final BarMeterChange barMeterChange = (BarMeterChange) melodyToken;
                this.numberOfBeatsPerBar = barMeterChange.numberOfBeatsPerBar;
                this.beatType = barMeterChange.beatType;
                
                barState = new BarState(barMeterChange.numberOfBeatsPerBar, beatDurationMilliseconds);
            }
            else {
                final MelodyNote melodyNote = (MelodyNote) melodyToken;
                melodyNotes.add(melodyNote);
                
                final Note note = buildNote(melodyNote, barState);
                notes.add(note);
            }
        }
    }

    
    /** Helper that tracks the millisecond position in bar. */
    private static class BarState
    {
        private final int barDurationMilliseconds;
        private final int barDurationMillisecondsHalf;
        private final boolean isBarHalvable;
        
        private int currentMillis = 0;
        
        BarState(int numberOfBeatsPerBar, int beatDurationMilliseconds) {
            this.barDurationMilliseconds = (numberOfBeatsPerBar * beatDurationMilliseconds);
            this.barDurationMillisecondsHalf = (int) Math.round((double) barDurationMilliseconds / 2.0);
            this.isBarHalvable = 
                    numberOfBeatsPerBar % 2 == 0 && // must be divisible by 2
                    numberOfBeatsPerBar % 3 != 0 &&
                    numberOfBeatsPerBar % 5 != 0 &&
                    numberOfBeatsPerBar % 7 != 0 &&
                    numberOfBeatsPerBar % 11 != 0;
                    // do not accept  6/8, 10/8, 12/8, 18/8, 20/8, ... 
                    // you never know how they are sub-divided, thus there could be a wrong accentuation
        }
        
        /** Adds a note to the current bar. */
        public void add(int durationMilliseconds) {
            currentMillis += durationMilliseconds;
            
            if (touches(currentMillis, barDurationMilliseconds))
                currentMillis = 0;
        }
        
        public boolean isBarStart() {
            return currentMillis == 0;
        }
        
        public boolean isBarHalf() {
            return isBarHalvable && // can be reliably divided into 2 parts
                    matches(currentMillis, barDurationMillisecondsHalf);
        }

        private boolean touches(int currentMillis, int limit) {
            return currentMillis > limit ||
                    matches(currentMillis, limit);
        }
    
        private boolean matches(int currentMillis, int limit) {
            return currentMillis == limit ||
                    currentMillis == limit - 1 ||
                    currentMillis == limit + 1;
        }
    }
    

    private MelodyToken newMelodyToken(String melodyToken) {
        final NoteConnections noteConnections = new NoteConnections(melodyToken.trim());
        
        final MelodyNote noteAndLength = splitNoteAndLength(noteConnections.melodyToken);
        
        // when noteName is a number, this is a bar-meter change
        final Integer numberOfBeatsPerBar = toInteger(noteAndLength.ipnName);
        if (numberOfBeatsPerBar != null) {
            // the beat-type must also be a number, else error
            final Integer beatType = toInteger(noteAndLength.length);
            if (beatType != null)
                return new BarMeterChange(numberOfBeatsPerBar, beatType);
            
            throw new IllegalArgumentException("Invalid token, not a note, not a bar-meter: "+melodyToken);
        }
        
        return new MelodyNote(noteAndLength.ipnName, noteAndLength.length, noteConnections);
    }
    
    
    /**
     * Symbols around notes like ties "(A4 (B4) C5)" or slurs "{A4 B4 C5}".
     * A slur is for 2-n notes of different pitch. A tie is for 2-n notes
     * with same pitch, but the parentheses must be repeated on any note within.
     * Slurs and ties can be combined, but slur must be the outer symbol, like "{(A4".
     */
    private static class NoteConnections
    {
        private static class NoteConnection
        {
            public final boolean exists;
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
                
                if (index != 0 && index != melodyToken.length() - 1)
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
        
        NoteConnections(String melodyToken) {
            NoteConnection noteConnection;
            noteConnection = new NoteConnection(melodyToken, SLUR_START_SYMBOL);
            this.slurStart = noteConnection.exists;
            noteConnection = new NoteConnection(noteConnection.editedToken, SLUR_END_SYMBOL);
            this.slurEnd = noteConnection.exists;
            noteConnection = new NoteConnection(noteConnection.editedToken, TIE_START_SYMBOL);
            this.tieStart = noteConnection.exists;
            noteConnection = new NoteConnection(noteConnection.editedToken, TIE_END_SYMBOL);
            this.tieEnd = noteConnection.exists;
            
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
    }

    
    private MelodyNote splitNoteAndLength(String melodyToken) {
        final int durationStartIndex = melodyToken.indexOf(DURATION_SEPARATOR);
        
        final String noteName = ((durationStartIndex > 0)
                ? melodyToken.substring(0, durationStartIndex)
                : melodyToken) // has no length
            .trim();
        
        final String noteLength;
        if (durationStartIndex > 0) {
            final String duration = melodyToken.substring(durationStartIndex + 1).trim();
            if (duration.length() > 0)
                noteLength = duration;
            else
                noteLength = DEFAULT_NOTE_LENGTH;
        }
        else {
            noteLength = DEFAULT_NOTE_LENGTH;
        }
        
        return new MelodyNote(noteName, noteLength, null);
    }

    private Integer toInteger(final String string) {
        try {
            return Integer.valueOf(string);
        }
        catch (NumberFormatException e) { // is legal here
            return null;
        }
    }
    
    
    /** Marker interface. */
    private interface MelodyToken
    {
    }
    
    /** Normal melody note. */
    private static class MelodyNote implements MelodyToken
    {
        public final String ipnName;
        public final String length;
        public final NoteConnections noteConnections;

        /** Note constructor. */
        MelodyNote(String ipnName, String length, NoteConnections noteConnections) {
            this.ipnName = ipnName;
            this.length = length;
            this.noteConnections = noteConnections;
        }
    }
    
    /** Bar-meter change, e.g. going from 3/3 to 4/4 bar during tune. */
    private static class BarMeterChange implements MelodyToken
    {
        public final Integer numberOfBeatsPerBar;
        public final Integer beatType;

        /** Note constructor. */
        BarMeterChange(Integer numberOfBeatsPerBar, Integer beatType) {
            this.numberOfBeatsPerBar = numberOfBeatsPerBar;
            this.beatType = beatType;
        }
    }
    
    
    private Note buildNote(MelodyNote melodyNote, BarState barState) {
        final int duration = durationMilliseconds(melodyNote.length);
        
        final double volumeFactor = getVolumeFactor(barState);
        final int volumeInBar = (int) Math.round(volumeFactor * (double) volume);
        final boolean emphasized = (volumeFactor == BAR_START_VOLUME_FACTOR);
        
        // finally skip barState to next note
        barState.add(duration);

        return new Note(
                toneSystem,
                melodyNote.ipnName, 
                duration,
                volumeInBar,
                emphasized,
                null,
                null);
    }

    private int durationMilliseconds(String noteLength) {
        String noteLengthString = noteLength;
        // remove optional dot at right end
        final boolean dotted = noteLengthString.endsWith(DOTTED_SYMBOL);
        if (dotted)
            noteLengthString = noteLengthString.substring(0, noteLengthString.length() - DOTTED_SYMBOL.length());
        
        // remove optional multiplet-type, left of dot
        final int multipletSeparatorIndex = noteLengthString.indexOf(MULTIPLET_SEPARATOR);
        final Integer multipletType = getMultipletType(noteLengthString, multipletSeparatorIndex);
        if (multipletType != null)
            noteLengthString = noteLengthString.substring(0, multipletSeparatorIndex);
        
        final Integer length = toInteger(noteLengthString);
        if (length == null)
            throw new IllegalArgumentException("Note length is not a number: "+noteLength);
        
        return toMillis(length, dotted, multipletType);
    }
    
    private Integer getMultipletType(String noteLengthString, int multipletSeparatorIndex) {
        if (multipletSeparatorIndex <= 0)
            return null;
        
        final String multipletString = noteLengthString.substring(multipletSeparatorIndex + 1);
        Integer multipletType = toInteger(multipletString);
        if (multipletType == null)
            throw new IllegalArgumentException("Multiplet type is not a number: "+noteLengthString);
        
        return multipletType;
    }

    private int toMillis(int noteLengthDivisor, boolean isDottedNote, Integer multipletType) {
        final double beatFactor = (double) beatType / (double) noteLengthDivisor;
        // "C4/1" in a 3/4 waltz must be written as "C4/2."! (mind the dot)
        double millis = (double) beatDurationMilliseconds * beatFactor;
        
        if (multipletType != null)
            millis = calculateMultipletDuration(multipletType, millis);
        
        if (isDottedNote)
            millis *= 1.5; // dotted factor
        
        return (int) Math.round(millis);
    }

    private double calculateMultipletDuration(Integer multipletType, double millis) {
        final double factor;
        // duplets (2) or quadruplets (4) will occur only in 3-ticks-per-beat, 6/8 or 12/8 
        switch (multipletType) {
        case 2: // duplets take 3/2 duration, 2 notes on the duration of 3 equal notes
            if (numberOfBeatsPerBar % 3 != 0)
                throw new IllegalStateException("Do not use duplets in "+numberOfBeatsPerBar+"/"+beatType+" measures!");
            factor = 3.0 / 2.0; 
            break;
        case 3: // triplets take 2/3 duration, 3 notes on the duration of 2 equal notes
        case 6: // sixtuplets take 4/6==2/3 duration, 6 notes on the duration of 4 equal notes
            factor = 2.0 / 3.0; 
            break;
        case 4: // quadruplets take 3/4 duration, 4 notes on the duration of 3 equal notes
            if (numberOfBeatsPerBar % 3 != 0)
                throw new IllegalStateException("Do not use quadruplets in "+numberOfBeatsPerBar+"/"+beatType+" measures!");
            factor = 3.0 / 4.0;
            break;
        case 5: // quintuplets take 4/5 duration, 5 notes on the duration of 4 equal notes
            factor = 4.0 / 5.0;
            break;
        default:
            throw new IllegalStateException("Unsupported multiplet type: "+multipletType);
        }
        return millis * factor;
        /*
         * Following spec from https://abcwiki.org/abc:syntax
         * contradicts https://en.wikipedia.org/wiki/Tuplet#Rhythm where "(5" is described as 5 over 4,
         *     
           If the time signature is compound (6/8, 9/8, 12/8) then n is three, otherwise n is two.
            "(2" = 2 notes in the time of 3
            "(3" = 3 notes in the time of 2
            "(4" = 4 notes in the time of 3
            "(5" = 5 notes in the time of n
            "(6" = 6 notes in the time of 2
            "(7" = 7 notes in the time of n
            "(8" = 8 notes in the time of 3
            "(9" = 9 notes in the time of n
         */
    }

    /** Notes at first beat should be louder. */
    private double getVolumeFactor(BarState barState) {
        if (barState.isBarStart())
            return BAR_START_VOLUME_FACTOR; // being at first beat
        
        if (barState.isBarHalf())
            return 1.4; // being in middle of the bar
        
        return 1.0;
    }
    
    
    private List<Note> connectSlurredAndTiedNotes(List<MelodyNote> melodyNotes, List<Note> notes) {
        final List<Note> tiedAndSluredMelody = new ArrayList<>(melodyNotes.size());
        
        boolean inTie = false;
        boolean inSlur = false;
        for (int i = 0; i < melodyNotes.size(); i++) {
            final MelodyNote melodyNote = melodyNotes.get(i);
            final Note note = notes.get(i);
            
            final boolean slurStart = (inSlur == false && melodyNote.noteConnections.isSlurStart());
            final boolean slurEnd   = (inSlur == true  && melodyNote.noteConnections.isSlurEnd());
            final Boolean slurred = slurEnd ? Boolean.FALSE : (slurStart || inSlur) ? Boolean.TRUE : null;
            
            final boolean tieStart = (inTie == false && melodyNote.noteConnections.isTieStart());
            final boolean tieEnd   = (inTie == true  && melodyNote.noteConnections.isTieEnd());
            final Boolean tied = tieEnd ? Boolean.FALSE : (tieStart || inTie) ? Boolean.TRUE : null;
            
            final int durationMilliseconds;
            if (tieStart)
                durationMilliseconds = sumTiedDurations(notes, i, melodyNotes);
            else if (inTie) // durations have been moved to note where tie starts
                durationMilliseconds = 0;
            else // not in tie
                durationMilliseconds = note.durationMilliseconds;
            
            final Note tiedAndSlurredNote = new Note(note, durationMilliseconds, slurred, tied);
            tiedAndSluredMelody.add(tiedAndSlurredNote);
            
            inSlur = slurStart ? true : slurEnd ? false : inSlur;
            inTie = tieStart ? true : tieEnd ? false : inTie;
        }
        return tiedAndSluredMelody;
    }
    
    private int sumTiedDurations(List<Note> notes, int fromIndex, List<MelodyNote> melodyNotes) {
        int duration = 0;
        int i = fromIndex;
        NoteConnections connections;
        do {
            connections = melodyNotes.get(i).noteConnections;
            duration += notes.get(i).durationMilliseconds;
            i++;
        }
        while (i < notes.size() && connections.isTieEnd() == false);
        return duration;
    }
}