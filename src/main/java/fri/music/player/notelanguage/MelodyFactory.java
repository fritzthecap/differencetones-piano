package fri.music.player.notelanguage;

import java.util.ArrayList;
import java.util.List;
import fri.music.ToneSystem;
import fri.music.Tones;
import fri.music.player.Note;

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
 * Ties are started by "(" and ended by ")", notes in between the start and end note SHOULD be enclosed in
 * parentheses, because this is how it would look like in written notes.
 * Space between parentheses and note is allowed.
 * <p/>
 * Notes connected by "slur" are notes of different pitch that are phrased together, even across several bars.
 * Slurs are started by "{" and ended by "}", notes in between MUST NOT be enclosed in "{...}",
 * because it is not clear how to phrase several notes that are all slurred together.
 * Space between braces and note is allowed.
 */
public class MelodyFactory
{
    /** Factor by which first note in bar should be louder than subsequent notes. */
    private static final double BAR_START_VOLUME_FACTOR = 1.8;
    
    private static final String DEFAULT_NOTE_LENGTH = "4";
    
    /** The separator between IPN-name and note length (duration). */
    private static final char DURATION_SEPARATOR = '/';
    /** The symbol at the end of dotted notes, duration factor 3/2. */
    private static final String DOTTED_SYMBOL = ".";
    /** The separator character for triplet and other multiplet numbers. */
    private static final char MULTIPLET_SEPARATOR = ',';
    
    private final Tones toneSystem;
    private final Integer volume;
    /** The duration in milliseconds of one beat. This stays the same even on bar-meter changes. */
    public final int beatDurationMilliseconds;
    
    private Integer numberOfBeatsPerBar; // bar-meter, waltz (3) or foxtrot (4) or ...
    private Integer beatType; // eighth (8) or quarter (4) or ... notes in bar

    /** Factory with default settings. */
    public MelodyFactory() {
        this(null, null, null, null, null);
    }
    
    /** Factory with given tone-system. */
    public MelodyFactory(ToneSystem toneSystem) {
        this(toneSystem, null);
    }
    
    /** Factory with given BPM (beats per minute) tempo. */
    public MelodyFactory(Integer numberOfBeatsPerMinute) {
        this(null, numberOfBeatsPerMinute);
    }
    
    /** Factory with given tone-system and BPM (beats per minute) tempo. */
    public MelodyFactory(ToneSystem toneSystem, Integer numberOfBeatsPerMinute) {
        this(toneSystem, numberOfBeatsPerMinute, null, null);
    }
    
    /** Factory with given tone-system, BPM (beats per minute) tempo and bar-meter. */
    public MelodyFactory(ToneSystem toneSystem, Integer numberOfBeatsPerMinute, Integer numberOfBeatsPerBar, Integer beatType) {
        this(toneSystem, numberOfBeatsPerMinute, numberOfBeatsPerBar, beatType, null);
    }
    
    /**
     * The global settings of any melody this factory will produce.
     * @param toneSystem optional, the tuning the notes should be built from, default is EqualTemperament.
     * @param numberOfBeatsPerMinute optional, tempo, null-default is 120 BPM.
     * @param numberOfBeatsPerBar optional, dividend of time signature, for 6/8 this would be 6, null-default is 4. 
     * @param beatType optional, divisor of time signature, for 6/8 this would be 8, null-default is 4.
     * @param volume optional, lowest loudness, 1..127, null-default is 7.
     */
    public MelodyFactory(
            ToneSystem toneSystem,
            Integer numberOfBeatsPerMinute,
            Integer numberOfBeatsPerBar,
            Integer beatType,
            Integer volume)
    {
        this.toneSystem = (toneSystem != null) ? new Tones(toneSystem.tones()) : new Tones();
        this.volume = (volume != null) ? volume : Note.DEFAULT_VOLUME;
        
        this.numberOfBeatsPerBar = (numberOfBeatsPerBar != null) ? numberOfBeatsPerBar : 4;
        this.beatType = (beatType != null) ? beatType : 4;
        
        final int bpm = (numberOfBeatsPerMinute != null) ? numberOfBeatsPerMinute : Note.DEFAULT_TEMPO_BPM;
        this.beatDurationMilliseconds = (int) Math.round(1000.0 * 60.0 / (double) bpm);
    }
    
    /**
     * Translates given text to a playable melody. See <code>translate(String[])</code> for docs.
     * @param text contains the notes of the melody, including ties and bar-meter changes.
     * @throws IllegalArgumentException when notes are invalid.
     */
    public Note[] translate(String text) {
        return translate(new InputTextScanner().toStringArray(text));
    }
    
    /**
     * Translates given strings to a playable melody.
     * If a note has no "/" with subsequent length, it will be regarded as quarter-note.
     * @param melodyTokens the sequence of IPN-notes with length, e.g. "C4/4", "D4/8", "B3/8",
     *      also possibly containing bar-meter changes like "3/4" or "6/8",
     *      also possibly containing parentheses that lengthen notes, like "(C5/8", "(C5/4)", "C5/8)".
     *      Spaces generally should not matter, but parentheses may not arrive without a note.
     * @return a sequence of <code>Note</code> objects representing <code>melodyTokens</code>.
     * @throws IllegalArgumentException when notes are invalid.
     */
    public Note[] translate(String[] melodyTokens) {
        final List<MelodyToken> melodyNotes = new ArrayList<>(melodyTokens.length);
        
        final List<Note> rawNotes = buildNotes(melodyTokens, melodyNotes);
        
        final List<Note> connectedNotes = connectSlurredAndTiedNotes(melodyNotes, rawNotes);
        
        return connectedNotes.toArray(new Note[connectedNotes.size()]);
    }

    private List<Note> buildNotes(String[] melodyTokens, List<MelodyToken> melodyNotes) {
        final List<Note> notes = new ArrayList<>(melodyTokens.length);
        BarState barState = new BarState(numberOfBeatsPerBar, beatDurationMilliseconds);
        
        for (int i = 0; i < melodyTokens.length; i++) {
            final InputToken melodyToken = newInputToken(melodyTokens[i].trim());
            
            if (melodyToken instanceof BarMeterToken) {
                final BarMeterToken barMeterChange = (BarMeterToken) melodyToken;
                this.numberOfBeatsPerBar = barMeterChange.numberOfBeatsPerBar;
                this.beatType = barMeterChange.beatType;
                
                barState = new BarState(barMeterChange.numberOfBeatsPerBar, beatDurationMilliseconds);
            }
            else {
                final MelodyToken melodyNote = (MelodyToken) melodyToken;
                melodyNotes.add(melodyNote);
                
                final Note note = buildNote(melodyNote, barState);
                notes.add(note);
            }
        }
        return notes;
    }

    private InputToken newInputToken(String melodyToken) {
        final NoteConnections noteConnections = new NoteConnections(melodyToken.trim());
        
        final MelodyToken noteAndLength = splitByDurationSeparator(noteConnections.melodyToken);
        
        // when noteName is a number, then this is a bar-meter change
        final Integer numberOfBeatsPerBar = toIntegerOrNull(noteAndLength.ipnName);
        if (numberOfBeatsPerBar != null) {
            // the beat-type must also be a number, else error
            final Integer beatType = toIntegerOrNull(noteAndLength.length);
            if (beatType != null)
                return new BarMeterToken(numberOfBeatsPerBar, beatType);
            
            throw new IllegalArgumentException("Not a note, not a bar-meter: "+melodyToken);
        }
        
        if (noteAndLength.ipnName.matches("[ABCDEFG]#?[0-9]+") == false)
            throw new IllegalArgumentException("Invalid note name: '"+noteAndLength.ipnName+"'");
        
        return new MelodyToken(noteAndLength.ipnName, noteAndLength.length, noteConnections);
    }
    
    private Integer toIntegerOrNull(final String string) {
        try {
            return Integer.valueOf(string);
        }
        catch (NumberFormatException e) { // is legal here
            return null;
        }
    }
    
    
    private MelodyToken splitByDurationSeparator(String melodyToken) {
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
        
        return new MelodyToken(noteName, noteLength, null);
    }

    
    /** Marker interface for one note input unit. */
    private interface InputToken
    {
    }
    
    /** Normal melody note. */
    private static class MelodyToken implements InputToken
    {
        public final String ipnName;
        public final String length;
        public final NoteConnections noteConnections;

        /** Note constructor. */
        MelodyToken(String ipnName, String length, NoteConnections noteConnections) {
            this.ipnName = ipnName;
            this.length = length;
            this.noteConnections = noteConnections;
        }
    }
    
    /** Bar-meter change, e.g. going from 3/3 to 4/4 bar during tune. */
    private static class BarMeterToken implements InputToken
    {
        public final Integer numberOfBeatsPerBar;
        public final Integer beatType;

        /** Note constructor. */
        BarMeterToken(Integer numberOfBeatsPerBar, Integer beatType) {
            this.numberOfBeatsPerBar = numberOfBeatsPerBar;
            this.beatType = beatType;
        }
    }
    
    
    private Note buildNote(MelodyToken melodyToken, BarState barState) {
        final int duration = durationMilliseconds(melodyToken.length);
        
        final double volumeFactor = getVolumeFactor(barState);
        final int volumeInBar = (int) Math.round(volumeFactor * (double) volume);
        final boolean emphasized = (volumeFactor == BAR_START_VOLUME_FACTOR);
        
        // finally skip barState to next note
        barState.add(duration);

        return new Note(
                toneSystem,
                melodyToken.ipnName, 
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
        
        final Integer length = toIntegerOrNull(noteLengthString);
        if (length == null)
            throw new IllegalArgumentException("Note length is not a number: '"+noteLength+"'");
        
        return toMillis(length, dotted, multipletType);
    }
    
    private Integer getMultipletType(String noteLengthString, int multipletSeparatorIndex) {
        if (multipletSeparatorIndex <= 0)
            return null;
        
        final String multipletString = noteLengthString.substring(multipletSeparatorIndex + 1);
        Integer multipletType = toIntegerOrNull(multipletString);
        if (multipletType == null)
            throw new IllegalArgumentException("Multiplet type is not a number: '"+noteLengthString+"'");
        
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
            throw new IllegalStateException("Unsupported multiplet type: '"+multipletType+"'");
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
    
    
    private List<Note> connectSlurredAndTiedNotes(List<MelodyToken> melodyTokens, List<Note> rawNotes) {
        final List<Note> connectedNotes = new ArrayList<>(melodyTokens.size());
        boolean inTie = false;
        boolean inSlur = false;
        for (int i = 0; i < melodyTokens.size(); i++) {
            final MelodyToken melodyNote = melodyTokens.get(i);
            final Note rawNote = rawNotes.get(i);
            
            final boolean slurStart = (inSlur == false && melodyNote.noteConnections.isSlurStart());
            final boolean slurEnd   = (inSlur == true  && melodyNote.noteConnections.isSlurEnd());
            final Boolean slurred = slurEnd ? Boolean.FALSE : (slurStart || inSlur) ? Boolean.TRUE : null;
            
            final boolean tieStart = (inTie == false && melodyNote.noteConnections.isTieStart());
            final boolean tieEnd   = (inTie == true  && melodyNote.noteConnections.isTieEnd());
            final Boolean tied = tieEnd ? Boolean.FALSE : (tieStart || inTie) ? Boolean.TRUE : null;
            
            final int durationMilliseconds;
            if (tieStart)
                durationMilliseconds = sumTiedDurations(rawNotes, i, melodyTokens);
            else if (inTie) // durations have been moved to note where tie starts
                durationMilliseconds = 0;
            else // not in tie
                durationMilliseconds = rawNote.durationMilliseconds;
            
            final Note tiedAndSlurredNote = new Note(rawNote, durationMilliseconds, slurred, tied);
            connectedNotes.add(tiedAndSlurredNote);
            
            inSlur = slurStart ? true : slurEnd ? false : inSlur;
            inTie = tieStart ? true : tieEnd ? false : inTie;
        }
        return connectedNotes;
    }
    
    private int sumTiedDurations(List<Note> rawNotes, int fromIndex, List<MelodyToken> melodyTokens) {
        int duration = 0;
        int i = fromIndex;
        NoteConnections connections;
        do {
            connections = melodyTokens.get(i).noteConnections;
            duration += rawNotes.get(i).durationMilliseconds;
            i++;
        }
        while (i < rawNotes.size() && connections.isTieEnd() == false);
        return duration;
    }
}