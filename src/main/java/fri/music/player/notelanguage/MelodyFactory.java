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
 * No space must appear inside notes and their length specification,
 * but at least one whitespace MUST be between notes.
 * A rest is written as "-".
 * In IPN there is no "Eb" or "Bb", you must give "D#" or "A#",
 * and there is no German "H", such is written as "B".
 * <p/>
 * Notes connected by "tie" are notes of same pitch that are played as single note, even across several bars.
 * Ties are started by an parenthesis "(" and ended by ")", notes in between the start and end note SHOULD be enclosed in
 * parentheses, because this is how it would look like in written notes.
 * Space between parentheses and note is allowed.
 * <p/>
 * Notes connected by "slur" are notes of different pitch that are phrased together, even across several bars.
 * Slurs are started by a brace "{" and ended by "}", notes in between MUST NOT be enclosed in "{...}",
 * because it is not clear how to phrase several notes that are all slurred together.
 * Space between braces and note is allowed.
 * <p/>
 * TODO: Chords can be written in brackets [rectangular parentheses].
 * Space between brackets and note is allowed.
 */
public class MelodyFactory
{
    /** If a note has no length, it will get DEFAULT_NOTE_LENGTH. */
    public static final String DEFAULT_NOTE_LENGTH = "4";
    
    /** If a melody has no time signature, it will get this. */
    public static final Integer DEFAULT_NUMBER_OF_BEATS_PER_BAR = 4;
    /** If a melody has no time signature, it will get this. */
    public static final Integer DEFAULT_BEAT_TYPE = 4;
    
    static final String NEWLINE = System.getProperty("line.separator");

    /** Factor by which first note in bar should be louder than subsequent notes. */
    private static final double BAR_START_VOLUME_FACTOR = 1.8;
    private static final double BAR_HALF_VOLUME_FACTOR = 1.4;
    
    /** The separator between IPN-name and note length (duration). */
    private static final char DURATION_SEPARATOR = Note.DURATION_SEPARATOR;
    /** The symbol at the end of dotted notes, duration factor 3/2. */
    private static final String DOTTED_SYMBOL = ".";
    /** The separator character for triplet and other multiplet numbers. */
    private static final char MULTIPLET_SEPARATOR = ',';
    
    
    private final Tones toneSystem;
    private final Integer volume;
    
    private int beatsPerMinute; // BPM
    private Integer beatsPerBar; // time signature, waltz (3) or foxtrot (4) or ...
    private Integer beatType; // beat-notes in bar, eighth (8) or quarter (4) or ... er changes. */
    private int beatDurationMilliseconds; // duration in milliseconds of one beat

    private boolean translateChangedTimeSignature;
    private boolean translateChangedTempo;
    
    /** Factory with default settings. */
    public MelodyFactory() {
        this(null, null, null, null, null);
    }
    
    /** Factory with given tone-system. */
    public MelodyFactory(ToneSystem toneSystem) {
        this(toneSystem, null, null, null);
    }
    
    /** Factory with given time signature. */
    public MelodyFactory(Integer beatsPerBar, Integer beatType) {
        this(null, beatsPerBar, beatType);
    }
    
    /** Factory with given BPM (beats per minute) tempo. */
    public MelodyFactory(Integer beatsPerMinute) {
        this(beatsPerMinute, null, null);
    }
    
    /** Factory with given BPM (beats per minute) tempo. */
    public MelodyFactory(Integer beatsPerMinute, Integer beatsPerBar, Integer beatType) {
        this((ToneSystem) null, beatsPerMinute, beatsPerBar, beatType);
    }
    
    /** Factory with given tone-system, BPM (beats per minute) tempo and time signature. */
    public MelodyFactory(
            ToneSystem toneSystem, 
            Integer beatsPerMinute, 
            Integer beatsPerBar, 
            Integer beatType) {
        this(toneSystem, beatsPerMinute, beatsPerBar, beatType, null);
    }
    
    /**
     * The global settings of any melody this factory will produce.
     * @param toneSystem optional, the tuning the notes should be built from, default is <code>EqualTemperament</code>.
     * @param beatsPerMinute optional, tempo, null-default is 120 BPM.
     * @param beatsPerBar optional, dividend of time signature, for 6/8 this would be 6, null-default is 4. 
     * @param beatType optional, divisor of time signature, for 6/8 this would be 8, null-default is 4.
     * @param volume optional, lowest loudness, 1..127, null-default is 7.
     */
    public MelodyFactory(
            ToneSystem toneSystem,
            Integer beatsPerMinute,
            Integer beatsPerBar,
            Integer beatType,
            Integer volume)
    {
        this.toneSystem = (toneSystem != null) ? new Tones(toneSystem.tones()) : new Tones();
        this.volume = (volume != null) ? volume : Note.DEFAULT_VOLUME;
        
        this.beatsPerMinute = (beatsPerMinute != null) ? beatsPerMinute : Note.DEFAULT_TEMPO_BPM;
        this.beatsPerBar = (beatsPerBar != null) ? beatsPerBar : DEFAULT_NUMBER_OF_BEATS_PER_BAR;
        this.beatType = (beatType != null) ? beatType : DEFAULT_BEAT_TYPE;
        calculateBeatDurationMilliseconds();
    }
    
    /**
     * Translates given text to a playable melody. See <code>translate(String[])</code> for docs.
     * @param text contains the notes of the melody, including ties and time signature changes.
     * @throws IllegalArgumentException when notes are invalid.
     */
    public Note[] translate(String text) {
        return translate(new InputTextScanner().toStringArray(text));
    }
    
    /**
     * Translates given strings to a playable melody.
     * If a note has no "/" with subsequent length, it will be regarded as quarter-note.
     * @param melodyTokens the sequence of IPN-notes with length, e.g. "C4/4", "D4/8", "B3/8",
     *      also possibly containing time signature changes like "3/4" or "6/8",
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
    
    /**
     * This does the opposite of <code>translate()</code>, but with each bar in a new line.
     * @param notes the notes to output bar-formatted as string.
     * @return a text where each bar of given notes will be displayed in a new line.
     */
    public String toString(Note[] notes) {
        final StringBuilder result = new StringBuilder();
        boolean inSlur = false;
        boolean inTie = false;
        
        result.append("" + beatsPerMinute + NEWLINE);
        result.append("" + beatsPerBar + Note.DURATION_SEPARATOR + beatType + NEWLINE);
        
        for (int i = 0; i < notes.length; i++) {
            final Note note = notes[i];
            
            if (note.lengthNotation == null || note.lengthNotation.length() <= 0)
                throw new IllegalArgumentException("Note has no length notation, can not render it: "+note.ipnName);
            
            if (Boolean.TRUE.equals(note.slurred)) {
                if (inSlur == false)
                    result.append(NoteConnections.SLUR_START_SYMBOL);
                inSlur = true;
            }
            
            if (Boolean.TRUE.equals(note.tied)) {
                result.append(NoteConnections.TIE_START_SYMBOL);
            }
                
            if (note.emphasized && i > 0) // break line before fist note in bar
                result.append(NEWLINE);
            
            result.append(note.toString());
            
            if (Boolean.TRUE.equals(note.tied)) {
                if (inTie == true) // enclose every note within tie into parentheses
                    result.append(NoteConnections.TIE_END_SYMBOL);
                inTie = true;
            }
            else if (Boolean.FALSE.equals(note.tied)) {
                result.append(NoteConnections.TIE_END_SYMBOL);
                inTie = false;
            }
            
            if (Boolean.FALSE.equals(note.slurred)) {
                result.append(NoteConnections.SLUR_END_SYMBOL);
                inSlur = false;
            }
            
            if (i < notes.length - 1 && notes[i + 1].emphasized == false)
                result.append(" "); // separate notes by space when not last in bar
        }
        return result.toString();
    }
    
    
    public int getBeatsPerMinute() {
        return beatsPerMinute;
    }

    public int getBeatsPerBar() {
        return beatsPerBar;
    }
    
    public int getBeatType() {
        return beatType;
    }
    
    public boolean translateChangedTimeSignature() {
        return translateChangedTimeSignature;
    }
    
    public boolean translateChangedTempo() {
        return translateChangedTempo;
    }
    

    /** @return the milliseconds one beat lasts, for unit-tests. */
    Integer getBeatDurationMilliseconds() {
        return beatDurationMilliseconds;
    }
    
    private void calculateBeatDurationMilliseconds() {
        this.beatDurationMilliseconds = (int) Math.round(1000.0 * 60.0 / (double) this.beatsPerMinute);
    }
    
    private List<Note> buildNotes(String[] melodyTokens, List<MelodyToken> melodyNotes) {
        translateChangedTimeSignature = translateChangedTempo = false; // reset values from last translate call
        
        final List<Note> notes = new ArrayList<>(melodyTokens.length);
        BarState barState = new BarState(beatsPerBar, beatDurationMilliseconds);
        
        for (int i = 0; i < melodyTokens.length; i++) {
            final InputToken inputToken = newInputToken(melodyTokens[i].trim());
            
            if (inputToken instanceof TimeSignatureToken) {
                final TimeSignatureToken timeSignature = (TimeSignatureToken) inputToken;
                this.beatsPerBar = timeSignature.beatsPerBar;
                this.beatType = timeSignature.beatType;
                
                barState = new BarState(this.beatsPerBar, beatDurationMilliseconds);
                translateChangedTimeSignature = true;
            }
            else if (inputToken instanceof BeatsPerMinuteToken) {
                final BeatsPerMinuteToken beatsPerMinute = (BeatsPerMinuteToken) inputToken;
                this.beatsPerMinute = beatsPerMinute.beatsPerMinute;
                calculateBeatDurationMilliseconds();
                
                barState = new BarState(this.beatsPerBar, beatDurationMilliseconds);
                translateChangedTempo = true;
            }
            else {
                final MelodyToken melodyToken = (MelodyToken) inputToken;
                melodyNotes.add(melodyToken);
                
                final Note note = buildNote(melodyToken, barState);
                notes.add(note);
            }
        }
        return notes;
    }

    private InputToken newInputToken(String melodyToken) {
        final NoteConnections noteConnections = new NoteConnections(melodyToken);
        
        final MelodyToken noteAndLength = splitByDurationSeparator(noteConnections.melodyToken);
        
        // check whether noteName is a number
        final Integer leadingNumber = toIntegerOrNull(noteAndLength.ipnName);
        if (leadingNumber != null) { // could be time signature or BPM
            final Integer beatType = toIntegerOrNull(noteAndLength.length);
            return (beatType != null)
                    ? new TimeSignatureToken(leadingNumber, beatType)
                    : new BeatsPerMinuteToken(leadingNumber);
        }
        
        if (noteAndLength.ipnName.equals(ToneSystem.REST_SYMBOL) == false &&
                noteAndLength.ipnName.matches("[ABCDEFG]#?[0-9]+") == false)
            throw new IllegalArgumentException("Invalid note name: '"+noteAndLength.ipnName+"'");
        
        return new MelodyToken(
                noteAndLength.ipnName, 
                (noteAndLength.length != null) ? noteAndLength.length : DEFAULT_NOTE_LENGTH, 
                noteConnections);
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
        final boolean hasDurationSeparator = (durationStartIndex > 0);
        
        final String noteName = (hasDurationSeparator
                ? melodyToken.substring(0, durationStartIndex)
                : melodyToken) // has no length
            .trim();
        
        final String noteLength;
        if (hasDurationSeparator && melodyToken.length() > durationStartIndex + 1)
            // if there is a non-empty length
            noteLength = melodyToken.substring(durationStartIndex + 1);
        else
            noteLength = null;
        
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

        MelodyToken(String ipnName, String length, NoteConnections noteConnections) {
            this.ipnName = ipnName;
            this.length = length;
            this.noteConnections = noteConnections;
        }
    }
    
    /** Time signature, e.g. 3/4 or 4/4, can change during tune. */
    private static class TimeSignatureToken implements InputToken
    {
        public final Integer beatsPerBar; // the 3 in 3/4
        public final Integer beatType; // the 4 in 3/4

        TimeSignatureToken(Integer beatsPerBar, Integer beatType) {
            this.beatsPerBar = beatsPerBar;
            this.beatType = beatType;
        }
    }
    
    /** Tempo in BPM. */
    private static class BeatsPerMinuteToken implements InputToken
    {
        public final Integer beatsPerMinute;

        BeatsPerMinuteToken(Integer beatsPerMinute) {
            this.beatsPerMinute = beatsPerMinute;
        }
    }
    
    
    private Note buildNote(MelodyToken melodyToken, BarState barState) {
        final int duration = durationMilliseconds(melodyToken.length);
        
        final double volumeFactor = getVolumeFactor(barState);
        final int volumeInBar = (int) Math.round(volumeFactor * (double) volume);
        final boolean emphasized = (volumeFactor == BAR_START_VOLUME_FACTOR);
        
        // finally skip barState to next note
        barState.add(duration);

        if (melodyToken.ipnName.equals(ToneSystem.REST_SYMBOL))
            return new Note(duration, melodyToken.length);
        
        return new Note(
                toneSystem,
                melodyToken.ipnName, 
                duration,
                volumeInBar,
                emphasized,
                (Boolean) null,
                (Boolean) null,
                melodyToken.length);
    }

    private int durationMilliseconds(final String noteLength) {
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
            if (beatsPerBar % 3 != 0)
                throw new IllegalStateException("Do not use duplets in "+beatsPerBar+"/"+beatType+" measures!");
            factor = 3.0 / 2.0; 
            break;
        case 3: // triplets take 2/3 duration, 3 notes on the duration of 2 equal notes
        case 6: // sixtuplets take 4/6==2/3 duration, 6 notes on the duration of 4 equal notes
            factor = 2.0 / 3.0; 
            break;
        case 4: // quadruplets take 3/4 duration, 4 notes on the duration of 3 equal notes
            if (beatsPerBar % 3 != 0)
                throw new IllegalStateException("Do not use quadruplets in "+beatsPerBar+"/"+beatType+" measures!");
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
            return BAR_HALF_VOLUME_FACTOR; // being in middle of the bar
        
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
            
            final Note tiedAndSlurredNote = new Note(rawNote, durationMilliseconds, slurred, tied, rawNote.lengthNotation);
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