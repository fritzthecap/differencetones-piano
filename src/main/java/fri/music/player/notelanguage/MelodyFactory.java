package fri.music.player.notelanguage;

import java.util.ArrayList;
import java.util.List;
import fri.music.Tone;
import fri.music.ToneSystem;
import fri.music.Tones;
import fri.music.player.Note;

/**
 * Reads in a number of notes with length, headed by time-signature and tempo, 
 * and produces a melody (array of Note) from it, playable by a <code>Player</code>.
 * Just single notes are possible, no chords.
 * <p/>
 * <b>Input DSL:</b><br/>
<p>
Every note is given as an IPN-name (international pitch notation)
and its length behind a slash, for example: 
</p>
<ul>
<li>"A4/4" for a quarter note on pitch of A4 (440 Hz)</li>
<li>"C#4/2." for a dotted C#4 half note (spans three quarter notes)</li>
<li>"E5/8,3" for a E5 triplet eighth note 
    (each of the triplets must have the ",3" postfix!)</li>
<li>"(G5/1 (G5/1) G5/1)" for a G5 whole note that spans three 4/4 bars</li>
<li>"-/2" for a half rest note.</li>
</ul>
<p>
No space must appear inside notes and their length specification,
but at least one whitespace MUST be between different notes.
In IPN there is no "Eb" or "Bb", you must give "D#" or "A#",
and there is no German "H", such is written as "B".
But you can use both lower or upper case letters in IPN-names.
</p><p>
The time signature can appear on top of the notes, or everywhere in-between,
written as "4/4" or "3/4" or similar.
The tempo can appear as simple BPM number (beats per minute)
on top of the notes only, it can not change in-between.
</p><p>
Do not care about bars, the player will automatically calculate bar bounds
using the given time signature(s).
You can use the "Format" button to put every bar into a separate text line.
</p><p>
Notes connected by a "tie" are notes of same pitch that are played as single note, 
even across several bars.
Ties are started by an parenthesis "(" and ended by ")", 
notes in between the start and end note SHOULD be enclosed in
parentheses, because this is how it would look like in written notes.
Space between parentheses and note is allowed.
</p><p>
Notes connected by a "slur" are notes of different pitch that are phrased together, 
even across several bars.
Slurs are started by a brace "{" and ended by "}", 
notes in between MUST NOT be enclosed in "{...}",
because it is not clear how to phrase several notes that are all slurred together.
Space between braces and note is allowed.
</p><p>
Chord notes are two or more notes of different pitch that are played simultaneously.
Chords are started by a bracket "[" and ended by "]", 
just the first note needs to have a duration, it is assumed that all have the same length.
Ties and slurs from inside to outside of a chord are allowed.
Space between brackets and note is allowed.
</p>
 */
public class MelodyFactory
{
    /** If a note has no length, it will get DEFAULT_NOTE_LENGTH. */
    public static final String DEFAULT_NOTE_LENGTH = "4";
    
    /** If a melody has no time signature, it will get this. */
    public static final Integer DEFAULT_NUMBER_OF_BEATS_PER_BAR = 4;
    /** If a melody has no time signature, it will get this. */
    public static final Integer DEFAULT_BEAT_TYPE = 4;
    
    /** Maximum beats per bar is a dicussed topic. */
    public static final Integer MAXIMUM_BEATS_PER_BAR = 16; //32;
    
    public static final int TEMPO_MINIMUM_BPM = 40;
    public static final int TEMPO_MAXIMUM_BPM = 208;
    
    /** Notes can be 1/1, 1/2, 1/4, 1/. 1/16, 1/32, 1/64, not smaller. */
    public static final int SHORTEST_NOTELENGTH_DIVISOR = 64;

    static final String NEWLINE = System.getProperty("line.separator");

    /** Factor by which first note in bar should be louder than subsequent notes. */
    private static final double BAR_START_VOLUME_FACTOR = 1.8;
    private static final double BAR_HALF_VOLUME_FACTOR = 1.4;
    
    /** The symbol at the end of dotted notes, duration factor 3/2. */
    private static final String DOTTED_SYMBOL = ".";
    /** The separator character for triplet and other multiplet numbers. */
    private static final char MULTIPLET_SEPARATOR = ',';
    
    /** @return the amount of milliseconds one beat would last. */
    public static int beatDurationMillis(int beatsPerMinute) {
        return (int) Math.round(1000.0 * 60.0 / (double) beatsPerMinute);
    }
    
    /** @return the amount of milliseconds a note with given noteLengthDivisor would last. */
    public static double noteLengthMillis(int noteLengthDivisor, int beatType, int beatDurationMillis) {
        final double beatFactor = (double) beatType / (double) noteLengthDivisor;
        return (double) beatDurationMillis * beatFactor;
        // "C4/1" in a 3/4 waltz must be written as "C4/2."! (mind the dot)
    }
    
    /** @return the divisor for a note with given duration in milliseconds. */
    public static String noteLengthDivisor(int durationMillis, int beatType, int beatDurationMillis) {
        if (durationMillis <= 0)
            durationMillis = 1; // System.currentMillis() sometimes returns the same value for two calls
        
        final double beatFactor = (double) beatType / (double) durationMillis;
        final int noteLengthDivisor = (int) Math.round((double) beatDurationMillis * beatFactor);
        
        int minimum = Integer.MAX_VALUE, difference = Integer.MAX_VALUE, nearest = 0;
        for (int i = 1; i <= SHORTEST_NOTELENGTH_DIVISOR && difference <= minimum; i *= 2) {
            difference = Math.abs(noteLengthDivisor - i);
            if (difference < minimum) {
                minimum = difference;
                nearest = i;
            }
        }
        return ""+nearest;
    }
    
    
    private final Tones toneSystem;
    private final Integer volume;
    
    private Integer beatsPerMinute; // BPM
    private Integer beatsPerBar; // time signature, waltz (3) or foxtrot (4) or ...
    private Integer beatType; // beat-notes in bar, eighth (8) or quarter (4) or ... er changes. */
    /** Duration of one beat in milliseconds, package-visible for unit-tests. */
    int beatDurationMilliseconds;

    private String timeSignatureOnTop;
    private Integer tempoOnTop;
    
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
        
        checkValidBeatsPerBar(this.beatsPerBar);
        checkValidNoteLength(this.beatType, "Illegal beat type:");
        checkValidBeatsPerMinute(this.beatsPerMinute);
        
        calculateBeatDurationMilliseconds();
    }
    
    /**
     * Translates given text to a playable melody. See <code>translate(String[])</code> for docs.
     * @param text contains the notes of the melody, including ties and time signature changes.
     * @throws IllegalArgumentException when notes are invalid.
     */
    public Note[][] translate(String text) {
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
    public Note[][] translate(String[] melodyTokens) {
        final List<MelodyToken> melodyNotes = new ArrayList<>(melodyTokens.length);
        
        final List<Note> rawNotes = buildRawNotes(melodyTokens, melodyNotes);
        
        final List<Note[]> connectedNotes = buildConnectedNotes(melodyNotes, rawNotes);
        
        return connectedNotes.toArray(new Note[connectedNotes.size()][]);
    }
    
    /**
     * This does the opposite of <code>translate()</code>, but with each bar in a new line.
     * @param notes the notes to output bar-formatted as string.
     * @return a text where each bar of given notes will be displayed in a new line.
     */
    public String toString(Note[][] notes, boolean writeTempo, boolean writeTimeSignature) {
        final StringBuilder result = new StringBuilder();
        boolean inSlur = false;
        boolean inTie = false;
        boolean inChord = false;
        
        for (int i = 0; i < notes.length; i++) {
            for (Note note : notes[i]) {
                if (note.lengthNotation == null || note.lengthNotation.length() <= 0)
                    throw new IllegalArgumentException("Note has no length: "+note.ipnName);
                
                final boolean addNewlineBeforeNote = (note.emphasized && i > 0);
                
                // tempo can be attached to first note
                if (i == 0 && writeTempo)
                    result.append("" + note.beatInfo.beatsPerMinute() + NEWLINE);
    
                // time-signature changes can be attached to any note
                if ((i > 0 || writeTimeSignature) && note.beatInfo.timeSignature() != null) // initial or changed bar-type
                    result.append(
                        (i <= 0 || endsWithNewline(result) ? "" : NEWLINE) +
                        note.beatInfo.timeSignature() +
                        (addNewlineBeforeNote ? "" : NEWLINE));
                
                if (addNewlineBeforeNote) // break line before first note in bar
                    result.append(NEWLINE);
                
                if (Boolean.TRUE.equals(note.connectionFlags.slurred())) {
                    if (inSlur == false)
                        result.append(NoteConnections.SLUR_START_SYMBOL);
                    inSlur = true;
                }
                
                if (Boolean.TRUE.equals(note.connectionFlags.tied())) {
                    result.append(NoteConnections.TIE_START_SYMBOL);
                }
                    
                if (Boolean.TRUE.equals(note.connectionFlags.chord())) {
                    if (inChord == false)
                        result.append(NoteConnections.CHORD_START_SYMBOL);
                    inChord = true;
                }
                    
                result.append(note.toString());
                
                if (Boolean.TRUE.equals(note.connectionFlags.tied())) {
                    if (inTie == true) // enclose every note within tie into parentheses
                        result.append(NoteConnections.TIE_END_SYMBOL);
                    inTie = true;
                }
                else if (Boolean.FALSE.equals(note.connectionFlags.tied())) {
                    result.append(NoteConnections.TIE_END_SYMBOL);
                    inTie = false;
                }
                
                if (Boolean.FALSE.equals(note.connectionFlags.slurred())) {
                    result.append(NoteConnections.SLUR_END_SYMBOL);
                    inSlur = false;
                }
                
                if (Boolean.FALSE.equals(note.connectionFlags.chord())) {
                    result.append(NoteConnections.CHORD_END_SYMBOL);
                    inChord = false;
                }
                
                if (i < notes.length - 1 && (inChord || notes[i + 1][0].emphasized == false))
                    result.append(" "); // separate notes by space when not last in bar
            }
        }
        
        result.append(NEWLINE);
        return result.toString();
    }
    
    
    /** @return beatsPerMinute on text-area top from latest <code>translate()</code> call, or null if there was none. */
    public Integer getTempoOnTop() {
        return tempoOnTop;
    }

    /** @return "beatsPerBar/beatType" on text-area top from latest <code>translate()</code> call, or null if there was none. */
    public String getTimeSignatureOnTop() {
        return timeSignatureOnTop;
    }
    

    private void calculateBeatDurationMilliseconds() {
        this.beatDurationMilliseconds = beatDurationMillis(this.beatsPerMinute);
    }
    
    
    private String timeSignature(Integer beatsPerBar, Integer beatType) {
        return "" + beatsPerBar + Note.DURATION_SEPARATOR + beatType;        
    }
    
    private void checkValidIpnName(String ipnName) {
        if (ipnName.equals(ToneSystem.REST_SYMBOL) == false &&
                ipnName.matches("[ABCDEFG]#?[0-9]+") == false)
            throw new IllegalArgumentException("Invalid note name: '"+ipnName+"'");
    }

    private void checkValidBeatsPerMinute(Integer beatsPerMinute) {
        if (beatsPerMinute == null || beatsPerMinute < TEMPO_MINIMUM_BPM || beatsPerMinute > TEMPO_MAXIMUM_BPM)
            throw new IllegalArgumentException("Illegal tempo "+beatsPerMinute+" (BPM), must be "+TEMPO_MINIMUM_BPM+" - "+TEMPO_MAXIMUM_BPM+".");
    }
    
    private void checkValidBeatsPerBar(Integer beatsPerBar) throws IllegalArgumentException {
        if (beatsPerBar == null || beatsPerBar < 1 || beatsPerBar > MAXIMUM_BEATS_PER_BAR)
            throw new IllegalArgumentException("Illegal beats per bar: "+beatsPerBar);
    }

    /** To be used for both beatType and noteLength. */
    private void checkValidNoteLength(Integer beatType, String errorMessage) {
        boolean result = false;
        if (beatType != null)
            for (int i = 1; result == false && i <= SHORTEST_NOTELENGTH_DIVISOR; i *= 2) // 1, 2, 4, 8, 16, ...
                if (beatType.intValue() == i)
                    result = true;
        
        if (result == false)
            throw new IllegalArgumentException(errorMessage+" "+beatType+", must be one of 1,2,4,8,16,32,64");
    }
    
    
    private List<Note> buildRawNotes(String[] melodyTokens, List<MelodyToken> resultingNoteTokens) {
        // reset values from last translate call
        timeSignatureOnTop = null;
        tempoOnTop = null;
        
        final List<Note> notes = new ArrayList<>(melodyTokens.length);
        BarState barState = new BarState(beatsPerBar, beatDurationMilliseconds);
        
        String previousTimeSignature = timeSignature(beatsPerBar, beatType);
        String currentTimeSignature = previousTimeSignature;
        boolean previousTokenWasNote = true;
        String chordLength = null;
        
        for (int i = 0; i < melodyTokens.length; i++) {
            final InputToken inputToken = newInputToken(melodyTokens[i].trim(), chordLength);
            
            if (inputToken instanceof BeatsPerMinuteToken) {
                if (notes.size() > 0 || tempoOnTop != null)
                    throw new IllegalArgumentException("Tempo may appear just once on top of notes: "+melodyTokens[i]);
                
                final BeatsPerMinuteToken beatsPerMinute = (BeatsPerMinuteToken) inputToken;
                this.beatsPerMinute = beatsPerMinute.beatsPerMinute;
                calculateBeatDurationMilliseconds();
                
                barState = new BarState(this.beatsPerBar, beatDurationMilliseconds);
                
                tempoOnTop = this.beatsPerMinute;
                
                previousTokenWasNote = false;
            }
            else if (inputToken instanceof TimeSignatureToken) {
                if (previousTokenWasNote == false && (notes.size() > 0 || timeSignatureOnTop != null))
                    throw new IllegalArgumentException("Duplicate time signature: "+melodyTokens[i]);
                
                if (barState.isBarStart() == false)
                    throw new IllegalArgumentException("Time signature change is allowed on bar start only: "+melodyTokens[i]);
                    
                final TimeSignatureToken timeSignature = (TimeSignatureToken) inputToken;
                this.beatsPerBar = timeSignature.beatsPerBar;
                this.beatType = timeSignature.beatType;
                
                barState = new BarState(this.beatsPerBar, beatDurationMilliseconds);
                
                currentTimeSignature = timeSignature(this.beatsPerBar, this.beatType);
                
                if (timeSignatureOnTop == null && notes.size() <= 0)
                    timeSignatureOnTop = currentTimeSignature;
                
                previousTokenWasNote = false;
            }
            else {
                final MelodyToken melodyToken = (MelodyToken) inputToken;
                resultingNoteTokens.add(melodyToken); // needed later for calculating the duration of ties
                
                final boolean beatInfoRequired = // on first note, or when time signature changes
                        (notes.size() <= 0 || previousTimeSignature.equals(currentTimeSignature) == false);
                previousTimeSignature = currentTimeSignature;
                
                final Note note = buildRawNote(melodyToken, barState, currentTimeSignature, beatInfoRequired, chordLength != null);
                notes.add(note);
                
                if (melodyToken.noteConnections.isChordStart())
                    chordLength = melodyToken.length;
                else if (melodyToken.noteConnections.isChordEnd())
                    chordLength = null;
                    
                previousTokenWasNote = true;
            }
        }
        return notes;
    }

    
    private InputToken newInputToken(String melodyToken, String chordLength) {
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
        
        checkValidIpnName(noteAndLength.ipnName);
        
        final String length = (chordLength != null)
                ? chordLength
                : (noteAndLength.length != null)
                    ? noteAndLength.length
                    : DEFAULT_NOTE_LENGTH;
        
        return new MelodyToken(noteAndLength.ipnName, length, noteConnections);
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
        final int durationStartIndex = melodyToken.indexOf(Note.DURATION_SEPARATOR);
        final boolean hasDurationSeparator = (durationStartIndex > 0);
        
        final String noteName = (hasDurationSeparator
                ? melodyToken.substring(0, durationStartIndex)
                : melodyToken) // has no length
            .trim();
        
        final String noteLength;
        if (hasDurationSeparator && melodyToken.length() > durationStartIndex + 1)
            // if there is a non-empty length
            noteLength = melodyToken.substring(durationStartIndex + 1);
        else if (hasDurationSeparator == false)
            noteLength = null;
        else // has duration separator but no length, error!
            throw new IllegalArgumentException("Length is missing: '"+melodyToken+"'");
        
        return new MelodyToken(noteName.toUpperCase(), noteLength, null);
    }

    
    /** Marker interface for one note input unit. */
    private interface InputToken
    {
    }
    
    /** Normal melody note. */
    private class MelodyToken implements InputToken
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
    private class TimeSignatureToken implements InputToken
    {
        public final Integer beatsPerBar; // the 3 in 3/4
        public final Integer beatType; // the 4 in 3/4

        TimeSignatureToken(Integer beatsPerBar, Integer beatType) {
            checkValidBeatsPerBar(beatsPerBar);
            checkValidNoteLength(beatType, "Illegal beat type:");
            
            this.beatsPerBar = beatsPerBar;
            this.beatType = beatType;
        }
    }
    
    /** Tempo in BPM. */
    private class BeatsPerMinuteToken implements InputToken
    {
        public final Integer beatsPerMinute;

        BeatsPerMinuteToken(Integer beatsPerMinute) {
            checkValidBeatsPerMinute(beatsPerMinute);
            this.beatsPerMinute = beatsPerMinute;
        }
    }
    
    
    private Note buildRawNote(
            MelodyToken melodyToken, 
            BarState barState, 
            String currentTimeSignature, 
            boolean beatInfoRequired,
            boolean inChord)
    {
        final int duration = inChord ? 0 : durationMilliseconds(melodyToken.length); // inChord is false on chord start
        
        final double volumeFactor = getVolumeFactor(barState);
        final int volumeInBar = (int) Math.round(volumeFactor * (double) volume);
        final boolean emphasized = (inChord == false) && (volumeFactor == BAR_START_VOLUME_FACTOR);
        
        // skip barState to next note
        barState.add(duration);
        
        final Note.BeatInfo beatInfo;
        if (beatInfoRequired)
            beatInfo = new Note.BeatInfo(
                currentTimeSignature, 
                (tempoOnTop != null) ? tempoOnTop : beatsPerMinute);
        else
            beatInfo = null; // Note constructor will set a non-null default

        if (melodyToken.ipnName.equals(ToneSystem.REST_SYMBOL))
            return new Note(duration, emphasized, melodyToken.length, beatInfo);
        
        final Tone tone = toneSystem.forIpnName(melodyToken.ipnName);
        if (tone == null)
            throw new IllegalArgumentException("IPN note name out of range: "+melodyToken.ipnName);
        
        return new Note( // is a raw note because no tie/slur connections yet
                tone,
                duration,
                volumeInBar,
                emphasized,
                null, // connectionFlags will be added later
                melodyToken.length,
                beatInfo);
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
        
        checkValidNoteLength(length, "Illegal note length:");
        
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
        double millis = noteLengthMillis(noteLengthDivisor, beatType, beatDurationMilliseconds);
        
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
                    throw new IllegalArgumentException("Do not use duplets in "+beatsPerBar+"/"+beatType+" measures!");
                factor = 3.0 / 2.0; 
                break;
            case 3: // triplets take 2/3 duration, 3 notes on the duration of 2 equal notes
            case 6: // sixtuplets take 4/6==2/3 duration, 6 notes on the duration of 4 equal notes
                factor = 2.0 / 3.0; 
                break;
            case 4: // quadruplets take 3/4 duration, 4 notes on the duration of 3 equal notes
                if (beatsPerBar % 3 != 0)
                    throw new IllegalArgumentException("Do not use quadruplets in "+beatsPerBar+"/"+beatType+" measures!");
                factor = 3.0 / 4.0;
                break;
            case 5: // quintuplets take 4/5 duration, 5 notes on the duration of 4 equal notes
                factor = 4.0 / 5.0;
                break;
            default:
                throw new IllegalArgumentException("Unsupported multiplet type: '"+multipletType+"'");
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
    
    
    private List<Note[]> buildConnectedNotes(List<MelodyToken> melodyTokens, List<Note> rawNotes) {
        final List<Note[]> connectedNotes = new ArrayList<>(melodyTokens.size());
        final List<Note> chordNotes = new ArrayList<Note>();
        
        boolean inTie = false;
        boolean inSlur = false;
        boolean inChord = false;
        
        for (int i = 0; i < melodyTokens.size(); i++) {
            final MelodyToken melodyNote = melodyTokens.get(i);
            final Note rawNote = rawNotes.get(i);
            
            final boolean slurStart = (inSlur == false && melodyNote.noteConnections.isSlurStart());
            final boolean slurEnd   = (inSlur == true  && melodyNote.noteConnections.isSlurEnd());
            final Boolean slurred = slurEnd ? Boolean.FALSE : (slurStart || inSlur) ? Boolean.TRUE : null;
            
            final boolean tieStart = (inTie == false && melodyNote.noteConnections.isTieStart());
            final boolean tieEnd   = (inTie == true  && melodyNote.noteConnections.isTieEnd());
            final Boolean tied = tieEnd ? Boolean.FALSE : (tieStart || inTie) ? Boolean.TRUE : null;
            
            final boolean chordStart = (inChord == false && melodyNote.noteConnections.isChordStart());
            final boolean chordEnd   = (inChord == true  && melodyNote.noteConnections.isChordEnd());
            final Boolean chord = chordEnd ? Boolean.FALSE : (chordStart || inChord) ? Boolean.TRUE : null;
            
            final int durationMilliseconds;
            
            if (tieStart)
                durationMilliseconds = sumTiedDurations(rawNotes, i, melodyTokens);
            else if (inTie || inChord) // durations have been moved to note where tie starts
                durationMilliseconds = 0; // only first note of chord gets duration, restricting all to same length
            else // not in tie, not in chord or chord start
                durationMilliseconds = rawNote.durationMilliseconds;
            
            final Note connectedNote = new Note(
                    rawNote, 
                    durationMilliseconds, 
                    new Note.ConnectionFlags(slurred, tied, chord), 
                    rawNote.lengthNotation);
            
            if (chordStart || inChord)
                chordNotes.add(connectedNote);
            else
                connectedNotes.add(new Note[] { connectedNote });
            
            if (chordEnd) {
                connectedNotes.add(chordNotes.toArray(new Note[chordNotes.size()]));
                chordNotes.clear();
            }

            inSlur = slurStart ? true : slurEnd ? false : inSlur;
            inTie = tieStart ? true : tieEnd ? false : inTie;
            inChord = chordStart ? true : chordEnd ? false : inChord;
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
    
    private boolean endsWithNewline(StringBuilder sb) {
        final int newlineLength = NEWLINE.length();
        if (sb.length() < newlineLength)
            return false;
        
        int checkPosition = sb.length() - newlineLength;
        for (int i = 0; i < NEWLINE.length(); i++, checkPosition++)
            if (sb.charAt(checkPosition) != NEWLINE.charAt(i))
                return false;
        
        return true;
    }
}