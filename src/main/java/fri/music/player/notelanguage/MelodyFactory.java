package fri.music.player.notelanguage;

import java.util.ArrayList;
import java.util.List;
import fri.music.TextUtil;
import fri.music.Tone;
import fri.music.ToneSystem;
import fri.music.Tones;
import fri.music.player.Multiplet;
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
<li>"E5/16~3" for a E5 triplet sixteenth note 
    (each of the triplets must have the "~3" postfix!)
    A quarter note triplet must start with a quarter note 
    (that may be tied to a subsequent one),
    but it MUST NOT start with an eighth note or a half note,
    same applies to eighth or half note triplets.</li>
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
    
    static final String NEWLINE = TextUtil.NEWLINE;

    /** Factor by which first note in bar should be louder than subsequent notes. */
    private static final double BAR_START_VOLUME_FACTOR = 1.8;
    private static final double BAR_HALF_VOLUME_FACTOR = 1.4;
    
    
    /** @return the amount of milliseconds a note with given noteLengthDivisor would last. */
    public static double noteLengthMillis(int noteLengthDivisor, int beatType, int beatDurationMillis) {
        final double beatFactor = (double) beatType / (double) noteLengthDivisor;
        return (double) beatDurationMillis * beatFactor;
        // "C4/1" in a 3/4 waltz must be written as "C4/2." (mind the dot!)
    }
    
    /** @return the divisor for a note with given duration in milliseconds. */
    public static String noteLengthDivisor(int durationMillis, int beatType, int beatDurationMillis) {
        if (durationMillis <= 0)
            durationMillis = 1; // System.currentMillis() sometimes returns the same value for two calls
        
        final double beatFactor = (double) beatType / (double) durationMillis;
        final int noteLengthDivisor = (int) Math.round((double) beatDurationMillis * beatFactor);
        
        int minimum = Integer.MAX_VALUE, difference = Integer.MAX_VALUE, nearest = 0;
        for (int i = 1; i <= Note.SHORTEST_NOTELENGTH_DIVISOR && difference <= minimum; i *= 2) {
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
        
        if (melodyNotes.size() != rawNotes.size())
            throw new IllegalArgumentException("Built "+rawNotes.size()+" raw notes but given were "+melodyNotes.size()+" melody notes!");
        
        final List<Note[]> connectedNotes = buildConnectedNotes(melodyNotes, rawNotes);
        
        return connectedNotes.toArray(new Note[connectedNotes.size()][]);
    }
    
    /**
     * This does the opposite of <code>translate()</code>, but with each bar in a new line.
     * @param notes the notes to output bar-formatted as string.
     * @return a text where each bar of given notes will be displayed in a new line.
     */
    public String formatBarLines(Note[][] notes, boolean writeTempo, boolean writeTimeSignature) {
        final StringBuilder result = new StringBuilder();
        boolean inSlur = false;
        boolean inTie = false;
        boolean inChord = false;
        
        for (int chordIndex = 0; chordIndex < notes.length; chordIndex++) {
            final Note[] chord = notes[chordIndex];
            Note note = chord[0];
            
            // tempo can be attached to first note only
            if (chordIndex == 0 && writeTempo)
                result.append("" + note.beatInfo.beatsPerMinute() + NEWLINE);
            
            final boolean barStart = note.emphasized;
            final boolean addNewline = (barStart && chordIndex > 0);
                
            // time-signature change can be attached to any note at bar-start
            if ((chordIndex > 0 || writeTimeSignature) && note.beatInfo.timeSignature() != null) // initial or changed bar-type
                result.append(
                    (chordIndex <= 0 || TextUtil.endsWithNewline(result) ? "" : NEWLINE) +
                    note.beatInfo.timeSignature() +
                    (addNewline ? "" : NEWLINE)); // add newline only if it wouldn't be added later
            
            if (addNewline) // break line before first note in bar
                result.append(NEWLINE);
            else if (chordIndex > 0)
                result.append(" ");
            
            if (Boolean.TRUE.equals(note.connectionFlags.slurred())) {
                if (inSlur == false)
                    result.append(NoteConnections.SLUR_START_SYMBOL);
                inSlur = true;
            }
            
            if (Boolean.TRUE.equals(note.connectionFlags.tied())) {
                if (inTie == false && inChord == false)
                    result.append(NoteConnections.TIE_START_SYMBOL);
                inTie = true;
            }
            
            if (chord.length > 1) {
                result.append(NoteConnections.CHORD_START_SYMBOL);
                inChord = true;
            }
            else {
                inChord = false;
            }
            
            for (int i = 0; i < chord.length; i++) { // mostly there will be just one note in chord
                note = chord[i];
                result.append(note.toString());
                
                if (i < chord.length - 1)
                    result.append(" ");
            }
            
            if (inChord)
                result.append(NoteConnections.CHORD_END_SYMBOL);
            
            if (Boolean.FALSE.equals(note.connectionFlags.tied())) {
                result.append(NoteConnections.TIE_END_SYMBOL);
                inTie = false;
            }
            
            if (Boolean.FALSE.equals(note.connectionFlags.slurred())) {
                result.append(NoteConnections.SLUR_END_SYMBOL);
                inSlur = false;
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
        this.beatDurationMilliseconds = Note.beatDurationMillis(this.beatsPerMinute);
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
        if (beatsPerMinute == null || beatsPerMinute < Note.TEMPO_MINIMUM_BPM || beatsPerMinute > Note.TEMPO_MAXIMUM_BPM)
            throw new IllegalArgumentException("Illegal tempo "+beatsPerMinute+" (BPM), must be "+Note.TEMPO_MINIMUM_BPM+" - "+Note.TEMPO_MAXIMUM_BPM);
    }
    
    private void checkValidBeatsPerBar(Integer beatsPerBar) throws IllegalArgumentException {
        if (beatsPerBar == null || beatsPerBar < 1 || beatsPerBar > MAXIMUM_BEATS_PER_BAR)
            throw new IllegalArgumentException("Illegal beats per bar: "+beatsPerBar);
    }

    /** To be used for both beatType and noteLength. */
    private void checkValidNoteLength(Integer beatType, String errorMessage) {
        boolean result = false;
        if (beatType != null)
            for (int i = 1; result == false && i <= Note.SHORTEST_NOTELENGTH_DIVISOR; i *= 2) // 1, 2, 4, 8, 16, ...
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
        Note firstChordNote = null;
        
        for (int i = 0; i < melodyTokens.length; i++) {
            final InputToken inputToken = newInputToken(
                    melodyTokens[i].trim(), 
                    (firstChordNote != null) ? firstChordNote.lengthNotation : null);
            
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
                
                final Note note = buildRawNote(melodyToken, barState, currentTimeSignature, beatInfoRequired, firstChordNote);
                notes.add(note);
                
                if (melodyToken.noteConnections.isChordStart())
                    firstChordNote = note;
                else if (melodyToken.noteConnections.isChordEnd())
                    firstChordNote = null;
                    
                previousTokenWasNote = true;
            }
        }
        return notes;
    }

    
    private InputToken newInputToken(String melodyToken, String firstChordNoteLength) {
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
        
        final String length = (firstChordNoteLength != null)
                ? firstChordNoteLength
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
        
        private int overallMultipletDuration;
        private Integer multipletType;

        MelodyToken(String ipnName, String length, NoteConnections noteConnections) {
            this.ipnName = ipnName;
            this.length = length;
            this.noteConnections = noteConnections;
        }
        
        /**
         * This is needed to know where a triplet (multiplet) ends.
         * @param overallMultipletDuration gives the duration sum of this and all
         *      subsequent multiplet notes, assuming they have same length.
         */
        public void setOverallMultipletDuration(int overallMultipletDuration, Integer multipletType) {
            this.overallMultipletDuration = overallMultipletDuration;
            this.multipletType = multipletType;
        }
        /** @return the duration sum of this and all subsequent multiplet notes, assuming they have same length. */
        public int getOverallMultipletDuration() {
            return overallMultipletDuration;
        }
        public Integer getMultipletType() {
            return multipletType;
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
            Note firstChordNote) // not null from 2nd chord note on, give all notes in a chord first note's length
    {
        final DurationWithMultiplet durationAndMultiplet = durationMilliseconds(melodyToken.length);
        final int duration = durationAndMultiplet.duration;
        
        final double volumeFactor = getVolumeFactor(barState);
        final int volumeOfNote = (firstChordNote != null)
                ? firstChordNote.volume
                : (int) Math.round(volumeFactor * (double) this.volume);
        final boolean emphasized = (firstChordNote != null)
                ? firstChordNote.emphasized
                : (volumeFactor == BAR_START_VOLUME_FACTOR);
        
        // increment barState when not in chord
        barState.add((firstChordNote == null) ? duration : 0);
        
        final Note.BeatInfo beatInfo;
        if (beatInfoRequired)
            beatInfo = new Note.BeatInfo(
                currentTimeSignature, 
                (tempoOnTop != null) ? tempoOnTop : beatsPerMinute);
        else
            beatInfo = null; // Note constructor will set a non-null default

        final Note rawNote;
        if (melodyToken.ipnName.equals(ToneSystem.REST_SYMBOL)) {
            if (firstChordNote != null)
                throw new IllegalArgumentException("A rest can not be in a chord: "+melodyToken.ipnName+"/"+melodyToken.length);
            
            rawNote = new Note(
                    duration,
                    emphasized,
                    melodyToken.length,
                    beatInfo);
        }
        else {
            final Tone tone = toneSystem.forIpnName(melodyToken.ipnName);
            if (tone == null)
                throw new IllegalArgumentException("Unknown note name: "+melodyToken.ipnName);
            
            rawNote = new Note( // is a raw note because no tie/slur connections yet
                    tone,
                    duration,
                    volumeOfNote,
                    emphasized,
                    null, // connectionFlags will be added later
                    melodyToken.length,
                    beatInfo);
        }
        
        if (durationAndMultiplet.overallMultipletDuration > 0)
            melodyToken.setOverallMultipletDuration(
                    durationAndMultiplet.overallMultipletDuration,
                    durationAndMultiplet.multipletType);
        
        return rawNote;
    }

    
    private record DurationWithMultiplet(int duration, int overallMultipletDuration, Integer multipletType)
    {
    }
    
    private DurationWithMultiplet durationMilliseconds(String noteLength) {
        // remove optional dot at right end
        final boolean dotted = noteLength.endsWith(Note.DOTTED_SYMBOL);
        if (dotted)
            noteLength = noteLength.substring(0, noteLength.length() - Note.DOTTED_SYMBOL.length());
        
        // remove optional multiplet-type, left of dot
        final int multipletSeparatorIndex = noteLength.indexOf(Note.MULTIPLET_SEPARATOR);
        final Integer multipletType = getMultipletType(noteLength, multipletSeparatorIndex);
        if (multipletType != null)
            noteLength = noteLength.substring(0, multipletSeparatorIndex);
        
        final Integer length = toIntegerOrNull(noteLength);
        checkValidNoteLength(length, "Unknown note length:");
        
        final int[] durations = toMillis(length, dotted, multipletType);
        
        return new DurationWithMultiplet(durations[0], durations[1], multipletType);
    }
    
    private Integer getMultipletType(String noteLengthString, int multipletSeparatorIndex) {
        if (multipletSeparatorIndex <= 0)
            return null;
        
        final String multipletString = noteLengthString.substring(multipletSeparatorIndex + 1);
        final Integer multipletType = toIntegerOrNull(multipletString);
        if (multipletType == null)
            throw new IllegalArgumentException("Multiplet type is not a number: '"+noteLengthString+"'");
        
        return multipletType;
    }

    private int[] toMillis(int noteLengthDivisor, boolean isDottedNote, Integer multipletType) {
        double millis = noteLengthMillis(noteLengthDivisor, beatType, beatDurationMilliseconds);
        
        final int overallMultipletDuration;
        if (multipletType != null) {
            double[] durations = calculateMultipletDuration(multipletType, millis);
            millis = durations[0];
            overallMultipletDuration = (int) Math.round(durations[1]);
        }
        else {
            overallMultipletDuration = -1;
        }
        
        if (isDottedNote)
            millis *= 1.5; // dotted factor
        
        return new int[] { 
                (int) Math.round(millis), 
                overallMultipletDuration
            };
    }

    private double[] calculateMultipletDuration(Integer multipletType, double millis) {
        // duplets (2) or quadruplets (4) will occur only in "compound" time-signatures like 6/8 or 12/8
        final boolean compound = (beatsPerBar % 3 == 0);
        if (compound == false) // is even time-signature like 4/4
            if (multipletType == 2) // duplet
                throwUnsupportedMultiplet("duplets");
            else if (multipletType == 4) // quadruplet
                throwUnsupportedMultiplet("quadruplets");

        return Multiplet.toEnum(multipletType).adjust(millis);
        /*
         * Following specification from https://abcwiki.org/abc:syntax#duplets_triplets_quadruplets_etc
         * contradicts https://en.wikipedia.org/wiki/Tuplet#Rhythm 
         * where a quintole is described as 5 over 4, and a sextole as 6 over 4:
               If the time signature is compound (6/8, 9/8, 12/8) then n is 3, otherwise n is 2.
                "(5" = 5 notes in the time of n
                "(6" = 6 notes in the time of 2
         */
    }
    
    private void throwUnsupportedMultiplet(String multipletType) {
        throw new IllegalArgumentException("In "+beatsPerBar+"/"+beatType+" time-signatures, "+multipletType+" are ambiguous and thus not supported!");
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
        
        int overallMultipletDuration = 0;
        int multipletDuration = 0;
        
        for (int i = 0; i < melodyTokens.size(); i++) {
            final MelodyToken melodyToken = melodyTokens.get(i);
            final Note rawNote = rawNotes.get(i);
            
            final boolean slurStart = (inSlur == false && melodyToken.noteConnections.isSlurStart());
            final boolean slurEnd   = (inSlur == true  && melodyToken.noteConnections.isSlurEnd());
            final Boolean slurred = slurEnd ? Boolean.FALSE : (slurStart || inSlur) ? Boolean.TRUE : null;
            
            final boolean tieStart = (inTie == false && melodyToken.noteConnections.isTieStart());
            final boolean tieEnd   = (inTie == true  && melodyToken.noteConnections.isTieEnd());
            final Boolean tied = tieEnd ? Boolean.FALSE : (tieStart || inTie) ? Boolean.TRUE : null;
            
            final boolean chordStart = (inChord == false && melodyToken.noteConnections.isChordStart());
            final boolean chordEnd   = (inChord == true  && melodyToken.noteConnections.isChordEnd());
            final Boolean chord = chordEnd ? Boolean.FALSE : (chordStart || inChord) ? Boolean.TRUE : null;
            
            final Boolean multiplet;
            if (overallMultipletDuration <= 0) {
                if ((overallMultipletDuration = melodyToken.getOverallMultipletDuration()) > 0) { // start of a multiplet
                    multipletDuration = rawNote.durationMilliseconds;
                    multiplet = Boolean.TRUE;
                }
                else {
                    multiplet = null;
                }
            }
            else { // running multiplet
                multipletDuration += rawNote.durationMilliseconds;
                if (overallMultipletDuration <= multipletDuration + Note.MINIMAL_DURATION) { // 999 is 1000
                    overallMultipletDuration = 0; // terminate multiplet
                    multiplet = Boolean.FALSE;
                }
                else {
                    multiplet = Boolean.TRUE;
                }
            }
      
            final int durationMilliseconds;
            
            if (tieStart)
                durationMilliseconds = sumTiedDurations(rawNotes, i, melodyTokens, chord);
            else if (inChord) // all chord notes have same length
                durationMilliseconds = chordNotes.get(0).durationMilliseconds;
            else if (inTie) // duration gets summed on note where tie starts
                durationMilliseconds = 0;
            else // not in tie
                durationMilliseconds = rawNote.durationMilliseconds;
            
            final Note connectedNote = new Note(
                    rawNote, 
                    durationMilliseconds, 
                    new Note.ConnectionFlags(slurred, tied, chord, multiplet, melodyToken.getMultipletType()), 
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
    
    private int sumTiedDurations(List<Note> rawNotes, int fromIndex, List<MelodyToken> melodyTokens, Boolean chord) {
        final Note startNote = rawNotes.get(fromIndex);
        int duration = startNote.durationMilliseconds;
        int i = fromIndex + 1;
        // sum duration until tie ends
        NoteConnections connections;
        do {
            connections = melodyTokens.get(i).noteConnections;
            // if in chord, go to end of chord
            if (chord != null) {
                if (connections.isChordEnd() == true)
                    chord = null;
            }
            else {
                final Note rawNote = rawNotes.get(i);
                duration += rawNote.durationMilliseconds;
                if (connections.isChordStart() == true)
                    chord = Boolean.TRUE;
            }
            i++;
        }
        while (i < rawNotes.size() && connections.isTieEnd() == false);

        return duration;
    }
}