package fri.music.player;

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
 * In IPN there is no "Eb" or "Bb", you must give "D#" or "A#",
 * and there is no German "H", such is written as "B".
 */
public class MelodyFactory
{
    /** Factor by which first note in bar should be louder than subsequent notes. */
    private static final double BAR_START_VOLUME_FACTOR = 1.8;
    
    /** The separator between IPN-name and note length (duration). */
    private static final char DURATION_SEPARATOR = '/';
    /** The symbol at the end of dotted notes which have duration factor 1.5. */
    private static final String DOTTED_SYMBOL = ".";
    /** The separator character for triplet add other multiplet numbers. */
    private static final char MULTIPLET_SEPARATOR = ',';
    
    private final Tones toneSystem;
    private final Integer numberOfBeatsPerBar;
    private final Integer beatType; // eight or quarter notes
    private final Integer volume;

    private final int beatDurationMilliseconds;
    private final int barDurationMilliseconds;
    private final int barDurationMillisecondsHalf;
    
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
     * @param numberOfBeatsPerBar optional, dividend of time signature, for 6/8 this would be 6, null-default is 4. 
     * @param beatType optional, divisor of time signature, for 6/8 this would be 8, null-default is 4.
     * @param numberOfBeatsPerMinute optional, tempo, null-default is 120 BPM.
     * @param volume optional, lowest loudness, 1..127, null-default is 7.
     */
    public MelodyFactory(
            ToneSystem toneSystem,
            Integer numberOfBeatsPerBar,
            Integer beatType,
            Integer numberOfBeatsPerMinute,
            Integer volume)
    {
        this.toneSystem = (toneSystem != null) ? new Tones(toneSystem.tones()) : new Tones();
        this.numberOfBeatsPerBar = (numberOfBeatsPerBar != null) ? numberOfBeatsPerBar : 4;
        this.beatType = (beatType != null) ? beatType : 4;
        this.volume = (volume != null) ? volume : Note.DEFAULT_VOLUME;
        
        final int bpm = (numberOfBeatsPerMinute != null) ? numberOfBeatsPerMinute : Note.DEFAULT_TEMPO_BPM;
        this.beatDurationMilliseconds = (int) Math.round(1000.0 * 60.0 / (double) bpm);
        this.barDurationMilliseconds = (this.numberOfBeatsPerBar * beatDurationMilliseconds);
        this.barDurationMillisecondsHalf = (int) Math.round((double) barDurationMilliseconds / 2.0);
    }
    
    /**
     * Translates given string-inputs to a playable melody.
     * If a note has no "/" with subsequent length, it will be regarded as quarter-note.
     * @param notesWithLengths the sequence of IPN-notes with length to scan, e.g. ["C4/4", "D4/8", "B3/8" ...].
     * @return a sequence of <code>Note</code> Java-objects representing <code>notesWithLengths</code>.
     */
    public Note[] translate(String[] notesWithLengths) {
        final Note[] melody = new Note[notesWithLengths.length];
        final BarState barState = new BarState();
        for (int i = 0; i < notesWithLengths.length; i++)
            melody[i] = translate(notesWithLengths[i], barState);
        return melody;
    }

    private Note translate(String noteWithLength, BarState barState) {
        final int durationStartIndex = noteWithLength.indexOf(DURATION_SEPARATOR);
        
        final String ipnName = (durationStartIndex > 0)
                ? noteWithLength.substring(0, durationStartIndex)
                : noteWithLength; // has no length
        
        String noteLengthString = (durationStartIndex > 0)
                ? noteWithLength.substring(durationStartIndex + 1)
                : "4"; // assume quarter note
        
        // remove optional dot is at right end
        final boolean dotted = noteLengthString.endsWith(DOTTED_SYMBOL);
        if (dotted)
            noteLengthString = noteLengthString.substring(0, noteLengthString.length() - DOTTED_SYMBOL.length());
        
        // remove optional multiplet type left of dot
        final int multipletSeparatorIndex = noteLengthString.indexOf(MULTIPLET_SEPARATOR);
        final Integer multipletType = getMultipletFactor(noteLengthString, multipletSeparatorIndex);
        if (multipletType != null)
            noteLengthString = noteLengthString.substring(0, multipletSeparatorIndex);
        
        final int noteLength = Integer.valueOf(noteLengthString);
        final int durationMilliseconds = toMillis(noteLength, dotted, multipletType);
        
        final double volumeFactor = barState.getVolumeFactor();
        final int volumeInBar = (int) Math.round(volumeFactor * (double) volume);
        final boolean emphasized = (volumeFactor == BAR_START_VOLUME_FACTOR);
        
        // finally skip barState to next note
        barState.add(durationMilliseconds);

        return new Note(toneSystem, ipnName, durationMilliseconds, volumeInBar, emphasized);
    }

    private Integer getMultipletFactor(String noteLengthString, int multipletSeparatorIndex) {
        if (multipletSeparatorIndex <= 0)
            return null;
        
        final String multipletString = noteLengthString.substring(multipletSeparatorIndex + 1);
        if (multipletString.length() <= 0)
            throw new IllegalArgumentException("Illegal multiplet number in '"+noteLengthString+"'");
        
        return Integer.valueOf(multipletString);
    }

    private int toMillis(int noteLengthDivisor, boolean isDottedNote, Integer multipletType) {
        final double beatFactor = (double) beatType / (double) noteLengthDivisor;
        // "C4/1" in a 3/4 waltz must be written as "C4/2." (mind the dot)!
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
        case 2: // duplets take 3/2, 2 notes on the duration of 3 equal notes
            if (numberOfBeatsPerBar % 3 != 0)
                throw new IllegalArgumentException("Do not use duplets in "+numberOfBeatsPerBar+"/"+beatType+" measures!");
            factor = 3.0 / 2.0; break;
        case 3: // triplets take 2/3, 3 notes on the duration of 2 equal notes
        case 6: // sixtuplets take 4/6, 6 notes on the duration of 4 equal notes
            factor = 2.0 / 3.0; break;
        case 4: // quadruplets take 3/4, 4 notes on the duration of 3 equal notes
            if (numberOfBeatsPerBar % 3 != 0)
                throw new IllegalArgumentException("Do not use quadruplets in "+numberOfBeatsPerBar+"/"+beatType+" measures!");
            factor = 3.0 / 4.0; break;
        case 5: // quintuplets take 4/5, 5 notes on the duration of 4 equal notes
            factor = 4.0 / 5.0; break;
        default:
            throw new IllegalArgumentException("Unsupported multiplet type: "+multipletType);
        }
        return millis * factor;
    }


    /** Helper that tracks the millisecond position in bar. */
    private class BarState
    {
        private int currentMillis = 0;
        
        /** Adds a note to the current bar. */
        public void add(int durationMilliseconds) {
            currentMillis += durationMilliseconds;
            if (touches(currentMillis, barDurationMilliseconds))
                currentMillis = 0;
        }
        
        /** Notes at first beat should be louder. */
        public double getVolumeFactor() {
            if (currentMillis == 0)
                return BAR_START_VOLUME_FACTOR; // being at first beat
            
            if (numberOfBeatsPerBar % 2 == 0 && numberOfBeatsPerBar % 3 != 0 && // can be reliably divided into 2 parts
                    matches(currentMillis, barDurationMillisecondsHalf))
                return 1.4; // being in middle of the bar
            
            return 1.0;
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
}