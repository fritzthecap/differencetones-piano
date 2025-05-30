package fri.music.player;

import fri.music.ToneSystem;
import fri.music.Tones;

/**
 * Reads in a number of notes with length, headed by time-signature and tempo, 
 * and produces a melody (array of Note) from it, playable by a <code>Player</code>.
 * <p/>
 * <i>Input DSL:</i><br/>
 * A note is given as an IPN-name and its length behind a slash, 
 * e.g. "A4/4" for a quarter note on pitch of A4 (440 Hz),
 * or "C#4/8." for a dotted C#4 eighth note,
 * or "B3/8,3" for a B3 triplet eighth note.
 * In IPN there is no "Eb" or "Bb", you must give "D#" or "A#",
 * and there is no German "H", such is written as "B".
 */
public class MelodyFactory
{
    private static final double TRIPLET_DURATION_FACTOR = 2.0 / 3.0; // 3 notes in place of 2;
    
    private static final double BAR_START_VOLUME_FACTOR = 1.8; // first in bar should be louder
    
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
     * @param toneSystem optional, the tuning the notes should be built from.
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
        final int slashIndex = noteWithLength.indexOf('/');
        
        final String ipnName = (slashIndex > 0)
                ? noteWithLength.substring(0, slashIndex)
                : noteWithLength; // has no length
        
        String noteLengthString = (slashIndex > 0)
                ? noteWithLength.substring(slashIndex + 1)
                : "4"; // assume quarter note
        
        final String DOTTED_SYMBOL = ".";
        final boolean dotted = noteLengthString.endsWith(DOTTED_SYMBOL);
        if (dotted)
            noteLengthString = noteLengthString.substring(0, noteLengthString.length() - DOTTED_SYMBOL.length());
        
        final String TRIPLET_SYMBOL = ",3";
        final boolean triplet = noteLengthString.endsWith(TRIPLET_SYMBOL);
        if (triplet)
            noteLengthString = noteLengthString.substring(0, noteLengthString.length() - TRIPLET_SYMBOL.length());
        
        final int noteLength = Integer.valueOf(noteLengthString);
        final int durationMilliseconds = toMillis(noteLength, dotted, triplet);
        
        final double volumeFactor = barState.getVolumeFactor();
        final int volumeInBar = (int) Math.round(volumeFactor * (double) volume);
        final boolean emphasized = (volumeFactor == BAR_START_VOLUME_FACTOR);
        
        // finally skip barState to next note
        barState.add(durationMilliseconds);

        return new Note(toneSystem, ipnName, durationMilliseconds, volumeInBar, emphasized);
    }

    private int toMillis(int noteLengthDivisor, boolean isDottedNote, boolean isTripletNote) {
        final double beatFactor = (double) beatType / (double) noteLengthDivisor;
        // "C4/1" in a 3/4 waltz must be written as "C4/2." (mind the dot)!
        double millis = (double) beatDurationMilliseconds * beatFactor;
        
        if (isTripletNote)
            millis = millis * TRIPLET_DURATION_FACTOR;
        
        if (isDottedNote)
            millis = millis * 1.5; // dotted factor
        
        return (int) Math.round(millis);
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