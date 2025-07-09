package fri.music.player;

import fri.music.Tone;
import fri.music.ToneSystem;
import fri.music.Tones;

/**
 * A note is a written tone.
 * It has a frequency from a tuning (tone-system), duration and volume.
 * If a note is the first in its bar, it will be emphasized,
 * i.e. its volume will be louder than all subsequent notes in same bar.
 */
public class Note extends Tone
{
    /** Default loudness, 0-127. */
    public static final int DEFAULT_VOLUME = 7;
    /** Default tempo is 120 BPM. */
    public static final int DEFAULT_TEMPO_BPM = 120;
    /** Default duration of a quarter-note on 120 BPM, in milliseconds. */
    public static final int DEFAULT_BEAT_DURATION = 1000 * 60 / DEFAULT_TEMPO_BPM; //500
    
    public static final char DURATION_SEPARATOR = '/';
    
    /** The length of this note, in milliseconds. */
    public final int durationMilliseconds;
    /** The loudness of this note, 0-127. */
    public final int volume;
    
    /** The first note in a bar would be emphasized, meaning it is louder than others. */
    public final boolean emphasized;
    
    /** The symbolic notation of the length, e.g. "4,3." */
    public final String lengthNotation;
    
    /**
     * Connection flag:
     * a note that is "phrased" together with predecessor or successor or both, having DIFFERENT pitches.
     * Null means not slurred, TRUE means slur start or continued, FALSE means slur end.
     */
    public final Boolean slurred;
    
    /**
     * Connection flag:
     * a note that is tied together with predecessor or successor or both, all having SAME pitch.
     * Null means not tied, TRUE means tie start or continued, FALSE means tie end.
     * If the tie is not started but continued, durationMilliseconds will be zero.
     */
    public final Boolean tied;
    
    /** A note from given tone-system with default duration (quarter note). */
    public Note(Tones toneSystem, String ipnName) {
        this(toneSystem, ipnName, -1);
    }
    /** A note from given tone-system with given duration and default loudness. */
    public Note(Tones toneSystem, String ipnName, int durationMilliseconds) {
        this(toneSystem, ipnName, durationMilliseconds, -1);
    }
    /** A note from given tone-system. */
    public Note(Tones toneSystem, String ipnName, int durationMilliseconds, int volume) {
        this(toneSystem, ipnName, durationMilliseconds, volume, false, null, null, null);
    }
    /** A note from given tone-system. */
    public Note(
            Tones toneSystem, 
            String ipnName, 
            int durationMilliseconds, 
            int volume, 
            boolean emphasized, 
            Boolean slurred, 
            Boolean tied, 
            String lengthNotation) {
        this(
            toneSystem.forIpnName(ipnName), 
            durationMilliseconds, 
            volume, 
            emphasized, 
            slurred, 
            tied,
            lengthNotation);
    }
    
    /** A note from given tone. */
    public Note(
            Tone tone, 
            int durationMilliseconds, 
            int volume, 
            boolean emphasized, 
            Boolean slurred, 
            Boolean tied, 
            String lengthNotation) {
        super(
            tone.ipnName, 
            tone.frequency, 
            tone.midiNumber, 
            tone.cent);
        
        this.durationMilliseconds = (durationMilliseconds < 0) ? DEFAULT_BEAT_DURATION : durationMilliseconds;
        this.volume = (volume <= 0 || volume > 127) ? DEFAULT_VOLUME : volume;
        this.emphasized = emphasized;
        this.slurred = slurred;
        this.tied = tied;
        this.lengthNotation = lengthNotation;
    }
    
    /** A clone of given tone with same duration and connection flags as given note. */
    public Note(Tone tone, Note note) {
        this(
            tone, 
            note.durationMilliseconds,
            note.volume,
            note.emphasized,
            note.slurred,
            note.tied,
            note.lengthNotation);
    }
    
    /** A clone with same IPN-name and frequency but different duration and connection flags. */
    public Note(Note note, int durationMilliseconds, Boolean slurred, Boolean tied, String lengthNotation) {
        super(
            note.ipnName, 
            note.frequency, 
            note.midiNumber, 
            note.cent);
        
        this.volume = note.volume;
        this.emphasized = note.emphasized;
        
        this.durationMilliseconds = durationMilliseconds;
        this.slurred = slurred;
        this.tied = tied;
        this.lengthNotation = lengthNotation;
    }
    
    /** A rest. */
    public Note(int durationMilliseconds, String lengthNotation) {
        super(ToneSystem.REST_SYMBOL, -1.0, -1, -1);
        
        this.durationMilliseconds = durationMilliseconds;
        this.lengthNotation = lengthNotation;
        this.volume = 0;
        this.emphasized = false;
        this.slurred = null;
        this.tied = null;
    }
    
    
    public boolean isRest() {
        return ToneSystem.REST_SYMBOL.equals(ipnName);
    }
    
    @Override
    public String toString() {
        return ipnName + DURATION_SEPARATOR + lengthNotation;
    }
}