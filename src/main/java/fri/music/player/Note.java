package fri.music.player;

import fri.music.Tone;
import fri.music.Tones;

/**
 * A tone with duration and volume.
 */
public class Note extends Tone
{
    /** Default loudness, 0-127. */
    public static final int DEFAULT_VOLUME = 7;
    /** Default duration of a quarter-note on 120 BPM, in milliseconds. */
    public static final int DEFAULT_DURATION = 500;
    
    /** The length of this note, in milliseconds. */
    public final int durationMilliseconds;
    /** The loudness of this note, 0-127. */
    public final int volume;
    
    /** CAUTION: this is a note from default tone-system EDO-12! */
    public Note(String ipnName, int durationInMilliseconds) {
        this(new Tones(), ipnName, durationInMilliseconds, -1);
    }
    
    /** A note from given tone-system with default duration. */
    public Note(Tones toneSystem, String ipnName) {
        this(toneSystem, ipnName, -1);
    }
    
    /** A note from given tone-system with given duration and default loudness. */
    public Note(Tones toneSystem, String ipnName, int durationMilliseconds) {
        this(toneSystem, ipnName, durationMilliseconds, -1);
    }
    
    /** A note from given tone-system. */
    public Note(Tones toneSystem, String ipnName, int durationMilliseconds, int volume) {
        this(toneSystem.forIpnName(ipnName), durationMilliseconds, volume);
    }
    
    /** A note from given tone with default duration. */
    public Note(Tone tone) {
        this(tone, -1);
    }
    
    /** A note from given tone with given duration. */
    public Note(Tone tone, int durationMilliseconds) {
        this(tone, durationMilliseconds, -1);
    }
    
    /** A note from given tone-system. */
    public Note(Tone tone, int durationMilliseconds, int volume) {
        super(
            tone.ipnName, 
            tone.frequency, 
            tone.midiNumber, 
            tone.cent);
        
        this.durationMilliseconds = (durationMilliseconds <= 0) ? DEFAULT_DURATION : durationMilliseconds;
        this.volume = (volume <= 0 || volume > 127) ? DEFAULT_VOLUME : volume;
    }
}