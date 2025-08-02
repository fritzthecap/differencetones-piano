package fri.music.player;

import java.util.Objects;
import fri.music.SoundChannel;
import fri.music.ToneSystem;

/**
 * Primitive synchronous notes player. 
 * All methods return only when the note finished playing.
 */
public class Player
{
    private final SoundChannel soundChannel;
    private boolean stopped; // = false by default
    
    public Player(SoundChannel soundChannel) {
        this.soundChannel = Objects.requireNonNull(soundChannel);
    }

    /**
     * Plays given note.
     * @param note the tone to play, e.g. "C#4" for a c# in 4th octave.
     */
    public /*synchronized*/ void play(Note note) {
        playSimultaneously(new Note[] { note });
    }
    
    /**
     * Plays given notes after each other.
     * @param notes the notes to play in a row.
     */
    public /*synchronized*/ void playInRow(Note[] notes) {
        for (Note note : notes)
            if (stopped)
                return;
            else
                play(note);
    }
    
    /**
     * Plays given intervals (or chords, or single notes) after each other.
     * @param notes the notes to play in a row.
     */
    public /*synchronized*/ void playInRow(Note[][] notes) {
        for (Note[] chord : notes)
            if (stopped)
                return;
            else
                playSimultaneously(chord);
    }
    
    /**
     * Plays given notes simultaneously for the duration of the 
     * longest note in array, which also could be a rest.
     * @param chord the array of notes to play.
     */
    public /*synchronized*/ void playSimultaneously(Note[] chord) {
        long millisToWait = chord[0].durationMilliseconds;
        if (chord.length > 1)
            for (int i = 1; i < chord.length; i++)
                if (chord[i].durationMilliseconds > millisToWait)
                    millisToWait = chord[i].durationMilliseconds;
        
        if (stopped || millisToWait <= 0) // zero could happen on tied notes
            return;
        
        turnNotesOnOrOff(chord, true);

        try {
            Thread.sleep(millisToWait);
        }
        catch (InterruptedException e) {
            throw new RuntimeException(e);
        }

        turnNotesOnOrOff(chord, false);
    }

    /** Stops any note playing. */
    public /*synchronized*/ void stop() {
        stopped = true;
    }
    
    /**
     * You MUST release system resources when not needed any more!
     * Stops any playing notes. The player is not usable any more after this call.
     */
    public /*synchronized*/ void close() {
        stop();
        soundChannel.allNotesOff();
    }


    private void turnNotesOnOrOff(Note[] chord, boolean on) {
        for (Note note : chord)
            if (note.ipnName.equals(ToneSystem.REST_SYMBOL) == false)
                if (on)
                    soundChannel.noteOn(note.midiNumber, note.volume);
                else
                    soundChannel.noteOff(note.midiNumber);
    }
}