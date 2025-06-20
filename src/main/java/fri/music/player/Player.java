package fri.music.player;

import java.util.Objects;
import fri.music.SoundChannel;

/**
 * Primitive notes player. 
 * All methods wait for the note to finish playing.
 */
public class Player
{
    private final SoundChannel channel;
    
    public Player(SoundChannel waveSoundChannel) {
        this.channel = Objects.requireNonNull(waveSoundChannel);
    }

    /**
     * Plays given note.
     * @param note the tone to play, e.g. "C#4" for a c# in 4th octave.
     */
    public void play(Note note) {
        playSimultaneously(new Note[] { note });
    }
    
    /**
     * Plays given notes simultaneously for the duration of the longest note in array.
     * @param chord the array of notes to play.
     */
    public void playSimultaneously(Note[] chord) {
        long millisToWait = chord[0].durationMilliseconds;
        if (chord.length > 1)
            for (int i = 1; i < chord.length; i++)
                if (chord[i].durationMilliseconds > millisToWait)
                    millisToWait = chord[i].durationMilliseconds;
        
        for (Note note : chord)
            channel.noteOn(note.midiNumber, note.volume);

        try {
            Thread.sleep(millisToWait);
        }
        catch (InterruptedException e) {
            throw new RuntimeException(e);
        }

        for (Note note : chord)
            channel.noteOff(note.midiNumber);
    }
    
    /**
     * Plays given notes after each other.
     * @param notes the notes to play in a row.
     */
    public void playInRow(Note[] notes) {
        for (Note note : notes)
            play(note);
    }
    
    /**
     * Plays given intervals (or chords, or single notes) after each other.
     * @param notes the notes to play in a row.
     */
    public void playInRow(Note[][] notes) {
        for (Note[] chord : notes)
            playSimultaneously(chord);
    }
    
    /**
     * You MUST release system resources when not needed any more!
     */
    public void close() {
        channel.allNotesOff();
    }
}