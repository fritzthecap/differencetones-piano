package fri.music.player;

import java.util.Objects;
import fri.music.SoundChannel;

public class Player
{
    private final SoundChannel channel;
    
    public Player(SoundChannel waveSoundChannel) {
        this.channel = Objects.requireNonNull(waveSoundChannel);
    }

    /**
     * Plays given note.
     * @param note            the tone to play, e.g. "C#4" for a c# in 4th octave
     * @param durationMillis  the duration of the note in milliseconds
     * @param volume          the 0-127 loudness
     */
    public void play(Note note) {
        playSimultaneously(new Note[] { note });
    }
    
    /**
     * Plays given notes simultaneously.
     * @param chord           the tones to play, e.g. "C#4" for a c# in 4th octave
     * @param durationMillis  the duration of the note in milliseconds
     * @param volume          the 0-127 loudness
     */
    public void playSimultaneously(Note[] chord) {
        for (Note note : chord)
            channel.noteOn(note.midiNumber, note.volume);

        try {
            Thread.sleep(chord[0].durationMilliseconds); // TODO: enable different durations?
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