package fri.music;

/**
 * Responsibilities of classes that want to play tones for piano key presses.
 * A piano is expected to have 12 keys per octave.
 */
public interface SoundChannel
{
    /** 
     * Start to play given tone.
     * @param midiNoteNumber the number of the MIDI note to play.
     * @param velocity MIDI strike force, 0-127.
     */
    void noteOn(int midiNoteNumber, int velocity);

    /**
     * Stop to play given tone.
     * @param midiNoteNumber the number of the MIDI note to play.
     */
    void noteOff(int midiNoteNumber);

    /**
     * Loudness change.
     * @param volume the new 0-127 value.
     */
    void volumeChange(int volume);

    /**
     * To be called when closing the piano. Releases all resources. 
     * This is like close(), but the channel is still usable afterwards.
     */
    void allNotesOff();
}