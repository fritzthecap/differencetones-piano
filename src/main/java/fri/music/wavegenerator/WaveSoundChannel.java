package fri.music.wavegenerator;

import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;
import fri.music.EqualTemperament;
import fri.music.SoundChannel;
import fri.music.Tone;
import fri.music.Tones;

/**
 * Base class for any wave sound channel.
 * Can play several tones simultaneously and continuously.
 * Can change the tone-system (tuning) dynamically.
 */
public abstract class WaveSoundChannel implements SoundChannel
{
    /** Maximum tones playing simultaneously (without LineUnavailableException). */
    private static final int MAXIMUM_SOUND_GENERATORS = 12; // saw LineUnavailableException when 33 WaveGenerators
    
    /** Turn this on to see whether difference-tone intervals play the difference-tone or not. They do not. */
    private static final boolean LOGGING = false;
    
    /** Key = MIDI note number, value = sound generator. */
    private final Map<Integer,WaveGenerator> generators = new Hashtable<>(MAXIMUM_SOUND_GENERATORS);
    /** Tone-system containing frequencies. */
    private Tones tones;
    
    public WaveSoundChannel(Tone[] tones) {
        this.tones = new Tones(tones == null ? new EqualTemperament().tones() : tones);
    }

    /** Factory method for sound wave generators. To be implemented by sub-classes. */
    protected abstract WaveGenerator newWaveGenerator();

    @Override
    public void noteOn(final int midiNoteNumber, final int amplitude) {
        final Map<Integer,WaveGenerator> generators = generators();

        WaveGenerator generator = generators.get(midiNoteNumber);
        if (generator != null && generator.isPlaying()) // already playing this tone
            generator.stop();
        
        if (generator == null && generators.size() >= MAXIMUM_SOUND_GENERATORS) {
            // if frequency is not present and having too many generators, try to reuse one
            generator = findAndRemoveNonPlayingGenerator();
            if (generator != null) // found an idle one
                generators.put(midiNoteNumber, generator);
        }
        
        if (generator == null) { // frequency is not present, no idle found
            generator = newWaveGenerator(); // create a new one
            // TODO: LineUnavailableException: line with format PCM_SIGNED 44100.0 Hz, 8 bit, mono, 1 bytes/frame not supported.
            generators.put(midiNoteNumber, generator);
        }
        
        final double frequency = midiNoteNumberToFrequency(midiNoteNumber);
        generator.start(frequency, amplitude);
        
        if (LOGGING)
            System.out.println("+ noteOn  "+midiNoteNumber);
        
        if (generators.size() > MAXIMUM_SOUND_GENERATORS) {
            cleanUp();
            
            if (generators.size() > MAXIMUM_SOUND_GENERATORS) // still overloaded
                throw new IllegalStateException("Too many tones are playing, maximum is "+MAXIMUM_SOUND_GENERATORS);
        }
    }

    @Override
    public void noteOff(int midiNoteNumber) {
        final WaveGenerator generator = findPlayingGenerator(midiNoteNumber);
        if (generator != null) { // stopGlissando may already have removed it
            generator.stop();
            
            if (LOGGING)
                System.out.println("- noteOff "+midiNoteNumber);
        }
    }
    
    @Override
    public void allNotesOff() {
        for (WaveGenerator generator : generators().values())
            generator.close();
        generators().clear();
    }
    
    @Override
    public void volumeChange(int volume) {
        for (WaveGenerator generator : generators().values())
            generator.setAmplitude(volume);
    }
    
    public Tone[] getTones() {
        return tones.tones;
    }
    
    /**
     * As a wave-generator allows to play every frequency, you can set a new tone-system here on the fly.
     * @param tones the new tone-system.
     */
    public void setTones(Tone[] tones) {
        cleanUp(); // remove all generators that are not playing
        
        this.tones = new Tones(tones); // changes frequency determination
        
        final Iterator<Map.Entry<Integer,WaveGenerator>> iterator = generators().entrySet().iterator();
        while (iterator.hasNext()) {
            final Map.Entry<Integer,WaveGenerator> entry = iterator.next();
            final WaveGenerator generator = entry.getValue(); // must be playing
            final Integer midiNoteNumber = entry.getKey();
            final double frequency = midiNoteNumberToFrequency(midiNoteNumber);
            generator.setFrequency(frequency);
        }
    }
    
    
    /** @return the map containing frequency to generator pairs. */
    protected final Map<Integer,WaveGenerator> generators() {
        return generators;
    }

    /** Closes and removes all non-playing generators. */
    protected final void cleanUp() {
        final Iterator<WaveGenerator> iterator = generators().values().iterator();
        while (iterator.hasNext()) {
            final WaveGenerator generator = iterator.next();
            if (generator.isPlaying() == false) {
                generator.close();
                iterator.remove();
            }
        }
    }

    private double midiNoteNumberToFrequency(int midiNoteNumber) {
        final Tone tone = tones.forMidiNoteNumber(midiNoteNumber);
        if (tone == null)
            throw new IllegalArgumentException("The midiNoteNumber "+midiNoteNumber+" could not be found in "+tones);
        return tone.frequency;
    }

    private WaveGenerator findPlayingGenerator(int midiNoteNumber) {
        WaveGenerator generator = generators().get(midiNoteNumber);
        return (generator != null && generator.isPlaying()) ? generator : null;
    }

    private WaveGenerator findAndRemoveNonPlayingGenerator() {
        final Iterator<Map.Entry<Integer,WaveGenerator>> iterator = generators().entrySet().iterator();
        while (iterator.hasNext()) {
            final Map.Entry<Integer,WaveGenerator> entry = iterator.next();
            final WaveGenerator generator = entry.getValue();
            if (generator.isPlaying() == false) {
                iterator.remove();
                return generator;
            }
        }
        return null;
    }
}