package fri.music.wavegenerator;

import java.util.Iterator;
import java.util.Map;
import java.util.Objects;
import fri.music.Tone;

/**
 * Plays tones of the generator whose class was given in constructor.
 */
public class GenericWaveSoundChannel extends WaveSoundChannel
{
    private Class<? extends WaveGenerator> generatorClass;
    
    /** 
     * A wave generator that can switch wave forms on the fly.
     * @param toneSystem optional, null for EqualTemperament, 
     *      else the wanted tone stock for notes to play. 
     * @param generatorClass optional, null for SineWaveGenerator, 
     *      else the fully qualified class-name of the sound wave generator.
     */
    public GenericWaveSoundChannel(Tone[] toneSystem, Class<? extends WaveGenerator> generatorClass) {
        super(toneSystem);
        this.generatorClass = (generatorClass == null) ? SineWaveGenerator.class: generatorClass;
    }

    @Override
    protected WaveGenerator newWaveGenerator() {
        try {
            return generatorClass.getConstructor().newInstance();
        }
        catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
    
    /**
     * Sets a new type of wave-generator. 
     * All currently playing tones will continue on their new generator.
     * @param generatorClass required, the class of the new generator.
     */
    public void setGeneratorClass(Class<? extends WaveGenerator> generatorClass) {
        if (this.generatorClass == Objects.requireNonNull(generatorClass))
            return; // nothing to do
        
        cleanUp(); // remove all generators that are not playing
        
        this.generatorClass = generatorClass; // changes the way how generators are created
        
        final Iterator<Map.Entry<Integer,WaveGenerator>> iterator = generators().entrySet().iterator();
        while (iterator.hasNext()) {
            final Map.Entry<Integer,WaveGenerator> entry = iterator.next();
            final WaveGenerator oldGenerator = entry.getValue();
            
            final double oldFrequency = oldGenerator.getFrequency();
            final int oldAmplitude = oldGenerator.getAmplitude();
            oldGenerator.close();
            
            final WaveGenerator newGenerator = newWaveGenerator();
            entry.setValue(newGenerator);
            newGenerator.start(oldFrequency, oldAmplitude);
        }
    }
}