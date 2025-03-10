package fri.music.wavegenerator;

import javax.sound.sampled.AudioFormat;

/**
 * See super-class.
 * @author Fritz Ritzberger, Dec 2024
 */
public class SawtoothWaveGenerator extends WaveGenerator
{
    /** See super-class. */
    public SawtoothWaveGenerator() {
        super();
    }
    
    /** See super-class. */
    public SawtoothWaveGenerator(
            AudioFormat audioFormat, 
            int lineBufferSize, 
            int minimalDurationMilliseconds,
            int maximumSamplesToFade)
    {
        super(audioFormat, lineBufferSize, minimalDurationMilliseconds, maximumSamplesToFade);
    }

    @Override
    protected double createWaveValue(double samplesPerWave, int sampleIndex) {
        return (sampleIndex % samplesPerWave) / samplesPerWave - 1.0;
    }
}