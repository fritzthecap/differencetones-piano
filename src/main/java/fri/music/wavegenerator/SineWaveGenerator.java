package fri.music.wavegenerator;

import javax.sound.sampled.AudioFormat;

/**
 * See super-class.
 * @author Fritz Ritzberger, Dec 2024
 */
public class SineWaveGenerator extends WaveGenerator
{
    private static final double TWO_PI = 2.0 * Math.PI;

    /** See super-class. */
    public SineWaveGenerator() {
        super();
    }
    
    /** See super-class. */
    public SineWaveGenerator(
            AudioFormat audioFormat, 
            int lineBufferSize, 
            int minimalDurationMilliseconds,
            int maximumSamplesToFade)
    {
        super(audioFormat, lineBufferSize, minimalDurationMilliseconds, maximumSamplesToFade);
    }

    @Override
    protected double createWaveValue(double samplesPerWave, int sampleIndex) {
        final double angle = TWO_PI * (double) sampleIndex / samplesPerWave;
        return Math.sin(angle);
    }
}