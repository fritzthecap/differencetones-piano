package fri.music.wavegenerator;

import javax.sound.sampled.AudioFormat;

/**
 * See super-class.
 * @author Fritz Ritzberger, Dec 2024
 */
public class SquareWaveGenerator extends SineWaveGenerator
{
    /** See super-class. */
    public SquareWaveGenerator() {
        super();
    }
    
    /** See super-class. */
    public SquareWaveGenerator(
            AudioFormat audioFormat, 
            int lineBufferSize, 
            int minimalDurationMilliseconds,
            int maximumSamplesToFade)
    {
        super(audioFormat, lineBufferSize, minimalDurationMilliseconds, maximumSamplesToFade);
    }

    @Override
    protected double createWaveValue(double samplesPerWave, int sampleIndex) {
        final double value = super.createWaveValue(samplesPerWave, sampleIndex);
        return Math.signum(value) / 2.0;
    }
}