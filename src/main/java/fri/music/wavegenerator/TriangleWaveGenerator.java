package fri.music.wavegenerator;

import javax.sound.sampled.AudioFormat;

/**
 * See super-class.
 * @author Fritz Ritzberger, Jan 2025
 */
public class TriangleWaveGenerator extends SineWaveGenerator
{
    /** See super-class. */
    public TriangleWaveGenerator() {
        super();
    }
    
    /** See super-class. */
    public TriangleWaveGenerator(
            AudioFormat audioFormat, 
            int lineBufferSize, 
            int minimalDurationMilliseconds,
            int maximumSamplesToFade)
    {
        super(audioFormat, lineBufferSize, minimalDurationMilliseconds, maximumSamplesToFade);
    }

    @Override
    protected double createWaveValue(double samplesPerWave, int sampleIndex) {
        final double sineValue = super.createWaveValue(samplesPerWave, sampleIndex);
        return 2 / Math.PI * Math.asin(sineValue);
    }
}