package fri.music.wavegenerator;

import fri.music.Tone;

/**
 * Plays sine tones.
 */
public class SineWaveSoundChannel extends WaveSoundChannel
{
    public SineWaveSoundChannel(Tone[] toneSystem) {
        super(toneSystem);
    }

    @Override
    protected WaveGenerator newWaveGenerator() {
        return new SineWaveGenerator();
    }
}