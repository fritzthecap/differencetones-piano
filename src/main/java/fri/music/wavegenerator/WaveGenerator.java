package fri.music.wavegenerator;

import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.FloatControl;
import javax.sound.sampled.LineUnavailableException;
import javax.sound.sampled.SourceDataLine;
import fri.music.SoundChannel;
import fri.music.ToneSystem;

/**
 * Can play exactly one tone of a given frequency at a time.
 * Use multiple instances of SineWaveGenerator to play several tones simultaneously.
 * Durations from 20 milliseconds upwards will be audible.
 * A frequency of zero will result in a pause (sleep) of given duration.
 * <p/>
 * The <code>play()</code> method can not be stopped and returns when the tone was played.
 * The <code>start()</code> method returns immediately and plays eternally 
 * by starting a background-thread. That thread must be stopped by calling <code>stop()</code>. 
 * When having called <code>start()</code> without <code>stop()</code>, calls to 
 * <code>play()</code> and <code>start()</code> will throw an <code>IllegalStateException</code>.
 * 
 * @author Fritz Ritzberger, Dec 2024
 * @see https://quorumlanguage.com/tutorials/dsp/audiowavegenerator.html
 */
public abstract class WaveGenerator
{
    /** The default AudioFormat sample-rate. */
    public static final float SAMPLE_RATE = 44100f;
    /** The default minimal duration of an audible tone, in milliseconds. */
    public static final int MINIMAL_DURATION = 20;
    /** The default maximum number of sample bytes to fade-in and -out. */
    public static final int MAXIMUM_SAMPLES_TO_FADE = 1024;
    /** The maximum amplitude. Horrible sounds occur above 127! */
    public static final int MAXIMUM_AMPLITUDE = SoundChannel.MAXIMUM_VOLUME;
    
    /** State is used to control fade-in and -out. */
    private enum State
    {
        NONE, START, PLAY, END, START_END;
        
        State next(double frequency) {
            final boolean soundOff = (frequency <= 0.0);
            if (this == NONE)
                return soundOff ? NONE : START;
            if (this == START)
                return soundOff ? END : PLAY;
            if (this == PLAY)
                return soundOff ? END : PLAY;
            if (this == END)
                return soundOff ? NONE : START;
            // START_END has no next
            return NONE;
        }
    }
    
    /** Fader is used to avoid start/stop noise. */
    private static class Fader
    {
        private final State state;
        private final int headLimit;
        private final int tailLimit;
        private final double fadeStep;
        
        private double factor;
        
        Fader(State state, int samples, int maxSamplesToFade) {
            this.state = state;
            final int samplesToFade = Math.min(
                    (state == State.START_END) ? (samples / 3) : samples, 
                    maxSamplesToFade);
            this.fadeStep = 1.0 / samplesToFade;
            this.headLimit = samplesToFade;
            this.tailLimit = samples - samplesToFade;
            
            this.factor = (state == State.START || state == State.START_END) ? 0.0 : 1.0;
        }
        
        /** To be multiplied with amplitude. */
        double nextValue(int sampleIndex) {
            if ((state == State.START || state == State.START_END) && sampleIndex < headLimit) {
                factor = Math.min(factor + fadeStep, 1.0);
            }
            else if ((state == State.END || state == State.START_END) && sampleIndex >= tailLimit) {
                factor = Math.max(factor - fadeStep, 0.0);
            }
            return factor;
        }
    }

    private final SourceDataLine audioLine;
    private final float sampleRate; // "samples" (bytes) per second
    private final int minDuration; // milliseconds
    private final int maxSamplesToFade; // fade-in/out control
    
    private boolean playing;
    private Thread thread;
    private double frequency;
    private int amplitude;
    
    /** Opens and starts an AudioSystem SourceDataLine. */
    protected WaveGenerator() {
        this(
            new AudioFormat(SAMPLE_RATE, 8, 1, true, true),
            1024, // small buffer size to make audioLine react fast on frequency changes when playing continuously
            MINIMAL_DURATION, // milliseconds
            MAXIMUM_SAMPLES_TO_FADE
        );
    }
    
    /**
     * Opens and starts a SourceDataLine from given AudioSystem and configurations.
     * @param audioFormat the AudioFormat object to use.
     * @param lineBufferSize the byte-size for the line.open() call, normally the same as sampleRate, 
     *      but making it smaller (e.g. 1024) reduces the delays a frequency slider will have.
     * @param minimalDurationMilliseconds the minimal length of a tone, this will be the duration
     *      of the continuously playing repeated tone.
     * @param maximumSamplesToFade controls automatic amplitude fade-in and fade-out (avoids crack noise).
     */
    protected WaveGenerator(
            AudioFormat audioFormat, 
            int lineBufferSize, 
            int minimalDurationMilliseconds,
            int maximumSamplesToFade)
    {
        this.sampleRate = audioFormat.getSampleRate();
        this.minDuration = minimalDurationMilliseconds;
        this.maxSamplesToFade = maximumSamplesToFade;
        try {
            this.audioLine = AudioSystem.getSourceDataLine(audioFormat);
            audioLine.open(audioFormat, lineBufferSize);
            audioLine.start();
        }
        catch (LineUnavailableException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Plays given frequency at given amplitude for given duration. 
     * May return shortly before the tone has completed playing.
     * @param frequency the Hertz value for the tone to play.
     * @param durationMillis the duration of the tone, MINIMAL_DURATION - n.
     * @param amplitude the amplitude of the tone, 0 - MAX_AMPLITUDE.
     * @throws IllegalStateException when start() was called before, without stop().
     */
    public synchronized void play(double frequency, int durationMillis, int amplitude) {
        setPlayingAndEnsureJustOne(frequency, durationMillis, amplitude);
        try {
            durationMillis = Math.max(durationMillis, minDuration);
            writeToAudioLine(frequency, durationMillis, amplitude, State.START_END, false);
        }
        finally {
            this.playing = false;
        }
    }

    /**
     * Plays given frequency at given amplitude forever. Returns immediately,
     * you must call stop() to turn off the playing tone.
     * @param frequency the Hertz value for the tone to play.
     * @param amplitude the amplitude (loudness) of the tone, 0 - MAX_AMPLITUDE.
     * @throws IllegalStateException when start() without stop() was called before.
     */
    public synchronized void start(double frequency, int amplitude) {
        setPlayingAndEnsureJustOne(frequency, minDuration, amplitude);
        thread = new Thread(new Runnable() {
            @Override
            public void run() {
                int amplitude = getAmplitude();
                double frequency = getFrequency();
                State state = State.NONE.next(frequency);
                while (isPlaying()) {
                    writeToAudioLine(frequency, minDuration, amplitude, state, true);
                    amplitude = getAmplitude();
                    frequency = getFrequency(); // was possibly changed meanwhile
                    state = state.next(frequency);
                }
                writeToAudioLine(frequency, minDuration, amplitude, State.END, true); // fade-out
            }
        });
        thread.start();
    }
    
    /** @return true when playing continuously, else false. */
    public synchronized boolean isPlaying() {
        return playing;
    }

    /** Call this to stop continuous playing. */
    public void stop() {
        synchronized(this) {
            playing = false;
        }
        if (thread != null) {
            try {
                thread.join(); // wait for thread termination
                thread = null;
            }
            catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
        }
    }
    
    /** @return the most recently set frequency. */
    public synchronized double getFrequency() {
        return frequency;
    }
    /** @param frequency sets a new frequency in case a tone is currently playing in background. */
    public synchronized void setFrequency(double frequency) {
        this.frequency = frequency;
    }

    /** @return the most recently set amplitude. */
    public synchronized int getAmplitude() {
        return amplitude;
    }
    /** @param amplitude sets a new amplitude in case a tone is currently playing in background. */
    public synchronized void setAmplitude(int amplitude) {
        this.amplitude = amplitude;
    }

    public float getMinimumGain() {
        return getGainControl().getMinimum();
    }
    public float getMaximumGain() {
        return getGainControl().getMaximum();
    }
    public float getGain() {
        return getGainControl().getValue();
    }
    public void setGain(float gain) {
        getGainControl().setValue(gain);
    }
    private FloatControl getGainControl() {
        return (FloatControl) audioLine.getControl(FloatControl.Type.MASTER_GAIN);
    }

    /** Releases all resources. This generator can not be used any more afterwards. */
    public void close() {
        stop();
        //audioLine.drain(); // very slow and unnecessary, could also block
        audioLine.stop();
        audioLine.close();
    }

    
    private void assertionsOnPlayStart(double frequency, int durationMillis, int amplitude) {
        if (durationMillis < minDuration)
            throw new IllegalArgumentException("Duration is too small, would cause crackling: "+durationMillis);
        if (amplitude < 0 || amplitude > MAXIMUM_AMPLITUDE) // horrible sounds occur above 127
            throw new IllegalArgumentException("Amplitude must be 0-"+MAXIMUM_AMPLITUDE+": "+amplitude);
        if (frequency < ToneSystem.MINIMUM_FREQUENCY || frequency > ToneSystem.MAXIMUM_FREQUENCY) // humans can not hear this any more
            throw new IllegalArgumentException(
                    "Frequency must be between "+ToneSystem.MINIMUM_FREQUENCY+" and "+ToneSystem.MAXIMUM_FREQUENCY+", but is "+frequency);
        
        if (isPlaying())
            throw new IllegalStateException(getClass().getSimpleName()+" is playing continuously, call stop()!");
    }

    private void setPlayingAndEnsureJustOne(double frequency, int durationInMilliseconds, int amplitude) {
        assertionsOnPlayStart(frequency, durationInMilliseconds, amplitude);        
        
        this.playing = true;
        setFrequency(frequency);
        setAmplitude(amplitude);
    }

    private void writeToAudioLine(double frequency, int durationMillis, int amplitude, State state, boolean continuous) {
        if (frequency <= 0.0) { // inaudible tone: rest
            try { Thread.sleep(durationMillis); } catch (InterruptedException e) {} 
        }
        else {
            final byte[] toneBuffer = createWaveBuffer(frequency, durationMillis, amplitude, state, continuous);
            audioLine.write(toneBuffer, 0, toneBuffer.length);
        }
    }

    private byte[] createWaveBuffer(double frequency, int durationMillis, int amplitude, State state, boolean continuous) {
        final double waveDuration = 1.0 / frequency; // seconds per wave
        final double samplesPerWave = sampleRate * waveDuration; // samples per second * seconds per wave
        
        final int samples;
        if (continuous) // must be complete waves, not duration-accurate
            samples = (int) Math.round(samplesPerWave * (double) durationMillis) / 2; // 2: empirical time correction
        else // must be duration-accurate
            samples = (int) Math.round(sampleRate * (double) durationMillis / 1000.0);
        
        final Fader fader = new Fader(state, samples, maxSamplesToFade);
        final byte[] buffer = new byte[samples]; // allocate wave form description buffer
        
        for (int i = 0; i < samples; i++) { // fill wave form description buffer
            final double waveValue = createWaveValue(samplesPerWave, i);
            buffer[i] = (byte) (waveValue * (double) amplitude * fader.nextValue(i)); // loudness control
        }
        return buffer;
    }
    
    /**
     * Generates one sample for a specific wave form.
     * @param samplesPerWave the (sampleRate / frequency) value.
     * @param sampleIndex the index of the sample to generate.
     * @return the wave-value at given index.
     */
    protected abstract double createWaveValue(double samplesPerWave, int sampleIndex);

}