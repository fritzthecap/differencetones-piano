package fri.music;

import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.Test;

class ToneSystemTest
{
    public static final double frequencyComparisonPrecision = 0.0000000000001;
    
    @Test
    void semitoneSteps() {
        for (int i = 0; i <= ToneSystem.SEMITONES_PER_OCTAVE; i++) {
            final String intervalName = ToneSystem.intervalName(i);
            final int semitoneSteps = ToneSystem.semitoneSteps(intervalName);
            assertEquals(i, semitoneSteps);
        }
    }

    @Test
    void intervalNames() {
        for (String intervalName : ToneSystem.INTERVAL_NAMES) {
            final int semitoneSteps = ToneSystem.semitoneSteps(intervalName);
            final String name = ToneSystem.intervalName(semitoneSteps);
            assertEquals(intervalName, name);
        }
    }

    @Test
    void equalTemperament() {
        final ToneSystem toneSystem = new EqualTemperament();
        assertDefaultToneSystem(toneSystem);
    }

    @Test
    void justIntonation() {
        final ToneSystem toneSystem = new JustIntonation();
        assertDefaultToneSystem(toneSystem);
    }

    private void assertDefaultToneSystem(final ToneSystem toneSystem) {
        assertEquals(ToneSystem.DEFAULT_BASETONE_IPN_NAME, toneSystem.baseToneIpnName());
        assertEquals(ToneSystem.DEFAULT_REFERENCE_FREQUENCY, toneSystem.referenceFrequency());
        
        final Tone[] tones = toneSystem.tones();
        
        final Tones toneManager = new Tones(tones);
        final Tone c4 = toneManager.forIpnName("C4");
        assertEquals(60, c4.midiNumber);
        final Tone a4 = toneManager.forIpnName("A4");
        assertEquals(69, a4.midiNumber);
        
        for (int i = 0, midiNumber = ToneSystem.DEFAULT_BASETONE_MIDI_NUMBER; i < tones.length; i++, midiNumber++) {
            final Tone tone = tones[i];
            assertEquals(midiNumber, tone.midiNumber);
        }
    }
}
