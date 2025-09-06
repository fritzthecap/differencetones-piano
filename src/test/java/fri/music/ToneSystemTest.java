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
    void ipnNameToMidiNumber() {
        assertEquals(
                ToneSystem.DEFAULT_BASETONE_MIDI_NUMBER, 
                ToneSystem.ipnNameToMidiNumber(ToneSystem.DEFAULT_BASETONE_IPN_NAME));
        assertEquals(132, ToneSystem.ipnNameToMidiNumber("C10"));
        assertEquals(60, ToneSystem.ipnNameToMidiNumber("C4"));
        assertEquals(61, ToneSystem.ipnNameToMidiNumber("C#4"));
        assertEquals(62, ToneSystem.ipnNameToMidiNumber("D4"));
        assertEquals(63, ToneSystem.ipnNameToMidiNumber("D#4"));
        assertEquals(69, ToneSystem.ipnNameToMidiNumber("A4"));
    }

    @Test
    void midiNumberToIpnName() {
        assertEquals(
                ToneSystem.DEFAULT_BASETONE_IPN_NAME,
                ToneSystem.midiNumberToIpnName(ToneSystem.DEFAULT_BASETONE_MIDI_NUMBER));
        assertEquals("C10", ToneSystem.midiNumberToIpnName(132));
        assertEquals("C4", ToneSystem.midiNumberToIpnName(60));
        assertEquals("C#4", ToneSystem.midiNumberToIpnName(61));
        assertEquals("D4", ToneSystem.midiNumberToIpnName(62));
        assertEquals("D#4", ToneSystem.midiNumberToIpnName(63));
        assertEquals("A4", ToneSystem.midiNumberToIpnName(69));
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
