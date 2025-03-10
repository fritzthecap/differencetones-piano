package fri.music;

import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.Test;

class EqualTemperamentTest
{
    @Test
    void numberOfOctaves() {
        AbstractToneSystem toneSystem;
        
        toneSystem = new EqualTemperament();
        assertEquals(ToneSystem.MAXIMUM_OCTAVES, toneSystem.octaves);
        
        int octaves;
        toneSystem = new EqualTemperament(octaves = 4);
        assertEquals(octaves, toneSystem.octaves);
        
        toneSystem = new EqualTemperament(0);
        assertEquals(1, toneSystem.tones().length);
        
        toneSystem = new EqualTemperament("A7", octaves = 2); // 2 octaves is maximum for A7
        assertEquals(octaves, toneSystem.octaves);
        
        assertThrows(IllegalArgumentException.class, () -> {
            new EqualTemperament(ToneSystem.MAXIMUM_OCTAVES + 1).tones(); // just 12 octaves possible
        });
        
        // challenge octave limit
        final int startOctave = 7;
        final int requestedOctaves = ToneSystem.MAXIMUM_OCTAVES - startOctave;
        toneSystem = new EqualTemperament("C"+startOctave, requestedOctaves);
        assertEquals(requestedOctaves, toneSystem.octaves);
        
        assertThrows(IllegalArgumentException.class, () -> {
            new EqualTemperament("C#7", requestedOctaves).tones(); // just 12 octaves possible
        });
    }

    @Test
    void frequencies() {
        final double frequencyOfA4 = 436.0;
        final AbstractToneSystem toneSystem = new EqualTemperament(frequencyOfA4, "C4", 1);
        assertEquals(1, toneSystem.octaves);
        
        final Tone[] tones = toneSystem.tones();
        assertEquals(12 + 1, tones.length);
        
        assertTone(tones[0], "C4",  60, 259.2472);
        assertTone(tones[1], "C#4", 61, 274.6628);
        assertTone(tones[2], "D4",  62, 290.9951);
        assertTone(tones[3], "D#4", 63, 308.2986);
        assertTone(tones[4], "E4",  64, 326.6309);
        assertTone(tones[5], "F4",  65, 346.0534);
        assertTone(tones[6], "F#4", 66, 366.6308);
        assertTone(tones[7], "G4",  67, 388.4318);
        assertTone(tones[8], "G#4", 68, 411.5292);
        assertTone(tones[9], "A4",  69, 436.0000);
        assertTone(tones[10],"A#4", 70, 461.9259);
        assertTone(tones[11],"B4",  71, 489.3935);
        assertTone(tones[12],"C5",  72, 518.4943);
    }


    private void assertTone(Tone tone, String exectedIpnName, int expectedMidiNumber, double expectedFrequency) {
        assertEquals(exectedIpnName, tone.ipnName);
        assertEquals(expectedMidiNumber, tone.midiNumber);
        assertEquals(expectedFrequency, tone.frequency, 0.0001);
    }
}