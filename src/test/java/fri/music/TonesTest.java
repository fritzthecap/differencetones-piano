package fri.music;

import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.Test;

class TonesTest
{
    @Test
    void indexOf() {
        final Tones tones = new Tones(new EqualTemperament().tones());
        final String TEST_TONE = "C4";
        final int indexOfC4 = tones.indexOf(TEST_TONE);
        assertEquals(48, indexOfC4);
        assertEquals(TEST_TONE, tones.tones[indexOfC4].ipnName);
    }
    
    @Test
    void getEnclosingTones() {
        final Tones tones = new Tones(new EqualTemperament().tones());
        Tone[] enclosingTones;

        enclosingTones = tones.getEnclosingTones(ToneSystem.DEFAULT_REFERENCE_FREQUENCY);
        assertEquals("A4", enclosingTones[0].ipnName);
        assertEquals("A4", enclosingTones[1].ipnName);
        assertEquals(enclosingTones[0], enclosingTones[1]);

        enclosingTones = tones.getEnclosingTones(ToneSystem.DEFAULT_REFERENCE_FREQUENCY + 2.0);
        assertEquals("A4", enclosingTones[0].ipnName);
        assertEquals("A#4", enclosingTones[1].ipnName);

        enclosingTones = tones.getEnclosingTones(tones.getHighest().frequency - 3.0);
        assertEquals("B9", enclosingTones[0].ipnName);
        assertEquals("C10", enclosingTones[1].ipnName);

        enclosingTones = tones.getEnclosingTones(tones.getLowest().frequency + 0.1);
        assertEquals("C0", enclosingTones[0].ipnName);
        assertEquals("C#0", enclosingTones[1].ipnName);
    }
    
    @Test
    void getEnclosingTonesWhenFrequencyOutOfRange() {
        final Tones tones = new Tones(new EqualTemperament().tones());
        Tone[] enclosingTones;

        enclosingTones = tones.getEnclosingTones(tones.getHighest().frequency + 4.0);
        assertNull(enclosingTones[0]);
        assertEquals("C10", enclosingTones[1].ipnName);

        enclosingTones = tones.getEnclosingTones(tones.getLowest().frequency - 50.0);
        assertEquals("C0", enclosingTones[0].ipnName);
        assertNull(enclosingTones[1]);
    }
}