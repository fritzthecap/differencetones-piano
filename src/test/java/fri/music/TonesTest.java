package fri.music;

import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.Test;

class TonesTest
{
    private final Tones tones = new Tones(new EqualTemperament().tones());
    
    @Test
    void indexOf() {
        final String TEST_TONE = "C4";
        final int indexOfC4 = tones.indexOf(TEST_TONE);
        assertEquals(48, indexOfC4);
        assertEquals(TEST_TONE, tones.tones[indexOfC4].ipnName);
    }
    
    @Test
    void upperCaseNamesSucceedLowerCaseNamesFail() {
        assertNotNull(tones.forIpnName("G5"));
        assertNull(tones.forIpnName("g5"));
    }
    
    @Test
    void getEnclosingTones() {
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
        Tone[] enclosingTones;

        enclosingTones = tones.getEnclosingTones(tones.getHighest().frequency + 4.0);
        assertNull(enclosingTones[0]);
        assertEquals("C10", enclosingTones[1].ipnName);

        enclosingTones = tones.getEnclosingTones(tones.getLowest().frequency - 50.0);
        assertEquals("C0", enclosingTones[0].ipnName);
        assertNull(enclosingTones[1]);
    }
}