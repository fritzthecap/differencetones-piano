package fri.music;

import static fri.music.ToneSystem.*;
import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.Test;

class ScaleTypesTest
{
    @Test
    void test() {
        assertEquals(7, ScaleTypes.numberOfWhiteKeysPerOctave());
        assertEquals(5, ScaleTypes.numberOfBlackKeysPerOctave());
        
        assertEquals(ScaleTypes.DORIAN, ScaleTypes.scaleName("D"));
        assertEquals(ScaleTypes.PHRYGIAN, ScaleTypes.scaleName("E"));
        assertEquals(ScaleTypes.LYDIAN, ScaleTypes.scaleName("F"));
        assertEquals(ScaleTypes.MIXOLYDIAN, ScaleTypes.scaleName("G"));
        assertEquals(ScaleTypes.AEOLIAN, ScaleTypes.scaleName("A"));
        assertEquals(ScaleTypes.LOCRIAN, ScaleTypes.scaleName("B"));
        
        assertEquals(semitoneSteps(FOURTH), ScaleTypes.cBasedSemitoneIndex("F3"));
        
        assertEquals("G2", ScaleTypes.ipnName("E", 2, semitoneSteps(MINOR_THIRD)));
        
        // the 4th (0-n) black key in a G-based scale is D#, it has 5 white keys below it
        assertEquals(5, ScaleTypes.numberOfWhiteKeysBelowBlackKey("G", (4-1)));
        
        // the 5th (0-n) black key in a C-based scale is A#, it is 10 semitone steps from C
        assertEquals(semitoneSteps(MINOR_SEVENTH), ScaleTypes.octaveBasedSemitoneIndex("C", (5-1), false));
    }
}