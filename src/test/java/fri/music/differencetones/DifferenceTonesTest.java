package fri.music.differencetones;

import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.Test;
import fri.music.EqualTemperament;
import fri.music.JustIntonation;
import fri.music.JustIntonation.ChromaticScales;
import fri.music.Tone;
import fri.music.ToneSystem;

class DifferenceTonesTest
{
    @Test
    void intervals() {
        Tone[] diff;
        
        {
            final DifferenceTones toneDifferencesEdo12 = 
                    new DifferenceTones(new EqualTemperament().tones(), 0.37);
            
            diff = toneDifferencesEdo12.findDifferenceTones("C4", "C#4");
            assertNull(diff[0]);
            diff = toneDifferencesEdo12.findDifferenceTones("C4", "D4");
            assertEquals("C1", diff[0].ipnName);
            diff = toneDifferencesEdo12.findDifferenceTones("C4", "D#4");
            assertEquals("G1", diff[0].ipnName);
            diff = toneDifferencesEdo12.findDifferenceTones("C4", "E4");
            assertEquals("C#2", diff[0].ipnName);
            diff = toneDifferencesEdo12.findDifferenceTones("C4", "F4");
            assertEquals("F2", diff[0].ipnName);
            diff = toneDifferencesEdo12.findDifferenceTones("C4", "F#4");
            assertEquals("A2", diff[0].ipnName);
            diff = toneDifferencesEdo12.findDifferenceTones("C4", "G4");
            assertEquals("C3", diff[0].ipnName);
            diff = toneDifferencesEdo12.findDifferenceTones("C4", "G#4");
            assertEquals("D#3", diff[0].ipnName);
            diff = toneDifferencesEdo12.findDifferenceTones("C4", "A4");
            assertEquals("F3", diff[0].ipnName);
            diff = toneDifferencesEdo12.findDifferenceTones("C4", "A#4");
            assertEquals("G#3", diff[0].ipnName);
            diff = toneDifferencesEdo12.findDifferenceTones("C4", "B4");
            assertEquals("A#3", diff[0].ipnName);
            diff = toneDifferencesEdo12.findDifferenceTones("C4", "C5");
            assertEquals("C4", diff[0].ipnName);
        }
        {
            final DifferenceTones toneDifferencesJust = 
                    new DifferenceTones(new JustIntonation().tones(), 0.1);
            
            diff = toneDifferencesJust.findDifferenceTones("C4", "C#4");
            assertEquals("C#0", diff[0].ipnName);
            diff = toneDifferencesJust.findDifferenceTones("C4", "D4");
            assertEquals("C1", diff[0].ipnName);
            diff = toneDifferencesJust.findDifferenceTones("C4", "D#4");
            assertEquals("G#1", diff[0].ipnName); // would fail with deviation 0.0
            diff = toneDifferencesJust.findDifferenceTones("C4", "E4");
            assertEquals("C2", diff[0].ipnName);
            diff = toneDifferencesJust.findDifferenceTones("C4", "F4");
            assertEquals("F2", diff[0].ipnName);
            diff = toneDifferencesJust.findDifferenceTones("C4", "F#4");
            assertNull(diff[0]);
            diff = toneDifferencesJust.findDifferenceTones("C4", "G4");
            assertEquals("C3", diff[0].ipnName);
            diff = toneDifferencesJust.findDifferenceTones("C4", "G#4");
            assertEquals("D#3", diff[0].ipnName);
            diff = toneDifferencesJust.findDifferenceTones("C4", "A4");
            assertEquals("F3", diff[0].ipnName);
            diff = toneDifferencesJust.findDifferenceTones("C4", "A#4");
            assertNull(diff[0]);
            diff = toneDifferencesJust.findDifferenceTones("C4", "B4");
            assertNull(diff[0]); // would succeed with deviation 0.3333
            diff = toneDifferencesJust.findDifferenceTones("C4", "C5");
            assertEquals("C4", diff[0].ipnName);
        }
    }
    
    @Test
    void outOfRange() {
        final ToneSystem toneSystem = new EqualTemperament("A2", 4);
        final DifferenceTones toneDifferences = new DifferenceTones(toneSystem.tones());
        
        assertThrows(NullPointerException.class, () -> toneDifferences.findDifferenceTones("C9", "D9"));
    }
    
    @Test
    void equalTemperamentDifferenceTones() {
        final ToneSystem toneSystem = new EqualTemperament("A2", 4);
        final double deviationTolerance = 0.37; // needed in EDO-12 to match "C3" from C6 with D6
        final DifferenceTones toneDifferences = new DifferenceTones(toneSystem.tones(), deviationTolerance);
        
        final Tone[] differenceTonesCD = toneDifferences.findDifferenceTones("C6", "D6");
        assertEquals("C3", differenceTonesCD[0].ipnName);
    }
    
    @Test
    void justIntonationDifferenceToneIsLowestInScale() {
        final ToneSystem toneSystem = new JustIntonation("C3", 4, ChromaticScales.LIMIT_5_SYMMETRIC_1);
        final double deviationTolerance = 0.0; // just intonation is very just
        final DifferenceTones toneDifferences = new DifferenceTones(toneSystem.tones(), deviationTolerance);
        
        final Tone[] differenceTonesCD = toneDifferences.findDifferenceTones("C6", "D6");
        assertEquals("C3", differenceTonesCD[0].ipnName);
    }
    
    @Test
    void differenceTonesForMajorSecond() {
        final ToneSystem toneSystem = new EqualTemperament("A2", 4);
        final DifferenceTones toneDifferences = new DifferenceTones(toneSystem.tones(), DifferenceTones.TOLERANT_DEVIATION_EDO_12);
        
        final Tone[] differenceTonesCE = toneDifferences.findDifferenceTones("C6", "E6");
        assertEquals("C#4", differenceTonesCE[0].ipnName);

        final ToneSystem justToneSystem = new JustIntonation("C3", 4, ChromaticScales.LIMIT_5_SYMMETRIC_1);
        final DifferenceTones justToneDifferences = new DifferenceTones(justToneSystem.tones());
        
        final Tone[] justDifferenceTonesCE = justToneDifferences.findDifferenceTones("C6", "E6");
        assertEquals("C4", justDifferenceTonesCE[0].ipnName);
    }
    
    @Test
    void wrongNotesOrder() {
        final ToneSystem toneSystem = new EqualTemperament();
        final DifferenceTones toneDifferences = new DifferenceTones(toneSystem.tones());
        
        final Tone[] differenceTonesCG = toneDifferences.findDifferenceTones("G6", "C6"); // wrong order
        assertEquals("C5", differenceTonesCG[0].ipnName); // succeeds with auto-swap
    }
    
    @Test
    void intervalTooBig() {
        final ToneSystem toneSystem = new EqualTemperament();
        final DifferenceTones toneDifferences = new DifferenceTones(toneSystem.tones());
        
        final Tone[] differenceTonesC6D7 = toneDifferences.findDifferenceTones("C6", "D7"); // ninth is too big
        assertNull(differenceTonesC6D7[0]); // WARNING was displayed on stderr
        
        final Tone[] differenceTonesC6C7 = toneDifferences.findDifferenceTones("C6", "C7"); // octave
        assertNotNull(differenceTonesC6C7[0]); /// octave must be accepted
        assertEquals("C6", differenceTonesC6C7[0].ipnName);
    }
}