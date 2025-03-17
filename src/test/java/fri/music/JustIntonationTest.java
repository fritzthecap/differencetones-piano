package fri.music;

import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.Test;
import fri.music.JustIntonation.ChromaticScales;
import fri.music.JustIntonation.JustTone;

class JustIntonationTest
{
    @Test
    void constructorParameters() {
        final double frequencyOfA4 = 436.0;
        final int numberOfOctaves = 3;
        final int startOctave = 2;
        final String modalStartNote = "A";
        final String modalStartIpnName = modalStartNote+startOctave;
        final JustIntonation toneSystem = new JustIntonation(
                frequencyOfA4, // Hertz of A4
                "C"+(startOctave + 1), // scale build note
                modalStartIpnName, // lowest scale note
                numberOfOctaves,
                ChromaticScales.LIMIT_5_SYMMETRIC_1); // the parallel minor
        
        final Tone[] tones = toneSystem.tones();
        assertEquals(numberOfOctaves, toneSystem.octaves);
        assertEquals(modalStartIpnName, tones[0].ipnName);
        assertEquals(startOctave, tones[0].ipnOctave);
        assertEquals(startOctave + 1, tones[12].ipnOctave);
        assertTrue(tones[tones.length - 1].ipnName.startsWith(modalStartNote));
        
        final Tones tonesFinder = new Tones(tones);
        final Tone a4 = tonesFinder.forIpnName(ToneSystem.REFERENCE_FREQUENCY_IPN_NAME);
        assertEquals(ToneSystem.REFERENCE_FREQUENCY_IPN_NAME, a4.ipnName);
        assertEquals(frequencyOfA4, a4.frequency, ToneSystemTest.frequencyComparisonPrecision);
    }

    @Test
    void numberOfOctaves() {
        AbstractToneSystem toneSystem;
        
        toneSystem = new JustIntonation();
        assertEquals(ToneSystem.MAXIMUM_OCTAVES, toneSystem.octaves);
        
        final int OCTAVES = 4;
        toneSystem = new JustIntonation(OCTAVES);
        assertEquals(OCTAVES, toneSystem.octaves);
        
        toneSystem = new JustIntonation(0);
        assertEquals(1, toneSystem.tones().length);
        
        assertThrows(IllegalArgumentException.class, () -> {
            new JustIntonation(ToneSystem.MAXIMUM_OCTAVES + 1).tones(); // just 12 octaves possible
        });
    }

    @Test
    void referenceFrequencyOfA4() {
        AbstractToneSystem toneSystem;
        Tone[] tones;
        double frequencyOfA4;
        String scaleStartName;
        
        frequencyOfA4 = 435.0;
        scaleStartName = "G4";
        toneSystem = new JustIntonation(frequencyOfA4, "C#4", scaleStartName, 2, null);
        assertEquals(frequencyOfA4, toneSystem.referenceFrequency());
        tones = toneSystem.tones();
        assertEquals("G4", tones[0].ipnName);
        final Tone a4 = new Tones(tones).forIpnName(ToneSystem.REFERENCE_FREQUENCY_IPN_NAME);
        assertEquals(frequencyOfA4, a4.frequency, ToneSystemTest.frequencyComparisonPrecision);
        
        frequencyOfA4 = 442.0;
        scaleStartName = "B4";
        toneSystem = new JustIntonation(frequencyOfA4, "A#4", scaleStartName, 1, ChromaticScales.LIMIT_5_ASYMMETRIC);
        tones = toneSystem.tones();
        assertEquals("B4", tones[0].ipnName);
        final Tone a5 = new Tones(tones).forIpnName("A5"); // one octave higher
        assertEquals(frequencyOfA4, toneSystem.referenceFrequency());
        final double expectedFrequenceOfA5 = frequencyOfA4 * 2;
        assertEquals(expectedFrequenceOfA5, a5.frequency, ToneSystemTest.frequencyComparisonPrecision);
    }
    
    @Test
    void frequencyRange() {
        final double expectedC0Frequency = 16.5;
        final double expectedC10Frequency = 16896.0;
        
        final ToneSystem toneSystem = new JustIntonation();
        final Tone[] tones = toneSystem.tones();
        assertEquals(expectedC0Frequency, tones[0].frequency);
        assertEquals(expectedC10Frequency, tones[tones.length - 1].frequency);
    }

    @Test
    void limit5CMajorToneDistances() {
        final ToneSystem toneSystem = new JustIntonation(
                "C4", // scale build note AND lowest modal scale start
                1, // octaves
                ChromaticScales.LIMIT_5_SYMMETRIC_1); // the major scale
        final Tone[] tones = toneSystem.tones();
        assertEquals(12 + 1, tones.length); // one octave plus 1 topmost note
        
        final JustTone c4 = getJustTone(tones,  0, "C4");
        final JustTone d4 = getJustTone(tones,  2, "D4");
        final JustTone e4 = getJustTone(tones,  4, "E4");
        final JustTone f4 = getJustTone(tones,  5, "F4");
        final JustTone g4 = getJustTone(tones,  7, "G4");
        final JustTone a4 = getJustTone(tones,  9, "A4");
        final JustTone b4 = getJustTone(tones, 11, "B4");
        final JustTone c5 = getJustTone(tones, 12, "C5");
        
        // See https://en.wikipedia.org/wiki/Five-limit_tuning#Diatonic_scale
        assertDistance(c4, d4,  9, 8);
        assertDistance(d4, e4, 10, 9);
        assertDistance(e4, f4, 16, 15);
        assertDistance(f4, g4,  9, 8);
        assertDistance(g4, a4, 10, 9);
        assertDistance(a4, b4,  9, 8);
        assertDistance(b4, c5, 16, 15);
    }
    
    @Test
    void limit5IonianMajorFractions() {
        final int octavePlusFromC1 = 2 * 12; // C3 must be 24th tone from C1
        limit5_C3_D3_E3_fractions(
                "C1", // modal scale start note for IONIAN
                ChromaticScales.LIMIT_5_SYMMETRIC_1, // the parallel minor
                new int[] { octavePlusFromC1, octavePlusFromC1 + 2, octavePlusFromC1 + 4},
                new int[][] { 
                    { 9, 8 }, // distance must be 9/8 for C3 - D3 and  10/9 for D3 - E3
                    { 10, 9 } // See https://en.wikipedia.org/wiki/Five-limit_tuning#Diatonic_scale
                });
    }
    
    /** The parallel minor to C (Am) has switched fractions between third, fourth and fifth. */
    @Test
    void limit5AeolianMinorFractions() {
        final int octavePlusFromA2 = 3; // C3 must be 3rd tone from A2
        limit5_C3_D3_E3_fractions(
                "A2", // modal scale start note for AEOLIAN
                ChromaticScales.LIMIT_5_SYMMETRIC_2, // the parallel minor
                new int[] { octavePlusFromA2, octavePlusFromA2 + 2, octavePlusFromA2 + 4},
                new int[][] { 
                    { 10, 9 }, // distance must be 10/9 for C3 - D3 and  9/8 for D3 - E3
                    { 9, 8 } // See https://en.wikipedia.org/wiki/Five-limit_tuning#Diatonic_scale
                });
    }
    
    
    private void limit5_C3_D3_E3_fractions(
            String modalScaleStartNote, 
            ChromaticScales chromaticScale,
            int[] indexesOf_C3_D3_E3,
            int[][] expectedDividendAndDivisors)
    {
        final ToneSystem toneSystem = new JustIntonation(
                "C7", // scale build note, will be reduced to lowest possible octave
                modalScaleStartNote,
                chromaticScale);
        
        final Tone[] tones = toneSystem.tones();
        assertEquals(modalScaleStartNote, tones[0].ipnName);
        
        final JustTone c3 = getJustTone(tones, indexesOf_C3_D3_E3[0], "C3");
        final JustTone d3 = getJustTone(tones, indexesOf_C3_D3_E3[1], "D3");
        final JustTone e3 = getJustTone(tones, indexesOf_C3_D3_E3[2], "E3");
        
        assertDistance(c3, d3, expectedDividendAndDivisors[0][0], expectedDividendAndDivisors[0][1]);
        assertDistance(d3, e3, expectedDividendAndDivisors[1][0], expectedDividendAndDivisors[1][1]);
    }
    
    private JustTone getJustTone(final Tone[] tones, int index, String expectedIpnName) {
        final JustTone tone = (JustTone) tones[index];
        assertEquals(expectedIpnName, tone.ipnName);
        return tone;
    }

    private void assertDistance(JustTone lowerTone, JustTone upperTone, int expectedDividend, int expectedDivisor) {
        final long[] distance = lowerTone.distance(upperTone);
        assertEquals(expectedDividend, distance[0]);
        assertEquals(expectedDivisor,  distance[1]);
    }
}