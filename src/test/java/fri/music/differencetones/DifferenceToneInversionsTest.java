package fri.music.differencetones;

import static fri.music.ToneSystem.*;
import static org.junit.jupiter.api.Assertions.*;
import java.util.List;
import org.junit.jupiter.api.Test;
import fri.music.EqualTemperament;
import fri.music.JustIntonation;
import fri.music.Tone;
import fri.music.ToneSystem;
import fri.music.differencetones.DifferenceToneInversions.TonePair;

class DifferenceToneInversionsTest
{
    @Test
    void c4WithJustIntonation() {
        final ToneSystem toneSystem = new JustIntonation(); // C0 - C10
        final DifferenceToneInversions.Configuration config = new DifferenceToneInversions.Configuration(
                toneSystem.tones(),
                ToneSystem.semitoneSteps(ToneSystem.MINOR_THIRD),
                ToneSystem.semitoneSteps(ToneSystem.MAJOR_SIXTH),
                DifferenceTones.PRECISE_DEVIATION_JI); // small deviation for JI, else F#4-D#5 will be in list
        final DifferenceToneInversions differenceToneInversions = new DifferenceToneInversions(config);
        final List<TonePair> intervals = differenceToneInversions.getIntervalsGenerating("C4");
        
        assertNotNull(intervals);
        assertEquals(6, intervals.size());
        
        assertEquals("E6", intervals.get(0).lowerTone().ipnName); /// MINOR_THIRD
        assertEquals("G6", intervals.get(0).upperTone().ipnName);
        
        assertEquals("C6", intervals.get(1).lowerTone().ipnName); // MAJOR_THIRD
        assertEquals("E6", intervals.get(1).upperTone().ipnName);
        
        assertEquals("G5", intervals.get(2).lowerTone().ipnName); // FOURTH
        assertEquals("C6", intervals.get(2).upperTone().ipnName);
        
        assertEquals("C5", intervals.get(3).lowerTone().ipnName); // FIFTH
        assertEquals("G5", intervals.get(3).upperTone().ipnName);
        
        assertEquals("A4", intervals.get(4).lowerTone().ipnName); // MINOR_SIXTH
        assertEquals("F5", intervals.get(4).upperTone().ipnName);
        
        assertEquals("G4", intervals.get(5).lowerTone().ipnName); // MAJOR_SIXTH
        assertEquals("E5", intervals.get(5).upperTone().ipnName);
    }

    @Test
    void c4WithEqualTemperament() {
        final ToneSystem toneSystem = new EqualTemperament(); // C0 - C10
        final DifferenceToneInversions.Configuration config = new DifferenceToneInversions.Configuration(
                toneSystem.tones(),
                DifferenceTones.DEFAULT_DEVIATION_TOLERANCE); // is sufficiently precise for EDO-12
        final DifferenceToneInversions differenceToneInversions = new DifferenceToneInversions(config);
        final List<TonePair> intervals = differenceToneInversions.getIntervalsGenerating("C4");
        
        assertNotNull(intervals);
        assertEquals(6, intervals.size());
        
        assertEquals("F6", intervals.get(0).lowerTone().ipnName); // MINOR_THIRD
        assertEquals("G#6", intervals.get(0).upperTone().ipnName);
        
        assertEquals("B5", intervals.get(1).lowerTone().ipnName); // MAJOR_THIRD
        assertEquals("D#6", intervals.get(1).upperTone().ipnName);
        
        assertEquals("G5", intervals.get(2).lowerTone().ipnName); // FOURTH
        assertEquals("C6", intervals.get(2).upperTone().ipnName);
        
        assertEquals("D#5", intervals.get(3).lowerTone().ipnName); // TRITONE
        assertEquals("A5", intervals.get(3).upperTone().ipnName);
        
        assertEquals("C5", intervals.get(4).lowerTone().ipnName); // FIFTH
        assertEquals("G5", intervals.get(4).upperTone().ipnName);
        
        assertEquals("A4", intervals.get(5).lowerTone().ipnName); // MINOR_SIXTH
        assertEquals("F5", intervals.get(5).upperTone().ipnName);
    }
    
    @Test
    void f4WithSmallIntervalRange() {
        final ToneSystem toneSystem = new JustIntonation(); // C0 - C10
        final DifferenceToneInversions.Configuration config = new DifferenceToneInversions.Configuration(
                toneSystem.tones(),
                semitoneSteps(MAJOR_THIRD), // allow only range from MAJOR_THIRD to FIFTH
                semitoneSteps(FIFTH),
                DifferenceTones.PRECISE_DEVIATION_JI);
        final DifferenceToneInversions differenceToneInversions = new DifferenceToneInversions(config);
        final List<TonePair> intervals = differenceToneInversions.getIntervalsGenerating("F4");
        
        assertNotNull(intervals);
        assertEquals(3, intervals.size());
        
        assertEquals("F6", intervals.get(0).lowerTone().ipnName); /// MAJOR_THIRD
        assertEquals("A6", intervals.get(0).upperTone().ipnName);
        
        assertEquals("C6", intervals.get(1).lowerTone().ipnName); // FOURTH
        assertEquals("F6", intervals.get(1).upperTone().ipnName);
        
        assertEquals("F5", intervals.get(2).lowerTone().ipnName); // FIFTH
        assertEquals("C6", intervals.get(2).upperTone().ipnName);
    }

    @Test
    void sortOrder() {
        final ToneSystem toneSystem = new EqualTemperament(); // C0 - C10
        final DifferenceToneInversions.Configuration config = new DifferenceToneInversions.Configuration(toneSystem.tones());
        final DifferenceToneInversions differenceToneInversions = new DifferenceToneInversions(config);
        final List<TonePair> intervals = differenceToneInversions.getIntervalsGenerating("C4");
        
        for (int i = 0, semitoneDistance = 0, upperMidiNumber = Integer.MAX_VALUE, lowerMidiNumber = Integer.MAX_VALUE;
                i < intervals.size();
                i++)
        {
            final TonePair interval = intervals.get(i);
            
            assertTrue(interval.semitoneDistance() >= semitoneDistance, "List of intervals is not sorted ascending by width!");
            semitoneDistance = interval.semitoneDistance();
            
            assertTrue(interval.upperTone().midiNumber <= upperMidiNumber, "List of intervals is not sorted descending by pitch of upper tone!");
            upperMidiNumber = interval.upperTone().midiNumber;
            
            assertTrue(interval.lowerTone().midiNumber <= lowerMidiNumber, "List of intervals is not sorted descending by pitch of lower tone!");
            lowerMidiNumber = interval.lowerTone().midiNumber;
        }
    }
    
    /** See also DifferenceToneInversionsDemo. */
    @Test
    void differenceTonesEqualTemperament() {
        final String BASE_TONE = "C2";
        final ToneSystem toneSystem = new EqualTemperament(BASE_TONE, 4); // C2 - C6
        final DifferenceToneInversions differenceToneInversions = new DifferenceToneInversions(
                new DifferenceToneInversions.Configuration(
                    toneSystem.tones(),
                    DifferenceTones.TOLERANT_DEVIATION_EDO_12)
            );
        final List<Tone> differenceTones = differenceToneInversions.differenceTones();
        
        assertNotNull(differenceTones);
        assertEquals(33, differenceTones.size());
        
        assertEquals(BASE_TONE, differenceTones.get(0).ipnName); // the list must be sorted
        assertEquals("G#4", differenceTones.get(differenceTones.size() - 1).ipnName);
    }
    
    /** See also DifferenceToneInversionsDemo. */
    @Test
    void differenceTonesJustIntonation() {
        final String BASE_TONE = "C2";
        final ToneSystem toneSystem = new JustIntonation(BASE_TONE, 4); // C2 - C6
        final DifferenceToneInversions differenceToneInversions = new DifferenceToneInversions(
                new DifferenceToneInversions.Configuration(
                    toneSystem.tones(),
                    DifferenceTones.PRECISE_DEVIATION_JI)
            );
        final List<Tone> differenceTones = differenceToneInversions.differenceTones();
        
        assertNotNull(differenceTones);
        assertEquals(32, differenceTones.size());
        
        assertEquals(BASE_TONE, differenceTones.get(0).ipnName); // the list must be sorted
        assertEquals("G#4", differenceTones.get(differenceTones.size() - 1).ipnName);
    }
    
    @Test
    void removeDissonant() {
        final ToneSystem toneSystem = new EqualTemperament("C3", 5);
        final DifferenceToneInversions differenceToneInversions = new DifferenceToneInversions(
                new DifferenceToneInversions.Configuration(
                    toneSystem.tones(),
                    DifferenceTones.TOLERANT_DEVIATION_EDO_12)
            );
        
        final List<TonePair> generatingC3 = differenceToneInversions.getIntervalsGenerating("C3");
        
        boolean containsDissonant = generatingC3.stream()
            .anyMatch(tonePair -> tonePair.lowerTone().ipnName.startsWith("B"));
        assertTrue(containsDissonant);
        
        differenceToneInversions.removeDissonant(false);
        
        containsDissonant = generatingC3.stream()
                .anyMatch(tonePair -> tonePair.lowerTone().ipnName.startsWith("B"));
        assertFalse(containsDissonant);
        containsDissonant = generatingC3.stream()
                .anyMatch(tonePair -> tonePair.upperTone().ipnName.startsWith("G#"));
        assertTrue(containsDissonant);
        
        differenceToneInversions.removeDissonant(true);
        
        containsDissonant = generatingC3.stream()
                .anyMatch(tonePair -> tonePair.upperTone().ipnName.startsWith("G#"));
        assertFalse(containsDissonant);
    }
}