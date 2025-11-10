package fri.music.differencetones.composer;

import org.junit.jupiter.api.Test;
import fri.music.EqualTemperament;
import fri.music.differencetones.DifferenceTones;

class ToneRangeTest extends AbstractComposerTest
{
    /** Check that the tone range for "When the Saints" (0.6 octaves) is calculated correctly. */
    @Test
    void melodyRangeLessThanOneOctave() {
        final String whenTheSaints = "-/8 G4/8 B4/8 C5/8 D5/4 B4/4 G4/4 B4/4 A4/2";
        final String[][] expectedIntervals = new String[][] {
            { "-/8", }, { "D6/8", "G6/8" }, { "D6/8", "G#6/8" }, { "C6/8", "G6/8" },
            { "B5/4", "G6/4" }, { "D6/4", "G#6/4" }, { "D6/4", "G6/4" }, { "D6/4", "G#6/4" }, { "E6/2", "A6/2" },
        };
        assertCompose(
                whenTheSaints,
                new EqualTemperament(), 
                DifferenceTones.TOLERANT_DEVIATION_EDO_12, 
                expectedIntervals);
    }
}