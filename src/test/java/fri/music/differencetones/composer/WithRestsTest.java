package fri.music.differencetones.composer;

import org.junit.jupiter.api.Test;

class WithRestsTest extends AbstractComposerTest
{
    /** The difference-tone mapping of a melody must stay the same when rests are put into the melody. */
    @Test
    void withRests() {
        final String[] melodyWithoutRests = new String[] { "C4/4", "D4/4", "E4/4" };
        final String[][] expectedIntervals1 = new String[][] {
            { "D#5/4", "A5/4" }, { "D5/4", "A5/4" }, { "C#5/4", "A5/4" },
        };
        assertComposeWithEqualTemperament(melodyWithoutRests, expectedIntervals1);
        
        final String[] melodyWithRests = new String[] { "C4/4", "-/4", "D4/4", "-/4", "E4/4" };
        final String[][] expectedIntervals2 = new String[][] {
            { "D#5/4", "A5/4" }, { "-/4", }, { "D5/4", "A5/4" }, { "-/4", }, { "C#5/4", "A5/4" },
        };
        assertComposeWithEqualTemperament(melodyWithRests, expectedIntervals2);
    }
}
