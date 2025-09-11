package fri.music.differencetones.composer;

import org.junit.jupiter.api.Test;

class BluesChordsTest extends AbstractComposerTest
{
    /** The quarter notes of the melody this test is about. */
    private static final String[] melody = new String[] { // 12-bar Blues
            "C4", "E4", "G4", "E4", // C
            "F4", "A4", "C5", "A4", // F
            "C4", "E4", "G4", "A4", // C
            "A#4","A4", "G4", "E4", // C7
            "F4", "A4", "C5", "A4", // F
            "D5", "C5", "A4", "F4", // F
            "C4", "E4", "G4", "E4", // C
            "A4", "G4", "E4", "C4", // C
            "G4", "B4", "D5", "B4", // G
            "F4", "A4", "C5", "A4", // F
            "C4", "E4", "G4", "A#4",// C
            "G4", "D4", "G3", "G3", // G
        };
    
    @Test
    void withEqualTemperament() {
        setCompareWithoutLength(true);
        final String[][] expectedIntervals = new String[][] {
            { "G5", "C6" }, { "G5", "C#6" }, { "G5", "D6" }, { "G5", "C#6" },
            { "F5", "C6" }, { "F#5", "D6" }, { "G5", "E6" }, { "F#5", "D6" },
            { "G5", "C6" }, { "G5", "C#6" }, { "G5", "D6" }, { "F#5", "D6" },
            { "F5", "D6" }, { "F#5", "D6" }, { "G5", "D6" }, { "G5", "C#6" },
            { "F5", "C6" }, { "F#5", "D6" }, { "G5", "E6" }, { "F#5", "D6" },
            { "A5", "F#6"}, { "A5", "F6"  }, { "A5", "E6" }, { "G#5", "D6" },
            { "G5", "C6" }, { "G5", "C#6" }, { "G5", "D6" }, { "G5", "C#6" },
            { "F#5", "D6"}, { "G5", "D6"  }, { "G5", "C#6"}, { "G5", "C6"  },
            { "G5", "D6" }, { "G#5", "E6" }, { "A5", "F#6"}, { "G#5", "E6" },
            { "F5", "C6" }, { "F#5", "D6" }, { "G5", "E6" }, { "F#5", "D6" },
            { "G5", "C6" }, { "G5", "C#6" }, { "G5", "D6" }, { "F5", "D6"  },
            { "G5", "D6" }, { "A5", "D6"  }, { "C6", "D#6"}, { "C6", "D#6" },
        };
        assertComposeWithEqualTemperament(melody, expectedIntervals);
    }
    
    @Test
    void withJustIntonation() {
        setCompareWithoutLength(true);
        final String[][] expectedIntervals = new String[][] {
            { "C6", "E6" }, { "B5", "E6" }, { "G5", "D6" }, { "B5", "E6" },
            { "F5", "C6" }, { "E6", "A6" }, { "G5", "E6" }, { "E6", "A6" },
            { "C6", "E6" }, { "B5", "E6" }, { "G5", "D6" }, { "E6", "A6" },
            { "A#5", "F6"}, { "E6", "A6" }, { "G5", "D6" }, { "B5", "E6" },
            { "F5", "C6" }, { "E6", "A6" }, { "G5", "E6" }, { "E6", "A6" },
            { "B5", "G6" }, { "G5", "E6" }, { "E6", "A6" }, { "F5", "C6" },
            { "C6", "E6" }, { "B5", "E6" }, { "G5", "D6" }, { "B5", "E6" },
            { "A5", "E6" }, { "D6", "G6" }, { "E5", "B5" }, { "C6", "E6" },
            { "G5", "D6" }, { "D#6", "A6"}, { "B5", "G6" }, { "D#6", "A6"},
            { "F5", "C6" }, { "E6", "A6" }, { "G5", "E6" }, { "E6", "A6" },
            { "C6", "E6" }, { "B5", "E6" }, { "G5", "D6" }, { "F6", "A#6"},
            { "G5", "D6" }, { "D6", "F#6"}, { "B5", "D6" }, { "B5", "D6" },
        };
        assertComposeWithJustIntonation(melody, expectedIntervals);
    }
}

/* withEqualTemperament
final String[][] expectedIntervals = new String[][] {
    { "E6","G6" }, { "D#6","G6" }, { "D6","G6" }, { "D#6","G6" }, // C
    { "C6","F6" }, { "C6","F#6" }, { "C6","G6" }, { "C6","F#6" }, // F
    { "E6","G6" }, { "D#6","G6" }, { "D6","G6" }, { "C6","F#6" }, // C
    { "A#5","F6"}, { "C6","F#6" }, { "D6","G6" }, { "D#6","G6" }, // C7
    { "C6","F6" }, { "C6","F#6" }, { "C6","G6" }, { "C6","F#6" }, // F
    { "B5","G6" }, { "C6","G6"  }, { "C6","F#6"}, { "C6","F6"  }, // F
    { "E6","G6" }, { "D#6","G6" }, { "D6","G6" }, { "D#6","G6" }, // C
    { "C6","F#6"}, { "D6","G6"  }, { "D#6","G6"}, { "E6","G6"  }, // C
    { "D6","G6" }, { "D6","G#6" }, { "D6","A6" }, { "D6","G#6" }, // G
    { "C6","F6" }, { "C6","F#6" }, { "C6","G6" }, { "C6","F#6" }, // F
    { "E6","G6" }, { "D#6","G6" }, { "D6","G6" }, { "C#6","G6" }, // C
    { "D6","G6" }, { "C#6","F6" }, { "B5","D6" }, { "B5","D6"  }, // G
};
*/