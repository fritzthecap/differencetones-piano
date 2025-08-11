package fri.music.differencetones.composer;

import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.Test;
import fri.music.*;
import fri.music.differencetones.DifferenceToneInversions;
import fri.music.differencetones.DifferenceTones;
import fri.music.player.Note;
import fri.music.player.NotesUtil;
import fri.music.player.Player;
import fri.music.player.notelanguage.MelodyFactory;
import fri.music.wavegenerator.SineWaveSoundChannel;

class DefaultComposerTest
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
        assertCompose(new EqualTemperament(), DifferenceTones.TOLERANT_DEVIATION_EDO_12, expectedIntervals);
    }
    
    @Test
    void withJustIntonation() {
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
        assertCompose(new JustIntonation(), DifferenceTones.PRECISE_DEVIATION_JI, expectedIntervals);
    }
    
    
    private void assertCompose(AbstractToneSystem toneSystem, double deviation, String[][] expectedIntervals) {
        // build test data
        final Tones tones = new Tones(toneSystem.tones());
        
        final MelodyFactory melodyFactory = new MelodyFactory(toneSystem);
        final Note[][] melodyNotes = melodyFactory.translate(melody);
        final Note[] melody = NotesUtil.toSingleNotesArray(melodyNotes);
        final AbstractComposer composer = new DefaultComposer(toneSystem.tones(), null, null, deviation);
        
        // perform test
        final Note[][] intervals = composer.compose(melody);
        
        // assert results
        assertNotNull(intervals);
        assertEquals(melody.length, intervals.length);
        
        optionalOutputs(toneSystem, tones, melody, intervals);
        
        for (int row = 0; row < expectedIntervals.length; row++) {
            final String[] expectedInterval = expectedIntervals[row];
            
            for (int column = 0; column < expectedInterval.length; column++) {
                final String expectedNote = expectedInterval[column];
                
                assertNotNull(intervals[row]);
                assertNotNull(intervals[row][column]);
                
                assertEquals(expectedNote, intervals[row][column].ipnName);
            }
        }
    }

    private void optionalOutputs(AbstractToneSystem toneSystem, Tones tones, Note[] melodyNotes, Note[][] intervals) {
        final boolean DEBUG = false; // set this to true when you want to see composed intervals on console
        final boolean PLAY = false; // set this to true when you want to hear composed intervals in Player
        
        if (DEBUG)  {
            System.out.println(toneSystem);
            
            dumpAsJavaSourcecode(intervals);
            
            for (int i = 0; i < intervals.length; i++) {
                final Note note = melodyNotes[i];
                final Note[] interval = intervals[i];
                System.out.println(note.ipnName+" = "+new DifferenceToneInversions.TonePair(interval[0], interval[1]));
            }
        }
        
        if (PLAY) {
            final Player player = new Player(new SineWaveSoundChannel(tones.tones));
            player.playInRow(intervals);
            player.close();
        }
    }

    /** Outputs given intervals as Java source code. */
    private void dumpAsJavaSourcecode(final Note[][] intervals) {
        System.out.println("final String[][] expectedIntervals = new String[][] {");
        
        for (int i = 0; i < intervals.length; i++) {
            final Note[] interval = intervals[i];
            
            if (i > 0 && i + 1 < intervals.length & i % 4 == 0) {
                System.out.println();
                System.out.print("    ");
            }
            else {
                if (i > 0)
                    System.out.print(" ");
                else
                    System.out.print("    ");
            }
            
            System.out.print("{ \""+interval[0].ipnName+"\", \""+interval[1].ipnName+"\" },");
        }
        
        System.out.println();
        System.out.println("};");
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