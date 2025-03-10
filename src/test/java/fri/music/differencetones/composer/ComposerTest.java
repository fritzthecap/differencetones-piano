package fri.music.differencetones.composer;

import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.Test;
import fri.music.*;
import fri.music.differencetones.DifferenceToneInversions;
import fri.music.differencetones.DifferenceTones;
import fri.music.player.Note;
import fri.music.player.Player;
import fri.music.wavegenerator.SineWaveSoundChannel;

class ComposerTest
{
    @Test
    void testComposerWithEqualTemperament() {
        final AbstractToneSystem toneSystem = new EqualTemperament();
        final double deviation = DifferenceTones.TOLERANT_DEVIATION_EDO_12;
        testComposer(toneSystem, deviation);
    }
    
    @Test
    void testComposerWithJustIntonation() {
        final AbstractToneSystem toneSystem = new JustIntonation();
        final double deviation = DifferenceTones.PRECISE_DEVIATION_JI;
        testComposer(toneSystem, deviation);
    }
    
    private void testComposer(AbstractToneSystem toneSystem, double deviation) {
        System.out.println(toneSystem);
        final Tones tones = new Tones(toneSystem.tones());
        
        final String[] notes = new String[] { // 12-bar Blues
            "C4", "E4", "G4", "E4", // C
            "F4", "A4", "C5", "A4", // F
            "C4", "E4", "G4", "A4", // C
            "A#4", "A4", "G4", "E4",// C7
            "F4", "A4", "C5", "A4", // F
            "D5", "C5", "A4", "F4", // F
            "C4", "E4", "G4", "E4", // C
            "A4", "G4", "E4", "C4", // C
            "G4", "B4", "D5", "B4", // G
            "F4", "A4", "C5", "A4", // F
            "C4", "E4", "G4", "A#4",// C
            "G4", "D4", "G3", "G3", // G
        };
        final Note[] melody = new Note[notes.length];
        final int millisPerQuarterNote = 500; // quarter note
        for (int i = 0; i < notes.length; i++)
            melody[i] = new Note(
                    tones, 
                    notes[i], 
                    millisPerQuarterNote, 
                    (i % 4 == 0) ? 16 : (i % 2 == 0) ? 12 : 8);
        
        final AbstractComposer composer = new Composer(toneSystem, deviation);
        final Note[][] intervals = composer.compose(melody);
        assertNotNull(intervals);
        assertEquals(notes.length, intervals.length);
        
        for (int i = 0; i < intervals.length; i++) {
            final Note note = melody[i];
            final Note[] interval = intervals[i];
            System.out.println(note.ipnName+" = "+new DifferenceToneInversions.TonePair(interval[0], interval[1]));
        }
        
//        final Player player = new Player(new SineWaveSoundChannel(tones.tones));
//        player.playInRow(intervals);
//        player.close();

//        final String[][] expectedIntervalNotes = new String[][] {
//            { "E6","G6" }, { "D#6","G6" }, { "D6","G6" }, { "D#6","G6" }, // C
//            { "C6","F6" }, { "C6","F#6" }, { "C6","G6" }, { "C6","F#6" }, // F
//            { "E6","G6" }, { "D#6","G6" }, { "D6","G6" }, { "C6","F#6" }, // C
//            { "A#5","F6"}, { "C6","F#6" }, { "D6","G6" }, { "D#6","G6" }, // C7
//            { "C6","F6" }, { "C6","F#6" }, { "C6","G6" }, { "C6","F#6" }, // F
//            { "B5","G6" }, { "C6","G6"  }, { "C6","F#6"}, { "C6","F6"  }, // F
//            { "E6","G6" }, { "D#6","G6" }, { "D6","G6" }, { "D#6","G6" }, // C
//            { "C6","F#6"}, { "D6","G6"  }, { "D#6","G6"}, { "E6","G6"  }, // C
//            { "D6","G6" }, { "D6","G#6" }, { "D6","A6" }, { "D6","G#6" }, // G
//            { "C6","F6" }, { "C6","F#6" }, { "C6","G6" }, { "C6","F#6" }, // F
//            { "E6","G6" }, { "D#6","G6" }, { "D6","G6" }, { "C#6","G6" }, // C
//            { "D6","G6" }, { "C#6","F6" }, { "B5","D6" }, { "B5","D6"  }, // G
//        };
//        
//        for (int row = 0; row < expectedIntervalNotes.length; row++) {
//            final String[] expectedInterval = expectedIntervalNotes[row];
//            
//            System.err.println(""+notes[row]+" -> "+intervals[row][0].ipnName+"-"+intervals[row][1].ipnName);
//            
//            for (int column = 0; column < expectedInterval.length; column++) {
//                final String expectedNote = expectedInterval[column];
//                
//                assertNotNull(intervals[row]);
//                assertNotNull(intervals[row][column]);
//                
//                assertEquals(expectedNote, intervals[row][column].ipnName);
//            }
//        }
    }
}