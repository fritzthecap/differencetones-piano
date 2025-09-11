package fri.music.differencetones.composer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import fri.music.AbstractToneSystem;
import fri.music.EqualTemperament;
import fri.music.JustIntonation;
import fri.music.Tones;
import fri.music.differencetones.DifferenceToneInversions;
import fri.music.differencetones.DifferenceTones;
import fri.music.player.Note;
import fri.music.player.NotesUtil;
import fri.music.player.Player;
import fri.music.player.notelanguage.MelodyFactory;
import fri.music.wavegenerator.SineWaveSoundChannel;

abstract class AbstractComposerTest
{
    /** Set this to true when you want to see composed intervals on console. */
    private final boolean DEBUG = false;
    /** Set this to true when you want to hear composed intervals in Player. */
    private final boolean PLAY = false;
    
    private boolean compareWithoutLength;
    
    protected final void setCompareWithoutLength(boolean compareWithoutLength) {
        this.compareWithoutLength = compareWithoutLength;
    }
    
    /** Checks whether given melody translates into expected intervals with EqualTemperament and 73 %. */
    protected void assertComposeWithEqualTemperament(
            String[] melody,
            String[][] expectedIntervals)
    {
        assertCompose(
                melody, 
                new EqualTemperament(), 
                DifferenceTones.TOLERANT_DEVIATION_EDO_12, 
                expectedIntervals);
    }
    
    /** Checks whether given melody translates into expected intervals with JustIntonation and 18 %. */
    protected void assertComposeWithJustIntonation(
            String[] melody,
            String[][] expectedIntervals)
    {
        assertCompose(
                melody, 
                new JustIntonation(), 
                DifferenceTones.PRECISE_DEVIATION_JI, 
                expectedIntervals);
    }
    
    /** Checks whether given melody translates into expected intervals. */
    protected void assertCompose(
            String[] melodyArray,
            AbstractToneSystem toneSystem, 
            double deviation, 
            String[][] expectedIntervals)
    {
        // build test data
        final Tones tones = new Tones(toneSystem.tones());
        
        final MelodyFactory melodyFactory = new MelodyFactory(toneSystem);
        final Note[][] melodyNotes = melodyFactory.translate(melodyArray);
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
                
                final String realValue = compareWithoutLength 
                        ? intervals[row][column].ipnName 
                        : intervals[row][column].toString();
                assertEquals(expectedNote, realValue, "Fails at interval "+row+" note "+column);
            }
        }
    }

    private void optionalOutputs(AbstractToneSystem toneSystem, Tones tones, Note[] melodyNotes, Note[][] intervals) {
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