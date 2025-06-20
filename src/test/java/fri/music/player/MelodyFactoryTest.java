package fri.music.player;

import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.Test;
import fri.music.EqualTemperament;
import fri.music.JustIntonation;
import fri.music.ToneSystem;

class MelodyFactoryTest
{
    @Test
    void translateToVolumeDurationFrequency() {
        final ToneSystem toneSystem = new EqualTemperament();
        final int BEATS_PER_BAR = 3;
        final int BEAT_TYPE = 4;
        final String[] notes = new String[] { // "Oh du lieber Augustin"
            "G4/4.", "A4/8", "G4/8", "F4/8", 
            "E4/4", "C4/4", "C4/4", 
            "D4/4", "G3/4", "G3/4", 
            "C4/2.",
        };
        
        final MelodyFactory melodyFactory = new MelodyFactory(
                toneSystem,
                BEATS_PER_BAR,
                BEAT_TYPE,
                140, // BPM
                null); // default volume
        final Note[] melody = melodyFactory.translate(notes);
        
        assertNotNull(melody);
        assertEquals(notes.length, melody.length);
        
        for (int i = 0; i < melody.length; i++)
            assertEquals(
                    notes[i].substring(0, notes[i].indexOf("/")), 
                    melody[i].ipnName);
        
        assertTrue(melody[0].volume > melody[1].volume, "First beat should be louder than subsequent ones!");
        assertEquals(melody[0].volume, melody[4].volume, "All first bar beats should be of same loudness!");
        
        assertEquals(melody[0].durationMilliseconds, 3 * melody[1].durationMilliseconds, 1, // 1 = comparison precision
                "First note should be 3 times longer than second note!");
        assertEquals(melody[1].durationMilliseconds, melody[2].durationMilliseconds,
                "Second and third note should be of same length!");
        assertEquals(melody[10].durationMilliseconds, 3 * melody[9].durationMilliseconds,
                "Last note should be 3 times longer than its predecessor!");
        
        assertEquals(melody[0].frequency, melody[2].frequency, "First and third note should have same frequency!");
        assertNotEquals(melody[0].frequency, melody[1].frequency, "First and second note should not have same frequency!");
    }

    @Test
    void tripletsDuration() {
        final String[] notes = new String[] { // one 4/4 bar
            "G4/4", "A4/4", 
            "B4/4,3", "A4/4,3", "G4/4,3", // triplet
            "D4/1",
        };
        
        final MelodyFactory melodyFactory = new MelodyFactory();
        final Note[] melody = melodyFactory.translate(notes);
        
        // triplet must have different duration than quarter
        assertNotEquals(melody[0].durationMilliseconds, melody[2].durationMilliseconds);
        
        // all triplets must have same duration
        assertEquals(melody[2].durationMilliseconds, melody[3].durationMilliseconds);
        assertEquals(melody[3].durationMilliseconds, melody[4].durationMilliseconds);
        
        assertEquals(
                melody[0].durationMilliseconds + melody[1].durationMilliseconds,
                melody[2].durationMilliseconds + melody[3].durationMilliseconds + melody[4].durationMilliseconds, 
                1, // one millisecond comparison precision 
                "Three triplet quarter notes should be of same duration as two quarter notes!");
    }

    @Test
    void quintupletsDuration() {
        final String[] notes = new String[] {
            "E4/4", "D4/4", 
            "G4/8,5", "A4/8,5", "B4/8,5", "A4/8,5", "G4/8,5", // quintuplet
            "D5/2",
        };
        
        final MelodyFactory melodyFactory = new MelodyFactory();
        final Note[] melody = melodyFactory.translate(notes);
        
        // quintuplet must have different duration than eighth
        assertNotEquals(melody[0].durationMilliseconds, melody[2].durationMilliseconds);
        
        // all quintuplets must have same duration
        assertEquals(melody[2].durationMilliseconds, melody[3].durationMilliseconds);
        assertEquals(melody[3].durationMilliseconds, melody[4].durationMilliseconds);
        assertEquals(melody[4].durationMilliseconds, melody[5].durationMilliseconds);
        assertEquals(melody[5].durationMilliseconds, melody[6].durationMilliseconds);
        
        assertEquals(
                melody[0].durationMilliseconds + melody[1].durationMilliseconds,
                melody[2].durationMilliseconds + 
                    melody[3].durationMilliseconds + 
                    melody[4].durationMilliseconds + 
                    melody[5].durationMilliseconds + 
                    melody[6].durationMilliseconds, 
                1, // one millisecond comparison precision 
                "Five quintuplet eighth notes should be of same duration as two quarter notes!");
    }

    @Test
    void wrongMultipletType() {
        final String[] notes = new String[] {
            "G4/8,7", // septuplet is ambiguous and thus not supported
        };
        assertThrows(
            IllegalArgumentException.class, 
            () -> new MelodyFactory().translate(notes)
        );
    }

    @Test
    void quarterNoteIsDefault() {
        final String[] notes = new String[] {
            "G4", "A4", "B4", "D4" // don't need the "/4" suffix on quarter notes
        };
        
        final MelodyFactory melodyFactory = new MelodyFactory(); // default BPM tempo
        final Note[] melody = melodyFactory.translate(notes);
        
        for (int i = 0; i < melody.length; i++)
            assertEquals(Note.DEFAULT_BEAT_DURATION, melody[i].durationMilliseconds);
    }

    @Test
    void differentTunings() {
        final String[] notes = new String[] { "C3" }; // do not use A4, because this has the same frequency in all tunings!
        
        final ToneSystem toneSystem1 = new EqualTemperament(); // EDO-12
        final MelodyFactory melodyFactory1 = new MelodyFactory(toneSystem1);
        final Note[] melodyEdo12 = melodyFactory1.translate(notes);
        
        final ToneSystem toneSystem2 = new JustIntonation(); // LIMIT_5_SYMMETRIC_1
        final MelodyFactory melodyFactory2 = new MelodyFactory(toneSystem2);
        final Note[] melodyLimit5 = melodyFactory2.translate(notes);
        
        assertNotEquals(melodyEdo12[0].frequency, melodyLimit5[0].frequency);
    }
}