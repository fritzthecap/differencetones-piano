package fri.music.player.notelanguage;

import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.Test;
import fri.music.EqualTemperament;
import fri.music.JustIntonation;
import fri.music.ToneSystem;
import fri.music.player.Note;

class MelodyFactoryTest
{
    private final static String[] AUGUSTIN = new String[] { // "Oh du lieber Augustin"
            "G4/4.", "A4/8", "G4/8", "F4/8", 
            "E4/4", "C4/4", "C4/4", 
            "D4/4", "G3/4", "G3/4", 
            "C4/2.",
        };

    @Test
    void translateToVolumeDurationFrequency() {
        final ToneSystem toneSystem = new EqualTemperament();
        final int BEATS_PER_BAR = 3;
        final int BEAT_TYPE = 4;
        
        final MelodyFactory melodyFactory = new MelodyFactory(
                toneSystem,
                140, // BPM
                BEATS_PER_BAR,
                BEAT_TYPE,
                null); // default volume
        final Note[] melody = melodyFactory.translate(AUGUSTIN);
        
        assertNotNull(melody);
        assertEquals(AUGUSTIN.length, melody.length);
        
        for (int i = 0; i < melody.length; i++)
            assertEquals(
                    AUGUSTIN[i].substring(0, AUGUSTIN[i].indexOf("/")), 
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
    void restShouldWork() {
        final String[] notes = new String[] { ToneSystem.REST_SYMBOL };
        final MelodyFactory melodyFactory = new MelodyFactory();
        final Note[] melody = melodyFactory.translate(notes);
        
        assertEquals(melody[0].ipnName, ToneSystem.REST_SYMBOL);
        assertEquals(melody[0].lengthNotation, MelodyFactory.DEFAULT_NOTE_LENGTH); // quarter note is default length
        
        final String REST_LENGTH = "8.";
        final String[] notes2 = new String[] { ToneSystem.REST_SYMBOL+"/"+REST_LENGTH };
        final Note[] melody2 = melodyFactory.translate(notes2);
        
        assertEquals(melody2[0].ipnName, ToneSystem.REST_SYMBOL);
        assertEquals(melody2[0].lengthNotation, REST_LENGTH);
    }
    
    @Test
    void toStringShouldGiveTextNotation() {
        final int BEATS_PER_MINUTE = 88;
        final int BEATS_PER_BAR = 3;
        final int BEAT_TYPE = 4;
        
        final MelodyFactory melodyFactory = new MelodyFactory(BEATS_PER_MINUTE, BEATS_PER_BAR, BEAT_TYPE);
        final Note[] melody = melodyFactory.translate(AUGUSTIN);
        final String textNotation = melodyFactory.toString(melody, true, true);
        
        final String expectedResult = 
                ""+BEATS_PER_MINUTE + MelodyFactory.NEWLINE+
                ""+BEATS_PER_BAR+"/"+BEAT_TYPE + MelodyFactory.NEWLINE+
                "G4/4. A4/8 G4/8 F4/8" + MelodyFactory.NEWLINE+
                "E4/4 C4/4 C4/4" + MelodyFactory.NEWLINE+
                "D4/4 G3/4 G3/4" + MelodyFactory.NEWLINE+
                "C4/2." + MelodyFactory.NEWLINE;
        assertEquals(expectedResult, textNotation);
        
        final Note[] roundTripped = melodyFactory.translate(textNotation);
        assertArrayEquals(melody, roundTripped);
    }
    
    @Test
    void slursAndTiesShouldBeFormattedCorrectly() {
        final int BEATS_PER_MINUTE = 88;
        final int BEATS_PER_BAR = 3;
        final int BEAT_TYPE = 4;
        
        final MelodyFactory melodyFactory = new MelodyFactory(BEATS_PER_MINUTE, BEATS_PER_BAR, BEAT_TYPE);
        
        final String[] notes = new String[] { "(G4/8", "G4/8", "G4/8)", "{A4/8", "G4/8", "F4/8}" };

        final Note[] melody = melodyFactory.translate(notes);
        final String textNotation = melodyFactory.toString(melody, true, true);
        
        final String expectedResult = 
                ""+BEATS_PER_MINUTE + MelodyFactory.NEWLINE+
                ""+BEATS_PER_BAR+"/"+BEAT_TYPE + MelodyFactory.NEWLINE+
                "(G4/8 (G4/8) G4/8) {A4/8 G4/8 F4/8}" + MelodyFactory.NEWLINE;
        assertEquals(expectedResult, textNotation);
    }
    
    @Test
    void tiesAcrossBarsShouldBeFormattedCorrectly() {
        final String tieAcrossBars = "E5/2. B5/8 {E5/8 (A5/1} (A5/1) A5/1)";
        
        final MelodyFactory melodyFactory = new MelodyFactory(); // 120 BPM default
        
        final Note[] notes = melodyFactory.translate(tieAcrossBars);
        final String text = melodyFactory.toString(notes, false, false);
        
        final String expectedResult = 
                "E5/2. B5/8 {E5/8" + MelodyFactory.NEWLINE +
                "(A5/1}" + MelodyFactory.NEWLINE +
                "(A5/1)" + MelodyFactory.NEWLINE +
                "A5/1)" + MelodyFactory.NEWLINE;
        
        assertEquals(expectedResult, text);
    }
    
    @Test
    void noteLengthMillis() {
        final int beatsPerMinute = 120;
        final int beatDurationMillis = MelodyFactory.beatDurationMillis(beatsPerMinute);
        assertEquals(500, beatDurationMillis); 
        
        // calculate 1/16 when BPM = 120 and beat-type is quarter note (like in 4/4 time signature)
        final int sixteenthNoteMillis = (int) Math.round(MelodyFactory.noteLengthMillis(16, 4, beatDurationMillis));
        assertEquals(125, sixteenthNoteMillis);
        
        // calculate 1/1 
        final int wholeNoteMillis = (int) Math.round(MelodyFactory.noteLengthMillis(1, 4, beatDurationMillis));
        assertEquals(2000, wholeNoteMillis); 
    }
    
    @Test
    void noteLengthDivisor() {
        final int beatsPerMinute = 120;
        final int beatDurationMillis = MelodyFactory.beatDurationMillis(beatsPerMinute);
        assertEquals(500, beatDurationMillis); 
        
        // calculate 1/16 when BPM = 120 and beat-type is quarter note (like in 4/4 time signature)
        final String sixteenth = MelodyFactory.noteLengthDivisor(160, 4, beatDurationMillis);
        assertEquals("16", sixteenth);
        
        // calculate 1/8
        final String eighth = MelodyFactory.noteLengthDivisor(210, 4, beatDurationMillis);
        assertEquals("8", eighth); 
        
        // calculate 1/4
        final String quarter = MelodyFactory.noteLengthDivisor(570, 4, beatDurationMillis);
        assertEquals("4", quarter); 
        
        // calculate 1/2
        final String half = MelodyFactory.noteLengthDivisor(912, 4, beatDurationMillis);
        assertEquals("2", half); 
        
        // calculate 1/1
        final String whole = MelodyFactory.noteLengthDivisor(1800, 4, beatDurationMillis);
        assertEquals("1", whole); 
        final String wholeLong = MelodyFactory.noteLengthDivisor(4000, 4, beatDurationMillis);
        assertEquals("1", wholeLong); // any length longer than a bar will be reduced to a whole note
        
        final String smallest = MelodyFactory.noteLengthDivisor(1, 4, beatDurationMillis);
        assertEquals("64", smallest);
        final String zero = MelodyFactory.noteLengthDivisor(0, 4, beatDurationMillis);
        assertEquals("64", zero);
    }

    @Test
    void tripletDuration() {
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
    void customTripletDuration() {
        final String[] notes = new String[] {
            "G4/4", "A4/4", 
            "B4/2,3", "A4/4,3", // triplet
            "D4/1",
        };
        
        final MelodyFactory melodyFactory = new MelodyFactory();
        final Note[] melody = melodyFactory.translate(notes);
        
        // the first two quarter notes must be of same length as the two triplet notes
        assertEquals(
                melody[0].durationMilliseconds + melody[1].durationMilliseconds,
                melody[2].durationMilliseconds + melody[3].durationMilliseconds, 
                1, // one millisecond comparison precision 
                "Custom triplet should be of same duration as two quarter notes!");
        // the first triplet note must be twice as long as the second
        assertEquals(
                melody[2].durationMilliseconds,
                2 * melody[3].durationMilliseconds, 
                1, // one millisecond comparison precision 
                "First triplet note should be twice as long as second triplet note!");
    }

    @Test
    void quintupletDuration() {
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
        
        final MelodyFactory melodyFactory = new MelodyFactory(); // sets default beat duration
        final Note[] melody = melodyFactory.translate(notes);
        
        for (int i = 0; i < melody.length; i++)
            assertEquals(Note.DEFAULT_BEAT_DURATION, melody[i].durationMilliseconds);
    }

    @Test
    void textInputShouldWork() {
        final String notesText = "G4/16 "+ToneSystem.REST_SYMBOL+"/16 A4/16";
        final MelodyFactory melodyFactory = new MelodyFactory();
        final Note[] melody = melodyFactory.translate(notesText);
        
        assertEquals(melody[0].ipnName, "G4");
        assertEquals(melody[0].lengthNotation, "16");
        assertEquals(melody[1].ipnName, ToneSystem.REST_SYMBOL);
        assertEquals(melody[1].lengthNotation, "16");
        assertEquals(melody[2].ipnName, "A4");
        assertEquals(melody[2].lengthNotation, "16");
    }
    
    @Test
    void c3HasDifferentFrequenciesInToneSystems() {
        final String[] notes = new String[] { "C3" }; // do not use A4, because this has the same frequency in all tunings!
        
        final ToneSystem toneSystem1 = new EqualTemperament(); // EDO-12
        final MelodyFactory melodyFactory1 = new MelodyFactory(toneSystem1);
        final Note[] melodyEdo12 = melodyFactory1.translate(notes);
        
        final ToneSystem toneSystem2 = new JustIntonation(); // LIMIT_5_SYMMETRIC_1
        final MelodyFactory melodyFactory2 = new MelodyFactory(toneSystem2);
        final Note[] melodyLimit5 = melodyFactory2.translate(notes);
        
        assertNotEquals(melodyEdo12[0].frequency, melodyLimit5[0].frequency);
    }

    @Test
    void timeSignatureChangeShouldWork() {
        final String[] notes = new String[] {
            "4/4 ", // not a note but time signature change!
            "A4", "B4", "C5", "B4", // 0, 1, 2, 3
            " 3/4", // time signature change!
            "C5", "D5", "E5", // 4, 5, 6
            "4/4 ", // time signature change!
            "F5", "G5", "A5", "G5", // 7, 8, 9, 10
        };
        final MelodyFactory melodyFactory = new MelodyFactory(); // 120 BPM default
        final Note[] melody = melodyFactory.translate(notes);
        
        assertEquals(notes.length - 3, melody.length);
        
        assertTrue(melody[0].emphasized); // first note in bar must be emphasized
        assertTrue(melody[0].volume > melody[1].volume); // first note is loudest
        assertTrue(melody[1].volume < melody[2].volume); // 4/4 half bar has a small accent
        assertTrue(melody[0].volume > melody[2].volume); // but less than first note
        assertTrue(melody[2].volume > melody[3].volume);
        assertTrue(melody[3].volume == melody[1].volume); // notes with no accent must be equal
        assertTrue(melody[3].volume < melody[4].volume);
        
        assertTrue(melody[4].emphasized); // first note in bar must be emphasized
        assertTrue(melody[4].volume > melody[5].volume); // first note is loudest
        assertTrue(melody[5].volume == melody[6].volume); // no half bar in 3/4, thus no accent
        assertTrue(melody[6].volume < melody[7].volume);
        
        assertTrue(melody[7].emphasized); // first note in bar must be emphasized
        assertTrue(melody[7].volume > melody[8].volume); // first note is loudest
        assertTrue(melody[8].volume < melody[9].volume); // 4/4 half bar has a small accent
        assertTrue(melody[9].volume > melody[10].volume);
    }

    @Test
    void tiesAndSlursShouldWork() {
        final String[] notes = new String[] {
            "A4/8", "( B4/8", " (B4/4 ) ", "B4/8)", "C5/4.",
            "{ B4", " C5 ", " B4}", "A4"
        };
        final MelodyFactory melodyFactory = new MelodyFactory(120); // 120 BPM
        assertEquals(500, melodyFactory.getBeatDurationMilliseconds());
        
        final Note[] resultMelody = melodyFactory.translate(notes);
        
        assertEquals(notes.length, resultMelody.length);
        
        assertNull(resultMelody[0].connectionFlags.slurred());
        assertNull(resultMelody[0].connectionFlags.tied());
        
        assertNull(resultMelody[1].connectionFlags.slurred());
        assertEquals(Boolean.TRUE, resultMelody[1].connectionFlags.tied());
        assertEquals(
            melodyFactory.getBeatDurationMilliseconds() * 2, // one beat is a quarter note here
            resultMelody[1].durationMilliseconds);
            // first note in tie should have duration of two eighth and one quarter notes
        assertEquals(
                1000, // a quarter note on 120 BPM is 1000 millis
                resultMelody[1].durationMilliseconds);
        
        assertNull(resultMelody[2].connectionFlags.slurred());
        assertEquals(Boolean.TRUE, resultMelody[2].connectionFlags.tied());
        assertEquals(0, resultMelody[2].durationMilliseconds); // tied follower must have no duration
        
        assertNull(resultMelody[3].connectionFlags.slurred());
        assertEquals(Boolean.FALSE, resultMelody[3].connectionFlags.tied());
        assertEquals(0, resultMelody[3].durationMilliseconds); // tied follower must have no duration
        
        assertNull(resultMelody[4].connectionFlags.slurred());
        assertNull(resultMelody[4].connectionFlags.tied());
        
        assertEquals(Boolean.TRUE, resultMelody[5].connectionFlags.slurred());
        assertNull(resultMelody[5].connectionFlags.tied());
        
        assertEquals(Boolean.TRUE, resultMelody[6].connectionFlags.slurred());
        assertNull(resultMelody[6].connectionFlags.tied());
        
        assertEquals(Boolean.FALSE, resultMelody[7].connectionFlags.slurred());
        assertNull(resultMelody[7].connectionFlags.tied());
        
        assertNull(resultMelody[8].connectionFlags.slurred());
        assertNull(resultMelody[8].connectionFlags.tied());
    }

    @Test
    void notesWithinTiesNeedNotBeTied() {
        final String[] notes = new String[] {
            "A4/8", "( B4/8", "B4/4", "B4/8)", "C5/4.",
        };
        final MelodyFactory melodyFactory = new MelodyFactory(); // 120 BPM default
        
        final Note[] resultMelody = melodyFactory.translate(notes);
        
        assertNull(resultMelody[0].connectionFlags.tied());
        assertEquals(Boolean.TRUE, resultMelody[1].connectionFlags.tied());
        assertEquals(Boolean.TRUE, resultMelody[2].connectionFlags.tied());
        assertEquals(Boolean.FALSE, resultMelody[3].connectionFlags.tied());
        assertNull(resultMelody[4].connectionFlags.tied());
    }

    @Test
    void nestedTiesAndSlursShouldWork() {
        final String[] notes = new String[] {
            "{ A4/8", "( B4/8", " (B4/4 ) ", "B4/8)", "C5/4.}",
        };
        final MelodyFactory melodyFactory = new MelodyFactory(); // 120 BPM default
        
        final Note[] resultMelody = melodyFactory.translate(notes);
        
        assertNull(resultMelody[0].connectionFlags.tied());
        assertEquals(Boolean.TRUE, resultMelody[0].connectionFlags.slurred());
        
        assertEquals(Boolean.TRUE, resultMelody[1].connectionFlags.tied());
        assertEquals(Boolean.TRUE, resultMelody[1].connectionFlags.slurred());
        
        assertEquals(Boolean.TRUE, resultMelody[2].connectionFlags.tied());
        assertEquals(Boolean.TRUE, resultMelody[2].connectionFlags.slurred());
        
        assertEquals(Boolean.FALSE, resultMelody[3].connectionFlags.tied());
        assertEquals(Boolean.TRUE, resultMelody[3].connectionFlags.slurred());
        
        assertNull(resultMelody[4].connectionFlags.tied());
        assertEquals(Boolean.FALSE, resultMelody[4].connectionFlags.slurred());
    }
}