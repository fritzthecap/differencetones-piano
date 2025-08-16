package fri.music.player.notelanguage;

import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.Test;
import fri.music.player.Note;

class ExportToAbcTest
{
    ////////////////////////////////////////////////////////////////////////////
    
    @Test
    void singleNotes() {
        final String MELODY_NOTES = """
3/4
G4/4. A4/8 G4/8 F4/8 E4/4 C4/4 C4/4
D4/4 G3/4 G3/4 E4/4 C4/4 C4/4
G4/4. A4/8 G4/8 F4/8 E4/4 C4/4 C4/4
D4/4 G3/4 G3/4 C4/2.
D4/4 G3/4 G3/4 E4/4 C4/4 C4/4
D4/4 G3/4 G3/4 E4/4 C4/4 C4/4
G4/4. A4/8 G4/8 F4/8 E4/4 C4/4 C4/4
D4/4 G3/4 G3/4 C4/2.""";
        
        final MelodyFactory melodyFactory = new MelodyFactory(
                144, // tempo in BPM
                3, // beats per bar
                4); // beat type is quarter note
        
        final ExportToAbc.Configuration configuration = new ExportToAbc.Configuration(
                2, // song number
                "Oh du lieber Augustin", // title
                "Wiener Lied aus der Pestzeit", // sub-title
                "Marx Augustin", // author
                "17. Jahrhundert", // date
                "C", // key of tune
                4); // bars per line
        
        final String expected = """
X: 2
T: Oh du lieber Augustin
T: Wiener Lied aus der Pestzeit
C: Marx Augustin, 17. Jahrhundert
M: 3/4
Q: 1/4=144
L: 1/1
K: C
G/4> A/4`G/8`F/8 | E/4 C/4 C/4 | D/4 G,/4 G,/4 | E/4 C/4 C/4 |
G/4> A/4`G/8`F/8 | E/4 C/4 C/4 | D/4 G,/4 G,/4 | C/2> |
D/2 G,/4 G,/4 | E/4 C/4 C/4 | D/4 G,/4 G,/4 | E/4 C/4 C/4 |
G/4> A/4`G/8`F/8 | E/4 C/4 C/4 | D/4 G,/4 G,/4 | C/2> ||""";
        
        runTest(MELODY_NOTES, expected, melodyFactory, configuration);
    }
    
    
    ////////////////////////////////////////////////////////////////////////////
    
    @Test
    void chords() {
        final String CHORD_NOTES = """
{[ C4/8 E4/8 G4/8 ] [ D4/8 F#4/8 A4/8 ]}
([ C4/8 E4/8 G4/8 ] [ C4/8 E4/8 G4/8 ])
{[ C4/4 E4/4 G4/4 ] [ D4/4 F#4/4 A4/4 ]}
([ C4/4 E4/4 G4/4 ] [ C4/4 E4/4 G4/4 ])""";
        
        final String expected = """
X: 1
M: 4/4
Q: 1/4=120
L: 1/1
K: C
([C/8E/8G/8][D/8^F/8A/8])[C/8E/8G/8]-[C/8E/8G/8] ([C/4 E/4 G/4] [D/4 ^F/4 A/4]) | [C/4 E/4 G/4]- [C/4 E/4 G/4] ||""";
        
        runTest(CHORD_NOTES, expected);
    }

    
    ////////////////////////////////////////////////////////////////////////////
    
    @Test
    void tripletQuarters() {
        final String TRIPLET_QUARTER_NOTES = """
G4/4 A4/4 B4/4,3 A4/4,3 G4/4,3 B4/4,3 A4/4,3 G4/4,3 B4/4,3 A4/4,3 G4/4,3 D4/1""";
        
        final String expected = """
X: 1
M: 4/4
Q: 1/4=120
L: 1/1
K: C
G/4 A/4 (3 B/4 A/4 G/4 | (3 B/4 A/4 G/4 (3 B/4 A/4 G/4 | D/1 ||""";
        
        runTest(TRIPLET_QUARTER_NOTES, expected);
    }
    
    
    ////////////////////////////////////////////////////////////////////////////
    
    @Test
    void tripletEighths() {
        final String TRIPLET_EIGHTH_NOTES = """
G4/8 A4/8 B4/8,3 A4/8,3 G4/8,3 B4/8,3 A4/8,3 G4/8,3 B4/8,3 A4/8,3 G4/8,3 D4/1""";
        
        final String expected2 = """
X: 1
M: 4/4
Q: 1/4=120
L: 1/1
K: C
G/8`A/8 (3 B/8`A/8`G/8 (3 B/8`A/8`G/8 (3 B/8`A/8`G/8 | D/1 ||""";
        
        runTest(TRIPLET_EIGHTH_NOTES, expected2);
    }
    
    
    ////////////////////////////////////////////////////////////////////////////
    
    @Test
    void tripletMixed() {
        final String MIXED_TRIPLET_NOTES = """
G4/4 A4/4 B4/2,3 A4/4,3 D4/1""";
        
        final String expected3 = """
X: 1
M: 4/4
Q: 1/4=120
L: 1/1
K: C
G/4 A/4 (3 B/2 A/4 | D/1 ||""";
        
        runTest(MIXED_TRIPLET_NOTES, expected3);
    }
    
    
    ////////////////////////////////////////////////////////////////////////////
    
    @Test
    void timeSignatureChange() {
        final String METER_CHANGE_NOTES = """
3/4
G4/4. A4/8 G4/8 F4/8 E4/4 C4/4 C4/4
4/4
D4/4 G3/4 G3/4 G3/4 E4/4 C4/4 C4/4 C4/4
3/4
D4/4 G3/4 G3/4  C4/2.""";
        
        final String expected = """
X: 1
M: 3/4
Q: 1/4=120
L: 1/1
K: C
G/4> A/4`G/8`F/8 | E/4 C/4 C/4 |\\
M:4/4
D/4 G,/4 G,/4 G,/4 | E/4 C/4 C/4 C/4 |
M:3/4
D/4 G,/4 G,/4 | C/2> ||""";
        // Backslash inside """ must be escaped, so there is a single backslash in fact.
        // The second meter-change line is not escaped because a new line is started after 4 bars
        
        runTest(METER_CHANGE_NOTES, expected);
    }
    
    
    ////////////////////////////////////////////////////////////////////////////
    
    @Test
    void multipleVoices() {
        // TODO
    }
    
    
    private void runTest(String testData, String expected) {
        runTest(testData, expected, null);
    }
    
    private void runTest(String testData, String expected, ExportToAbc.Configuration configuration) {
        runTest(testData, expected, new MelodyFactory(), null);
    }
    
    private void runTest(String testData, String expected, MelodyFactory melodyFactory, ExportToAbc.Configuration configuration) {
        final Note[][] notes = melodyFactory.translate(testData);
        
        final ExportToAbc exporter = new ExportToAbc(notes);
        final String abcText = exporter.export(configuration);
        
        //System.out.println(abcText);
        assertEquals(expected, abcText);
    }
}