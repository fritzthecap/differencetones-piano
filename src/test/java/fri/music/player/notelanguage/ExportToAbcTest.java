package fri.music.player.notelanguage;

import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.Test;
import fri.music.player.Note;

class ExportToAbcTest
{
    private static final String AUGUSTIN = """
3/4
G4/4. A4/8 G4/8 F4/8 E4/4 C4/4 C4/4
D4/4 G3/4 G3/4 E4/4 C4/4 C4/4
G4/4. A4/8 G4/8 F4/8 E4/4 C4/4 C4/4
D4/4 G3/4 G3/4 C4/2.
D4/4 G3/4 G3/4 E4/4 C4/4 C4/4
D4/4 G3/4 G3/4 E4/4 C4/4 C4/4
G4/4. A4/8 G4/8 F4/8 E4/4 C4/4 C4/4
D4/4 G3/4 G3/4 C4/2.
""";
    
    @Test
    void singleNotesShouldWork() {
        final MelodyFactory melodyFactory = new MelodyFactory(144, 3, 4);
        final Note[][] notes = melodyFactory.translate(AUGUSTIN);
        
        final ExportToAbc exporter = new ExportToAbc(notes);
        final ExportToAbc.Configuration configuration = new ExportToAbc.Configuration(
                "Oh du lieber Augustin",
                "Wiener Lied aus der Pestzeit",
                "C", // key
                "Marx Augustin", // author
                "17 Jahrhundert", // date
                4); // bars per line
        final String abcText = exporter.export(configuration);
        
        //System.out.println(abcText);
        final String expected = """
X: 1
T: Oh du lieber Augustin
T: Wiener Lied aus der Pestzeit
C: Marx Augustin, 17 Jahrhundert
M: 3/4
Q: 1/4=144
L: 1/1
K: C
G/4> A/4`G/8`F/8`| E/4 C/4 C/4 | D/4 G,/4 G,/4 | E/4 C/4 C/4 |
G/4> A/4`G/8`F/8`| E/4 C/4 C/4 | D/4 G,/4 G,/4 | C/2> |
D/2 G,/4 G,/4 | E/4 C/4 C/4 | D/4 G,/4 G,/4 | E/4 C/4 C/4 |
G/4> A/4`G/8`F/8`| E/4 C/4 C/4 | D/4 G,/4 G,/4 | C/2> ||""";
        
        assertEquals(expected, abcText);
    }
    
    @Test
    void chordsShouldWork() {
        // TODO
    }

    @Test
    void tripletNotesShouldWork() {
        // TODO
    }
    
    @Test
    void barChangeShouldWork() {
        // TODO
    }
    
    @Test
    void multipleVoicesShouldWork() {
        // TODO
    }
}