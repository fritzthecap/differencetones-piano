package fri.music.player.notelanguage.abc;

import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.Test;

class AbcKeyAndAccidentalsMapTest
{
    @Test
    void accidentalFor_C_Ascending() {
        final AbcKeyAndAccidentalsMap map = new AbcKeyAndAccidentalsMap("C");
        assertEquals("C", map.getAdjustedNote("C4"));
        assertEquals("^C", map.getAdjustedNote("C#4"));
        assertEquals("D", map.getAdjustedNote("D4"));
        assertEquals("^D", map.getAdjustedNote("D#4"));
        assertEquals("E", map.getAdjustedNote("E4"));
        assertEquals("F", map.getAdjustedNote("F4"));
        assertEquals("^F", map.getAdjustedNote("F#4"));
        assertEquals("G", map.getAdjustedNote("G4"));
        assertEquals("^G", map.getAdjustedNote("G#4"));
        assertEquals("A", map.getAdjustedNote("A4"));
        assertEquals("^A", map.getAdjustedNote("A#4"));
        assertEquals("B", map.getAdjustedNote("B4"));
        assertEquals("c", map.getAdjustedNote("C5"));
    }

    @Test
    void accidentalFor_C_Descending() {
        final AbcKeyAndAccidentalsMap map = new AbcKeyAndAccidentalsMap("C");
        assertEquals("c", map.getAdjustedNote("C5"));
        assertEquals("B", map.getAdjustedNote("B4"));
        assertEquals("^A", map.getAdjustedNote("A#4"));
        assertEquals("=A", map.getAdjustedNote("A4"));
        assertEquals("^G", map.getAdjustedNote("G#4"));
        assertEquals("=G", map.getAdjustedNote("G4"));
        assertEquals("^F", map.getAdjustedNote("F#4"));
        assertEquals("=F", map.getAdjustedNote("F4"));
        assertEquals("E", map.getAdjustedNote("E4"));
        assertEquals("^D", map.getAdjustedNote("D#4"));
        assertEquals("=D", map.getAdjustedNote("D4"));
        assertEquals("^C", map.getAdjustedNote("C#4"));
        assertEquals("=C", map.getAdjustedNote("C4"));
    }

    @Test
    void accidentalFor_G_Ascending() {
        final AbcKeyAndAccidentalsMap map = new AbcKeyAndAccidentalsMap("G");
        assertEquals("G", map.getAdjustedNote("G4"));
        assertEquals("^G", map.getAdjustedNote("G#4"));
        assertEquals("A", map.getAdjustedNote("A4"));
        assertEquals("^A", map.getAdjustedNote("A#4"));
        assertEquals("B", map.getAdjustedNote("B4"));
        assertEquals("c", map.getAdjustedNote("C5"));
        assertEquals("^c", map.getAdjustedNote("C#5"));
        assertEquals("d", map.getAdjustedNote("D5"));
        assertEquals("^d", map.getAdjustedNote("D#5"));
        assertEquals("e", map.getAdjustedNote("E5"));
        assertEquals("=f", map.getAdjustedNote("F5"));
        assertEquals("^f", map.getAdjustedNote("F#5")); // "f" was resolved before, now gets accidental
        assertEquals("g", map.getAdjustedNote("G5"));
    }

    @Test
    void accidentalFor_F_Descending() {
        final AbcKeyAndAccidentalsMap map = new AbcKeyAndAccidentalsMap("F");
        assertEquals("f", map.getAdjustedNote("F5"));
        assertEquals("e", map.getAdjustedNote("E5"));
        assertEquals("_e", map.getAdjustedNote("D#5"));
        assertEquals("d", map.getAdjustedNote("D5"));
        assertEquals("_d", map.getAdjustedNote("C#5"));
        assertEquals("c", map.getAdjustedNote("C5"));
        assertEquals("=B", map.getAdjustedNote("B4"));
        assertEquals("_B", map.getAdjustedNote("A#4")); // "B" was resolved before, now gets accidental
        assertEquals("A", map.getAdjustedNote("A4"));
        assertEquals("_A", map.getAdjustedNote("G#4"));
        assertEquals("G", map.getAdjustedNote("G4"));
        assertEquals("_G", map.getAdjustedNote("F#4"));
        assertEquals("F", map.getAdjustedNote("F4"));
    }

    @Test
    void accidentalForSharpMajorKey() {
        final AbcKeyAndAccidentalsMap map = new AbcKeyAndAccidentalsMap("F#"); // 6 #
        assertMaxiumumAccidentalsForSharpKey(map);
    }

    @Test
    void accidentalForSharpMinorKey() {
        final AbcKeyAndAccidentalsMap map = new AbcKeyAndAccidentalsMap("D#m"); // 6 #
        assertMaxiumumAccidentalsForSharpKey(map);
    }

    @Test
    void accidentalForFlatMajorKey() {
        final AbcKeyAndAccidentalsMap map = new AbcKeyAndAccidentalsMap("Gb"); // 6 b
        assertMaxiumumAccidentalsForFlatKey(map);
    }

    @Test
    void accidentalForFlatMinorKey() {
        final AbcKeyAndAccidentalsMap map = new AbcKeyAndAccidentalsMap("Ebm"); // 6 b
        assertMaxiumumAccidentalsForFlatKey(map);
    }


    private void assertMaxiumumAccidentalsForSharpKey(AbcKeyAndAccidentalsMap map) {
        // non-sharps
        assertEquals("b", map.getAdjustedNote("B5"));
        // sharps
        assertEquals("f", map.getAdjustedNote("F#5"));
        assertEquals("c", map.getAdjustedNote("C#5"));
        assertEquals("g", map.getAdjustedNote("G#5"));
        assertEquals("d", map.getAdjustedNote("D#5"));
        assertEquals("a", map.getAdjustedNote("A#5"));
        assertEquals("e", map.getAdjustedNote("F5")); // e sharp
    }
    
    private void assertMaxiumumAccidentalsForFlatKey(AbcKeyAndAccidentalsMap map) {
        // non-flats
        assertEquals("f", map.getAdjustedNote("F5"));
        // flats
        assertEquals("b", map.getAdjustedNote("A#5"));
        assertEquals("e", map.getAdjustedNote("D#5"));
        assertEquals("a", map.getAdjustedNote("G#5"));
        assertEquals("d", map.getAdjustedNote("C#5"));
        assertEquals("g", map.getAdjustedNote("F#5"));
        assertEquals("c'", map.getAdjustedNote("B5")); // octave jump for c flat
    }
}