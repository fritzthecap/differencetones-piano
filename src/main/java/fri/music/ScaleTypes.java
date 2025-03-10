package fri.music;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.SequencedMap;

/**
 * The piano-layout of white and black keys for all 7 possible "modal" scales
 * of chromatic 12-tone scales, i.e. diatonic scales whose lowest note always
 * is a white piano key.
 */
public final class ScaleTypes
{
    /** Scale name. */
    public static final String IONIAN = "IONIAN";
    /** Scale name. */
    public static final String DORIAN = "DORIAN";
    /** Scale name. */
    public static final String PHRYGIAN = "PHRYGIAN";
    /** Scale name. */
    public static final String LYDIAN = "LYDIAN";
    /** Scale name. */
    public static final String MIXOLYDIAN = "MIXOLYDIAN";
    /** Scale name. */
    public static final String AEOLIAN = "AEOLIAN";
    /** Scale name. */
    public static final String LOCRIAN = "LOCRIAN";
    
    /**
     * Key is scale name (IONIAN, DORIAN, ...), value is a boolean array
     * that has true for a white piano key and false for a black one.
     */
    public static final SequencedMap<String,boolean[]> scaleToLayout;
    /**
     * Key is scale name (IONIAN, DORIAN, ...), value is the start note of the scale.
     * The map is in order of the notes C, D, E, F, G, A, B. No octave number is in the name.
     */
    public static final SequencedMap<String,String> scaleToStartNote;
    
    static {
        // boolean arrays telling PianoKey.isWhite true or false
        // every black key (augmented, sharp) gets a false, every white key a true, first is always true
        final SequencedMap<String,boolean[]> map1 = new LinkedHashMap<>(7);
        map1.put(IONIAN,       new boolean[] { true,false,true,false,true,true,false,true,false,true,false,true });
        map1.put(DORIAN,       new boolean[] { true,false,true,true,false,true,false,true,false,true,true,false });
        map1.put(PHRYGIAN,     new boolean[] { true,true,false,true,false,true,false,true,true,false,true,false });
        map1.put(LYDIAN,       new boolean[] { true,false,true,false,true,false,true,true,false,true,false,true });
        map1.put(MIXOLYDIAN,   new boolean[] { true,false,true,false,true,true,false,true,false,true,true,false });
        map1.put(AEOLIAN,      new boolean[] { true,false,true,true,false,true,false,true,true,false,true,false });
        map1.put(LOCRIAN,      new boolean[] { true,true,false,true,false,true,true,false,true,false,true,false });
        scaleToLayout = map1;
        
        // add scale base tones ordered, lowest first
        final SequencedMap<String,String> map2 = new LinkedHashMap<>(7); // keeps sort order
        map2.put(IONIAN, "C");
        map2.put(DORIAN, "D");
        map2.put(PHRYGIAN, "E");
        map2.put(LYDIAN, "F");
        map2.put(MIXOLYDIAN, "G");
        map2.put(AEOLIAN, "A");
        map2.put(LOCRIAN, "B");
        scaleToStartNote = map2;
    }
    
    /** @return 7 for 12-tone scales. */
    public static int numberOfWhiteKeysPerOctave() {
        return numberOfKeysPerOctave(true);
    }
    /** @return 5 for 12-tone scales. */
    public static int numberOfBlackKeysPerOctave() {
        return numberOfKeysPerOctave(false);
    }
    private static int numberOfKeysPerOctave(boolean isWhite) {
        int n = 0;
        for (boolean white : scaleToLayout.get(IONIAN))
            if (isWhite == white)
                n++;
        return n;
    }
    
    /**
     * @param lowestScaleToneIpnName optional, the lowest tone of requested scale, with or without octave number.
     *      e.g. "C4", must be upper-case and not contain any '#' or 'b', default for null is IONIAN.
     * @return the name of one of the the 7 scales that starts with given IPN-name.
     * @throws IllegalArgumentException when not found.
     */
    public static String scaleName(String lowestScaleToneIpnName) {
        if (lowestScaleToneIpnName == null)
            lowestScaleToneIpnName = "C";
        else if (lowestScaleToneIpnName.indexOf('#') > 0 || lowestScaleToneIpnName.indexOf('b') > 0) // no flat/sharp allowed
            throw new IllegalArgumentException("Augmented tones are not scale start tones: "+lowestScaleToneIpnName);
        
        for (Map.Entry<String,String> scaleEntry : scaleToStartNote.entrySet())
            if (lowestScaleToneIpnName.startsWith(scaleEntry.getValue()))
                return scaleEntry.getKey();

        throw new IllegalArgumentException("Given start note doesn't match any scale: "+lowestScaleToneIpnName);
    }

    /**
     * @param lowestScaleToneIpnName optional, the lowest tone of requested scale, with or without octave number.
     *      e.g. "C4", must be upper-case and not contain any '#' or 'b', default for null is IONIAN.
     * @param blackPositionInOctave the 0-4 position of requested black key within black key set of the scale.
     * @return the 1-7 number of white keys being before given black key's 0-4 position
     *      in one octave of the scale starting with given IPN-name.
     * @throws IllegalArgumentException when not found.
     */
    public static int numberOfWhiteKeysBelowBlackKey(String lowestScaleToneIpnName, int blackPositionInOctave) {
        final String scaleName = scaleName(lowestScaleToneIpnName);
        
        int whiteIndex = 0, blackIndex = 0;
        for (boolean keysIsWhite : scaleToLayout.get(scaleName))
            if (keysIsWhite == true)
                whiteIndex++;
            else if (blackIndex == blackPositionInOctave)
                return whiteIndex;
            else
                blackIndex++;
        
        throw new IllegalArgumentException("Number of white keys not found for black key index "+blackPositionInOctave);
    }
    
    /**
     * @param lowestScaleToneIpnName the key for which to find the semi-tone index based on an IONIAN scale
     * @return the 0-11 semi-tone index for given IPN-name which is the base note of a scale.
     */
    public static int cBasedSemitoneIndex(String lowestScaleToneIpnName) {
        final String scaleName = scaleName(lowestScaleToneIpnName);
        final String firstNoteOfScale = ScaleTypes.scaleToStartNote.get(scaleName); // "E" for PHRYGIAN
        final int firstNoteIndex = new ArrayList<>(ScaleTypes.scaleToStartNote.sequencedValues())
                .indexOf(firstNoteOfScale);
        
        int whiteIndex = 0, semitoneIndex = 0;
        for (boolean keyIsWhite : scaleToLayout.get(IONIAN)) { // IONIAN is C-based
            if (keyIsWhite == true) // lowest scale tone always is a white key
                if (whiteIndex == firstNoteIndex)
                    return semitoneIndex;
                else
                    whiteIndex++;
            semitoneIndex++;
        }
        throw new IllegalArgumentException("Semitone index for white key not found "+lowestScaleToneIpnName);
    }
    
    /**
     * @param lowestScaleToneIpnName optional, the lowest tone of requested scale, with or without octave number.
     * @param positionInOctave the requested key's 0-4 position for a black or 0-6 position for a white key.
     * @param isWhite true when this is about a white key, else false.
     * @return the 0-11 semi-tone index within the 5 black and 7 white keys of one octave.
     */
    public static int octaveBasedSemitoneIndex(String lowestScaleToneIpnName, int positionInOctave, boolean isWhite) {
        final String scaleName = scaleName(lowestScaleToneIpnName);
        
        int whiteIndex = 0, blackIndex = 0, semitoneIndex = 0;
        for (boolean keyIsWhite : scaleToLayout.get(scaleName)) {
            if (isWhite == true && keyIsWhite == true && whiteIndex == positionInOctave ||
                    isWhite == false && keyIsWhite == false && blackIndex == positionInOctave)
                return semitoneIndex;
            else if (keyIsWhite)
                whiteIndex++;
            else
                blackIndex++;
            semitoneIndex++;
        }
        throw new IllegalArgumentException("Semitone index not found!");
    }

    /**
     * @param lowestScaleToneIpnName optional, the lowest tone of requested scale, with or without octave number.
     * @param octaveOfScaleStart the 0-n number of the octave, but based on MIDI numbering that increments on "C".
     * @param semitoneIndexInOctave 0-11 index of the requested note.
     * @return the IPN name of the note for given parameters.
     */
    public static String ipnName(String lowestScaleToneIpnName, int octaveOfScaleStart, int semitoneIndexInOctave) {
        if (semitoneIndexInOctave >= ToneSystem.SEMITONES_PER_OCTAVE)
            throw new IllegalArgumentException("Index of semitone must be 0-11 but is "+semitoneIndexInOctave);
        
        final String scaleName = scaleName(lowestScaleToneIpnName);
        final String firstNoteOfScale = ScaleTypes.scaleToStartNote.get(scaleName); // "E" for PHRYGIAN
        
        final List<String> scaleStartNotes = new ArrayList<>(ScaleTypes.scaleToStartNote.sequencedValues());
        final int indexOfFirstNote = scaleStartNotes.indexOf(firstNoteOfScale);
        final int numberOfNotes = scaleStartNotes.size(); // always 7 whole tones
        
        final boolean[] isWhite = ScaleTypes.scaleToLayout.get(scaleName); // always 12 semitones
        String noteBaseName = "";
        int octaveCorrection = 0;
        for (int semitoneIndex = 0, whiteIndex = 0; semitoneIndex <= semitoneIndexInOctave; semitoneIndex++) {
            if (isWhite[semitoneIndex] == true) { // white key
                final int indexOfNote = (indexOfFirstNote + whiteIndex) % numberOfNotes;
                noteBaseName = scaleStartNotes.get(indexOfNote);
                whiteIndex++;
                
                if (semitoneIndex > 0 && noteBaseName.equals("C"))
                    octaveCorrection++;
            }
            else { // black key
                noteBaseName = noteBaseName+"#";
            }
        }
        final String octavePostfix = ""+(octaveOfScaleStart + octaveCorrection);
        return noteBaseName+octavePostfix;
    }
    
    private ScaleTypes() {} // do not instantiate
}