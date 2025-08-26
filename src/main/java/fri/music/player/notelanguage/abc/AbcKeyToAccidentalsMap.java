package fri.music.player.notelanguage.abc;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import fri.music.TextUtil;
import fri.music.ToneSystem;

/**
 * Manages the accidentals (#, b) of notes in ABC notation
 * according to a key (C, Bb, Hm, ...).
 * <pre>
     IPN = ABC note-name and octave
     C0 = C,,,, 
     ...
     C2 = C,,
     C3 = C,
     C4 = C D E F G A B 
     C5 = c d e f g a b
     C6 = c'
     C7 = c''
     ...
     C9 = c''''
     C10 = c'''''
 * </pre>
 */
public class AbcKeyToAccidentalsMap
{
    private static final String ABC_NATURAL = "="; // resolution of some preceding accidental
    private static final String ABC_SHARP = "^";
    private static final String ABC_FLAT = "_";

    private static final Map<String,String> IPN_NOTE_TO_ABC_FLAT = new HashMap<>();
    private static final Map<String,String> IPN_NOTE_TO_ABC_SHARP = new HashMap<>();
    
    static {
        buildIpnToAbcNameMapping();
    }
    
    private static void buildIpnToAbcNameMapping() {
        final String ABC_REST = "z";
        IPN_NOTE_TO_ABC_SHARP.put(ToneSystem.REST_SYMBOL, ABC_REST);
        IPN_NOTE_TO_ABC_FLAT.put(ToneSystem.REST_SYMBOL, ABC_REST);
        
        for (int octave = ToneSystem.LOWEST_OCTAVE; octave < ToneSystem.MAXIMUM_OCTAVES; octave++) {
            if (octave < 0)
                throw new IllegalArgumentException("Can not process octave < 0!");
            
            for (int i = 0; i < ToneSystem.IPN_BASE_NAMES.length; i++) {
                final String ipnName = ToneSystem.IPN_BASE_NAMES[i];
                final String ipnNameWithoutAccent = getIpnNameWithoutAccent(ipnName);
                
                final String abcOctave = getAbcOctave(octave);
                final String abcName = toAbcName(ipnNameWithoutAccent, octave);
                final String abcNameWithOctave = abcName + abcOctave;
                
                final boolean accented = (ToneSystem.IPN_BASE_NAMES[i].length() != ipnNameWithoutAccent.length());
                final String ipnNameWithOctave = ipnName + octave;
                if (accented) {
                    IPN_NOTE_TO_ABC_SHARP.put(ipnNameWithOctave, ABC_SHARP + abcNameWithOctave);
                    
                    if (i < ToneSystem.IPN_BASE_NAMES.length - 1) { // not yet at "B"
                        final String ipnForFlat = getIpnNameWithoutAccent(ToneSystem.IPN_BASE_NAMES[i + 1]); // "B"
                        final String abcForFlat = toAbcName(ipnForFlat, octave);
                        final String abcNameWithOctaveFlat = ABC_FLAT + abcForFlat + abcOctave;
                        IPN_NOTE_TO_ABC_FLAT.put(ipnNameWithOctave, abcNameWithOctaveFlat);
                    }
                }
                else {
                    IPN_NOTE_TO_ABC_SHARP.put(ipnNameWithOctave, abcNameWithOctave);
                    IPN_NOTE_TO_ABC_FLAT.put(ipnNameWithOctave, abcNameWithOctave);
                }
            }
        }
    }
    
    private static String getIpnNameWithoutAccent(String ipnName) {
        if (ipnName.charAt(ipnName.length() - 1) == ToneSystem.SHARP_CHAR)
            return ipnName.substring(0, ipnName.length() - 1);
        return ipnName;
    }
    
    private static String toAbcName(String ipnNameWithoutAccent, int octave) {
        if (octave < 5)
            return ipnNameWithoutAccent; // already in upper case
        return ipnNameWithoutAccent.toLowerCase();
    }
    
    private static String getAbcOctave(int octave) {
        if (octave < 4)
            return ",".repeat(4 - octave); // ABC octave down symbol
        else if (octave > 5)
            return "'".repeat(octave - 5); // ABC octave up symbol
        return "";
    }
    

    private static final Map<String,List<String>> KEY_TO_NOTES_NOT_IN_KEY = new HashMap<>();
    private static final Map<String,List<String>> KEY_TO_NOTES_WITH_ACCIDENTALS = new HashMap<>();
    
    static {
        buildKeyToNotesNotInKeyMapping();
    }
    
    private static void buildKeyToNotesNotInKeyMapping() {
        final String[] POSSIBLE_SHARP_KEYS =       new String[] { "C",  "G",  "D",  "A",   "E",   "B",   "F#" };
        final String[] POSSIBLE_MINOR_SHARP_KEYS = new String[] { "Am", "Em", "Bm", "F#m", "C#m", "G#m", "D#m" };
        final String[] POSSIBLE_FLAT_KEYS =        new String[] { "F",  "Bb", "Eb", "Ab",  "Db",  "Gb" };
        final String[] POSSIBLE_MINOR_FLAT_KEYS =  new String[] { "Dm", "Gm", "Cm", "Fm",  "Bbm", "Ebm" };
        
        final String[] ALL_POSSIBLE_KEYS = new String[
                POSSIBLE_SHARP_KEYS.length + 
                POSSIBLE_MINOR_SHARP_KEYS.length + 
                POSSIBLE_FLAT_KEYS.length + 
                POSSIBLE_MINOR_FLAT_KEYS.length];
        
        System.arraycopy(
                POSSIBLE_SHARP_KEYS, 0, 
                ALL_POSSIBLE_KEYS, 0, 
                POSSIBLE_SHARP_KEYS.length);
        System.arraycopy(
                POSSIBLE_MINOR_SHARP_KEYS, 0, 
                ALL_POSSIBLE_KEYS, POSSIBLE_SHARP_KEYS.length, 
                POSSIBLE_MINOR_SHARP_KEYS.length);
        System.arraycopy(
                POSSIBLE_FLAT_KEYS, 0, 
                ALL_POSSIBLE_KEYS, POSSIBLE_SHARP_KEYS.length + POSSIBLE_MINOR_SHARP_KEYS.length, 
                POSSIBLE_FLAT_KEYS.length);
        System.arraycopy(
                POSSIBLE_MINOR_FLAT_KEYS, 0, 
                ALL_POSSIBLE_KEYS, POSSIBLE_SHARP_KEYS.length + POSSIBLE_MINOR_SHARP_KEYS.length + POSSIBLE_FLAT_KEYS.length, 
                POSSIBLE_MINOR_FLAT_KEYS.length);
        
        for (final String key : ALL_POSSIBLE_KEYS) {
            if (key.equals("C") || key.equals("Am"))
                KEY_TO_NOTES_NOT_IN_KEY.put(key, buildList("C#", "D#", "F#", "G#", "A#"));
            else if (key.equals("G") || key.equals("Em"))
                KEY_TO_NOTES_NOT_IN_KEY.put(key, buildList("C#", "D#", "F", "G#", "A#"));
            else if (key.equals("D") || key.equals("Bm"))
                KEY_TO_NOTES_NOT_IN_KEY.put(key, buildList("C", "D#", "F", "G#", "A#"));
            else if (key.equals("A") || key.equals("F#m"))
                KEY_TO_NOTES_NOT_IN_KEY.put(key, buildList("C", "D#", "F", "G", "A#"));
            else if (key.equals("E") || key.equals("C#m"))
                KEY_TO_NOTES_NOT_IN_KEY.put(key, buildList("C", "D", "F", "G", "A#"));
            else if (key.equals("B") || key.equals("G#m"))
                KEY_TO_NOTES_NOT_IN_KEY.put(key, buildList("C", "D", "F", "G", "A"));
            else if (key.equals("F#") || key.equals("D#m") || key.equals("Gb") || key.equals("Ebm"))
                KEY_TO_NOTES_NOT_IN_KEY.put(key, buildList("C", "D", "E", "G", "A"));
            else if (key.equals("Db") || key.equals("Bbm"))
                KEY_TO_NOTES_NOT_IN_KEY.put(key, buildList("B", "D", "E", "G", "A"));
            else if (key.equals("Ab") || key.equals("Fm"))
                KEY_TO_NOTES_NOT_IN_KEY.put(key, buildList("B", "D", "E", "F#", "A"));
            else if (key.equals("Eb") || key.equals("Cm"))
                KEY_TO_NOTES_NOT_IN_KEY.put(key, buildList("B", "C#", "E", "F#", "A"));
            else if (key.equals("Bb") || key.equals("Gm"))
                KEY_TO_NOTES_NOT_IN_KEY.put(key, buildList("B", "C#", "E", "F#", "G#"));
            else if (key.equals("F") || key.equals("Dm"))
                KEY_TO_NOTES_NOT_IN_KEY.put(key, buildList("B", "C#", "D#", "F#", "G#"));
            else
                throw new IllegalArgumentException("Can not build KEY_TO_NOTES_NOT_IN_KEY with unknown key: "+key);
        }
        
        for (final String key : ALL_POSSIBLE_KEYS) {
            if (key.equals("C") || key.equals("Am"))
                KEY_TO_NOTES_WITH_ACCIDENTALS.put(key, buildList());
            else if (key.equals("G") || key.equals("Em"))
                KEY_TO_NOTES_WITH_ACCIDENTALS.put(key, buildList("F"));
            else if (key.equals("D") || key.equals("Bm"))
                KEY_TO_NOTES_WITH_ACCIDENTALS.put(key, buildList("F", "C"));
            else if (key.equals("A") || key.equals("F#m"))
                KEY_TO_NOTES_WITH_ACCIDENTALS.put(key, buildList("F", "C", "G"));
            else if (key.equals("E") || key.equals("C#m"))
                KEY_TO_NOTES_WITH_ACCIDENTALS.put(key, buildList("F", "C", "G", "D"));
            else if (key.equals("B") || key.equals("G#m"))
                KEY_TO_NOTES_WITH_ACCIDENTALS.put(key, buildList("F", "C", "G", "D", "A"));
            else if (key.equals("F#") || key.equals("D#m"))
                KEY_TO_NOTES_WITH_ACCIDENTALS.put(key, buildList("F", "C", "G", "D", "A", "E"));
            else if (key.equals("Gb") || key.equals("Ebm"))
                KEY_TO_NOTES_WITH_ACCIDENTALS.put(key, buildList("B", "E", "A", "D", "G", "C"));
            else if (key.equals("Db") || key.equals("Bbm"))
                KEY_TO_NOTES_WITH_ACCIDENTALS.put(key, buildList("B", "E", "A", "D", "G"));
            else if (key.equals("Ab") || key.equals("Fm"))
                KEY_TO_NOTES_WITH_ACCIDENTALS.put(key, buildList("B", "E", "A", "D"));
            else if (key.equals("Eb") || key.equals("Cm"))
                KEY_TO_NOTES_WITH_ACCIDENTALS.put(key, buildList("B", "E", "A"));
            else if (key.equals("Bb") || key.equals("Gm"))
                KEY_TO_NOTES_WITH_ACCIDENTALS.put(key, buildList("B", "E"));
            else if (key.equals("F") || key.equals("Dm"))
                KEY_TO_NOTES_WITH_ACCIDENTALS.put(key, buildList("B"));
            else
                throw new IllegalArgumentException("Can not build KEY_TO_NOTES_WITH_ACCIDENTALS with unknown key: "+key);
        }
    }
    
    private static List<String> buildList(String... ipnNames) {
        final List<String> list = new ArrayList<>(ipnNames.length);
        for (String ipnName : ipnNames)
            list.add(ipnName);
        return list;
    }
    
    /**
     * Get accidentals for any keyOfTune from KEY_TO_NOTES_WITH_ACCIDENTALS: 
     * when isFlatKey is false, returns "G#" for "G", else "F#" for "G".
     * Thus it searches the next semi-tone in different directions.
     */
    private String skipNote(boolean isFlatKey, String ipnBaseName) {
        for (int i = 0; i < ToneSystem.IPN_BASE_NAMES.length; i++) {
            if (ToneSystem.IPN_BASE_NAMES[i].equals(ipnBaseName)) {
                int skipIndex = isFlatKey ? (i - 1) : (i + 1);
                if (skipIndex < 0)
                    skipIndex = ToneSystem.IPN_BASE_NAMES.length - 1;
                else if (skipIndex >= ToneSystem.IPN_BASE_NAMES.length)
                    skipIndex = 0;
                return ToneSystem.IPN_BASE_NAMES[skipIndex];
            }
        }
        throw new IllegalArgumentException("Can not skip '"+ipnBaseName+"' for finding accidentals of "+(isFlatKey ? "flat" : "sharp")+" key");
    }

    
    /**
     * Key of tune can be base tone with scale-type: C, Hm, Gb, Bbm, ...
     */
    private final String keyOfTune;
    /**
     * Flat keys are F, Bb, Eb, Ab, Db, Gb, they get written
     * with b accidentals rather than with #.
     */
    private final boolean isFlatKey;
    /**
     * List that contains note names that must be marked with 
     * # or b accidentals according to given keyOfTune.
     * In other words, they are not contained in the 12-tone scale of given keyOfTune.
     */
    private final List<String> ipnNamesNotInScale;
    /**
     */
    private final List<String> namesThatWouldGetAccidentals, accidentalsDefinedByKey;
    
    /**
     * List of notes (per bar) that have been marked with accidentals.
     * Any follower (in bar) might need an accidental-resolve.
     */
    private final Set<String> precedingNotInScaleWithAccidental = new HashSet<>();
    private final Set<String> precedingNotInScaleResolved = new HashSet<>();
    
    public AbcKeyToAccidentalsMap(AbcExport.Configuration configuration) {
        this.keyOfTune = configuration.getKey();
        this.isFlatKey = configuration.isFlatKey();
        this.ipnNamesNotInScale = KEY_TO_NOTES_NOT_IN_KEY.get(keyOfTune);
        this.namesThatWouldGetAccidentals = KEY_TO_NOTES_WITH_ACCIDENTALS.get(keyOfTune);
        this.accidentalsDefinedByKey = namesThatWouldGetAccidentals.stream()
                .map(name -> skipNote(isFlatKey, name))
                .toList();
    }
    
    /** To be called on bar start. Resets running accidental correction-list. */
    public void barStart() {
        precedingNotInScaleWithAccidental.clear();
        precedingNotInScaleResolved.clear();
    }

    public String getAdjustedNote(String ipnNoteName) {
        final String absoluteAbcName = getAbsolutelyMappedAbcNote(ipnNoteName); // accidentals set by keyOfTune not considered yet
        final boolean hasAccidental = hasAccidental(absoluteAbcName);
        String abcName = absoluteAbcName;
        
        if (isIn(ipnNamesNotInScale, ipnNoteName)) { // is NOT in scale, e.g. F# in key of C, or F in Key of D
            if (hasAccidental) {
                final boolean alreadyAdded = precedingNotInScaleWithAccidental.contains(ipnNoteName);
                if (alreadyAdded == false)
                    precedingNotInScaleWithAccidental.add(ipnNoteName); // remember it for removing accidental from followers
                else // remove accidental from follower
                    abcName = removeAccidental(absoluteAbcName);
            }
            else if (wouldBeAlteredByKeyOfTune(ipnNoteName)) { // B in key of F would be Bb, or F in key of G would be F#
                final boolean alreadyAdded = precedingNotInScaleResolved.contains(ipnNoteName);
                if (alreadyAdded == false) {
                    precedingNotInScaleResolved.add(ipnNoteName); // remember it for sparing followers
                    abcName = ABC_NATURAL + absoluteAbcName; // add accidental-resolver to first
                }
            }
        }
        else { // IS in scale of keyOfTune, but may carry redundant accidental
            String adjusted = null;
            if (hasAccidental) { // HAS accidental, check if there was a preceding resolve
                final Iterator<String> iterator = precedingNotInScaleResolved.iterator();
                final boolean searchUpwards = (isFlatKey == false);
                while (adjusted == null && iterator.hasNext()) {
                    if (isOnSameNoteline(iterator.next(), ipnNoteName, searchUpwards)) {
                        adjusted = absoluteAbcName; // leave its accidental
                        iterator.remove(); // accidental is active again, remove preceding resolve
                    }
                }
                
                if (adjusted == null && accidentalIsDefinedByKeyOfTune(ipnNoteName))
                    adjusted = removeAccidental(absoluteAbcName);
            }
            else { // has NO accidental, check if there was an accidental before
                final Iterator<String> iterator = precedingNotInScaleWithAccidental.iterator();
                final boolean searchUpwards = (isFlatKey == true);
                while (adjusted == null && iterator.hasNext()) {
                    if (isOnSameNoteline(iterator.next(), ipnNoteName, searchUpwards)) {
                        adjusted = resolveAccidental(absoluteAbcName);
                        iterator.remove(); // accidental is resolved, remove predecessor carrying accidental
                    }
                }
            }
            
            if (adjusted != null)
                abcName = adjusted;
        }
        
        return abcName;
    }

    private String getAbsolutelyMappedAbcNote(String ipnNoteName) {
        final String abcNoteName;
        if (isFlatKey)
            abcNoteName = IPN_NOTE_TO_ABC_FLAT.get(ipnNoteName);
        else
            abcNoteName = IPN_NOTE_TO_ABC_SHARP.get(ipnNoteName);
        
        if (abcNoteName == null)
            throw new IllegalArgumentException("No ABC mapping found for IPN-name '"+ipnNoteName+"'");
        
        return abcNoteName;
    }

    private boolean wouldBeAlteredByKeyOfTune(String ipnNoteName) {
        return isIn(namesThatWouldGetAccidentals, ipnNoteName);
    }
    
    private boolean accidentalIsDefinedByKeyOfTune(String ipnNoteName) {
        return isIn(accidentalsDefinedByKey, ipnNoteName);
    }
    
    /** Searches an IPN name like "F#4" in a list with names like "F", "F#", .... (comparison without octave). */
    private boolean isIn(List<String> names, String name) {
        final String nameWithoutOctave = TextUtil.getWithoutFirstNumber(name);
        return names.contains(nameWithoutOctave);
    }

    /** @return true if their octaves are the same and the given notes would be on same note line according to isFlatKey. */
    private boolean isOnSameNoteline(String precedingIpnNameNotInScale, String ipnNameInScale, boolean searchUpwards) {
        // check for same octave
        if (TextUtil.getFirstNumber(precedingIpnNameNotInScale) != TextUtil.getFirstNumber(ipnNameInScale))
            return false;
        
        // match would be preceding G#, current G, in sharp key like D,
        // or preceding Gb (as F#), current G, in flat key like Eb
        final String precedingIpnBaseName = TextUtil.getWithoutFirstNumber(precedingIpnNameNotInScale);
        final String[] ipnBaseNames = ToneSystem.IPN_BASE_NAMES;
        int index = 0;
        while (index < ipnBaseNames.length && ipnBaseNames[index].equals(precedingIpnBaseName) == false)
            index++;
        
        if (index >= ipnBaseNames.length)
            throw new IllegalArgumentException("Can not find IPN name: "+precedingIpnBaseName);
        
        index += (searchUpwards ? +1 : -1); // if preceding was flattened, go one upwards, else one downwards
        if (index < 0) // was counted to negative, go to end
            index = ipnBaseNames.length - 1;
        
        final String ipnNameBaseName = TextUtil.getWithoutFirstNumber(ipnNameInScale);
        
        return ipnBaseNames[index % ipnBaseNames.length].equals(ipnNameBaseName);
    }
    
    private boolean hasAccidental(String abcNoteName) {
        return abcNoteName.startsWith(ABC_SHARP) || abcNoteName.startsWith(ABC_FLAT);
    }
    
    /** Removes any accidental and sets ABC_NATURAL. */
    private String resolveAccidental(String abcNoteName) {
        return ABC_NATURAL + removeAccidental(abcNoteName);
    }
    
    /** Removes any accidental. */
    private String removeAccidental(String abcNoteName) {
        if (isFlatKey == false && abcNoteName.startsWith(ABC_SHARP))
            return abcNoteName.substring(ABC_SHARP.length());
        
        if (isFlatKey == true && abcNoteName.startsWith(ABC_FLAT))
            return abcNoteName.substring(ABC_FLAT.length());
        
        return abcNoteName;
    }
}