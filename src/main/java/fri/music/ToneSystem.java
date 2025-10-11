package fri.music;

import java.util.stream.IntStream;
import fri.music.utils.StringUtil;

/**
 * Common usage of equal-temperament or just-intonation 12 tone systems.
 * A concrete instance should have at least a lowest base-tone and a 
 * limited set of octaves given in constructor, and optionally the frequency of A4.
 * IPN = international pitch notation.
 */
public interface ToneSystem
{
    /** The default frequency of "A4", can be modified through constructor arguments. */
    double DEFAULT_REFERENCE_FREQUENCY = 440.0;
    
    /** "A4", the (immutable) IPN-name of the tone that all pitches will be calculated from. */
    String REFERENCE_FREQUENCY_IPN_NAME = "A4";
    
    /** The 12 base-names of tones (without octave number), starting with "C", ending with "B", sharp accent is "#". */
    String[] IPN_BASE_NAMES = new String[] 
            { "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B" };
    
    /** The IPN character used to raise a note by a semi-tone. */
    char SHARP_CHAR = '#';

    /** The symbol to be used for a rest. */
    String REST_SYMBOL = "-";
    
    /** Twelve-tone scales length is 12. */
    int SEMITONES_PER_OCTAVE = IPN_BASE_NAMES.length;
    
    /** The lowest octave number for C-based IPN note names. */
    int LOWEST_OCTAVE = 0;
    
    /** The lowest tone's IPN-name is "C0" (highest is "C10"). */
    String DEFAULT_BASETONE_IPN_NAME = IPN_BASE_NAMES[0]+LOWEST_OCTAVE;
    /** The lowest tone's MIDI-number, highest is 132. */
    int DEFAULT_BASETONE_MIDI_NUMBER = 12;
    
    /** Default number of octaves from DEFAULT_LOWEST_IPN_NAME, so the range will be from "C0" to "C10" (including). */
    int MAXIMUM_OCTAVES = 10;
    
    /** Audible limit for "C0", although even 20 Hertz is still hard to hear. */
    double MINIMUM_FREQUENCY = 16.0;
    /** Audible limit (CD-quality) would be "E10" inclusive. */
    double MAXIMUM_FREQUENCY = 22050.0;
    

    String UNISON = "UNISON";
    String MINOR_SECOND = "MINOR_SECOND";
    String MAJOR_SECOND = "MAJOR_SECOND"; 
    String MINOR_THIRD = "MINOR_THIRD";
    String MAJOR_THIRD = "MAJOR_THIRD";
    String FOURTH = "FOURTH";
    String TRITONE = "TRITONE";
    String FIFTH = "FIFTH";
    String MINOR_SIXTH = "MINOR_SIXTH";
    String MAJOR_SIXTH = "MAJOR_SIXTH";
    String MINOR_SEVENTH = "MINOR_SEVENTH";
    String MAJOR_SEVENTH = "MAJOR_SEVENTH";
    String OCTAVE = "OCTAVE";
    
    String[] INTERVAL_NAMES = new String[] {
        UNISON, MINOR_SECOND, MAJOR_SECOND, MINOR_THIRD, MAJOR_THIRD, FOURTH, TRITONE, 
        FIFTH, MINOR_SIXTH, MAJOR_SIXTH, MINOR_SEVENTH, MAJOR_SEVENTH, OCTAVE
    };
    
    
    /** @return the name of this tone-system. */
    String name();
    
    /** @return the actual frequency of the reference-note "A4" (REFERENCE_FREQUENCY_IPN_NAME), to be overridden. */
    double referenceFrequency();
    
    /** @return the number of octaves of this tone-system. */
    int octaves();
    
    /** @return the actual lowest note of this tone-system, for just-intonation the note the scale was built upon. */
    String baseToneIpnName();
    
    /** @return all tones of requested octaves in this tone-system. */
    Tone[] tones();
    
    
    /**
     * Logical names for the intervals of a 12-tone scale.
     * @param semitoneSteps the number of half-tones to describe the interval.
     * @return an interval name for given 0-12 number of semitones.
     */
    static String intervalName(int semitoneSteps)  {
        switch (semitoneSteps) {
            case 0: return UNISON;
            case 1: return MINOR_SECOND;
            case 2: return MAJOR_SECOND; 
            case 3: return MINOR_THIRD;
            case 4: return MAJOR_THIRD;
            case 5: return FOURTH;
            case 6: return TRITONE;
            case 7: return FIFTH;
            case 8: return MINOR_SIXTH;
            case 9: return MAJOR_SIXTH;
            case 10: return MINOR_SEVENTH;
            case 11: return MAJOR_SEVENTH;
            case 12: return OCTAVE;
            default: throw new IllegalArgumentException("Interval for "+semitoneSteps+" semitone-steps not implemented!");
        }
    }
    
    /**
     * Logical names for the intervals of a 12-tone scale.
     * @param semitoneSteps the number of half-tones to describe the interval.
     * @return an interval name for given 0-12 number of semitones.
     */
    static int semitoneSteps(String intervalName)  {
        switch (intervalName) {
            case UNISON: return 0;
            case MINOR_SECOND: return 1;
            case MAJOR_SECOND: return 2; 
            case MINOR_THIRD: return 3;
            case MAJOR_THIRD: return 4;
            case FOURTH: return 5;
            case TRITONE: return 6;
            case FIFTH: return 7;
            case MINOR_SIXTH: return 8;
            case MAJOR_SIXTH: return 9;
            case MINOR_SEVENTH: return 10;
            case MAJOR_SEVENTH: return 11;
            case OCTAVE: return 12;
            default: throw new IllegalArgumentException("Semitone steps for interval '"+intervalName+"' not implemented!");
        }
    }
    
    /** Converts a MIDI tone number to an IPN-name with octave number. */
    static String midiNumberToIpnName(int midiNumber) {
        final int midiOffsetFromC0 = midiNumber - DEFAULT_BASETONE_MIDI_NUMBER;
        if (midiOffsetFromC0 < 0)
            throw new IllegalArgumentException("MIDI number too small: "+midiNumber);
        
        final int octaveNumber = midiOffsetFromC0 / SEMITONES_PER_OCTAVE;
        final int octaveRemainder = midiOffsetFromC0 % SEMITONES_PER_OCTAVE;
        
        return IPN_BASE_NAMES[octaveRemainder] + octaveNumber;
    }
    
    /** Converts an IPN-name with octave number to a MIDI tone number. */
    static int ipnNameToMidiNumber(String ipnName) {
        final int octave = StringUtil.getFirstNumber(ipnName);
        final String ipnBaseName = StringUtil.getUntilFirstNumber(ipnName);
        
        final int ipnBaseNameIndex = IntStream.range(0, IPN_BASE_NAMES.length)
                .filter(i -> IPN_BASE_NAMES[i].equals(ipnBaseName))
                .findFirst()
                .orElseThrow(() -> new IllegalArgumentException("Unknown IPN-name: "+ipnName));
        
        return DEFAULT_BASETONE_MIDI_NUMBER + (octave * ToneSystem.SEMITONES_PER_OCTAVE) + ipnBaseNameIndex;
    }
}