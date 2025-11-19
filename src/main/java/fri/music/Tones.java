package fri.music;

import java.util.Arrays;
import java.util.Hashtable;
import java.util.Map;

/**
 * Manages Tone arrays.
 * Tone systems can be defined for different tunings
 * like equally-tempered, well-tempered, ...
 * All tunings use the same tone names, as they must fit to a 
 * conventional piano keyboard with 12 keys per octave.
 * <p/>
 * Mind that the constructor of this class builds 3 maps
 * from given tones and thus is "expensive"!
 */
public class Tones
{
    public final Tone[] tones;
    
    private final Map<String,Tone> ipnNamesToTones;
    private final Map<Integer,Tone> midiNumbersToTones;
    private final Map<Double,Tone> frequenciesToTones;
    
    /**
     * Builds three maps for given tones.
     * @param tones the tones to manage.
     */
    public Tones(Tone[] tones) {
        this.tones = (tones == null) ? new EqualTemperament().tones() : tones;
        
        this.ipnNamesToTones = new Hashtable<>(this.tones.length);
        for (Tone tone : this.tones)
            ipnNamesToTones.put(tone.ipnName, tone);
        
        this.midiNumbersToTones = new Hashtable<>(this.tones.length);
        for (Tone tone : this.tones)
            midiNumbersToTones.put(tone.midiNumber, tone);
        
        this.frequenciesToTones = new Hashtable<>(this.tones.length);
        for (Tone tone : this.tones)
            frequenciesToTones.put(tone.frequency, tone);
    }
    
    /**
     * @param ipnName the IPN-name of the tone to search.
     * @return the index in tones array for given IPN-name, or -1 if not present.
     */
    public int indexOf(String ipnName) {
        final Tone tone = ipnNamesToTones.get(ipnName);
        if (tone == null)
            return -1;
        return Arrays.binarySearch(tones, tone); // Tone implements Comparable
    }
    
    /**
     * @param ipnName the IPN-name of the tone to search.
     * @return the Tone for given IPN-name, or null if not present.
     */
    public Tone forIpnName(String ipnName) {
        return ipnNamesToTones.get(ipnName);
    }
    
    /**
     * @param midiNoteNumber the MIDI note number of the tone to search.
     * @return the Tone for given MIDI note number, or null if not present.
     */
    public Tone forMidiNoteNumber(int midiNoteNumber) {
        return midiNumbersToTones.get(midiNoteNumber);
    }
    
    /**
     * @param frequency the frequency of the tone to search.
     * @return the Tone for given frequency, or null if not present.
     */
    public Tone forFrequency(double frequency) {
        return frequenciesToTones.get(frequency);
    }

    /**
     * Fast binary search for given frequency.
     * The tones array must be sorted by lowest frequency first and highest last.
     * @return the tones enclosing given frequency, never null. 
     *      If Tone[0] == Tone[1], then the frequency matched exactly.
     *      If Tone[0] is not null and Tone[1] is null,
     *      the given frequency is above highest available tone,
     *      in this case Tone[0] is the highest available tone.
     *      If Tone[0] is null and Tone[1] is not null,
     *      the given frequency is below lowest available tone,
     *      in this case Tone[1] is the lowest available tone.
     */
    public Tone[] getEnclosingTones(double frequency) {
        int low = 0;
        int high = tones.length - 1;
        
        if (tones[low].frequency > frequency) // out of tone range on bottom
            return new Tone[] { null, tones[low] };
        
        if (tones[high].frequency < frequency) // out of tone range on top
            return new Tone[] { tones[high], null };
        
        while (high > low + 1) {
            final int mid = low + (high - low) / 2;
            final  Tone tone = tones[mid];
            if (tone.frequency < frequency)
                low = mid;
            else if (tone.frequency > frequency)
                high = mid;
            else // found frequency exactly
                return new Tone[] { tone, tone };
        }
        
        return new Tone[] { tones[low], tones[high] };
    }
    
    public Tone getHighest() {
        return tones[tones.length - 1];
    }
    
    public Tone getLowest() {
        return tones[0];
    }
    
    public Tone getNextUpper(Tone tone) {
        return forMidiNoteNumber(tone.midiNumber + 1);
    }

    public Tone getNextLower(Tone tone) {
        return forMidiNoteNumber(tone.midiNumber - 1);
    }
    
    public int getOctaves() {
        return tones.length / ToneSystem.SEMITONES_PER_OCTAVE;
    }

    public int getHighestOctave() {
        return getHighest().ipnOctave;
    }

    public int getLowestOctave() {
        return getLowest().ipnOctave;
    }
    
    @Override
    public String toString() {
        final Tone first = getLowest();
        final Tone last  = getHighest();
        final int octaves = getOctaves();
        return getClass().getSimpleName()+", "+octaves+" octaves, range "+
                first.ipnName+"("+first.midiNumber+") - "+last.ipnName+"("+last.midiNumber+")";
    }
}