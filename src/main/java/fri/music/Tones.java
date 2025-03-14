package fri.music;

import java.util.Hashtable;
import java.util.Map;

/**
 * Manages Tone arrays.
 * Tone systems can be defined for different tunings
 * like equally-tempered, well-tempered, ...
 * All tunings use the same tone names, as they must fit to a 
 * conventional piano keyboard with 12 keys per octave.
 * <p/>
 * Mind that the constructor of this class builds hashtables
 * from tones and thus is "expensive"!
 */
public class Tones
{
    public final Tone[] tones;
    
    private final Map<String,Tone> ipnNamesToTones;
    private final Map<Integer,Tone> midiNumbersToTones;
    private final Map<Double,Tone> frequenciesToTones;
    
    public Tones() {
        this(null);
    }
    
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

    /** Binary search for given frequency */
    public Tone[] getEnclosingTones(double frequency) {
        int low = 0;
        int high = tones.length - 1;
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
        
        if (tones[low].frequency > frequency || tones[high].frequency < frequency)
            return new Tone[] { null, null };
        
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
    
    @Override
    public String toString() {
        final Tone first = getLowest();
        final Tone last  = getHighest();
        final int octaves = getOctaves();
        return getClass().getSimpleName()+", "+octaves+" octaves, range "+
                first.ipnName+"("+first.midiNumber+") - "+last.ipnName+"("+last.midiNumber+")";
    }
}