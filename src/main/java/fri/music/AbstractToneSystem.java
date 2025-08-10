package fri.music;

import java.util.Arrays;
import java.util.Hashtable;
import java.util.Map;
import java.util.stream.IntStream;

/**
 * Common implementations for all ToneSystem classes.
 */
public abstract class AbstractToneSystem implements ToneSystem
{
    /** The global cache for the fully built tone-arrays. */
    public static final Map<Object,Tone[]> tonesCache = new Hashtable<>();
    
    /** The actual start tone of this tone-system, can be different from <code>baseToneIpnName()</code>. */
    public final String modalScaleStartIpnName;
    /** The actual number of octaves of this tone-system. */
    public final int octaves;
    
    private final double frequencyOfA4;
    private final String baseToneIpnName;

    /**
     * @param frequencyOfA4 the desired frequency of tone "A4".
     * @param baseToneIpnName the build-note for the requested tone array e.g. "E3" or "G1".
     *      It is the tone the chromaticScale is built upon, calculated from frequencyOfA4.
     *      This is NOT the <code>referenceIpnName</code>!
     * @param modalScaleStartIpnName the lowest note of the tone array resulting from a tones() call.
     *      When baseToneIpnName is "C0" and modalScaleStartIpnName is "A0" you will get an A-minor scale,
     *      built upon a C-major scale (AEOLIAN mode). Thus it is the specifier for the modal scale you want.
     *      By default it is <code>baseToneIpnName</code>.
     * @param octaves the 0-n number of octaves + 1 to return.
     *      When zero, just the lowest tone is returned,
     *      when less than zero, ToneSystem.DEFAULT_OCTAVES octaves will be returned.
     */
    protected AbstractToneSystem(double frequencyOfA4, String baseToneIpnName, String modalScaleStartIpnName, int octaves) {
        this.frequencyOfA4 = (frequencyOfA4 <= 0.0) ? ToneSystem.DEFAULT_REFERENCE_FREQUENCY : frequencyOfA4;
        this.baseToneIpnName = (baseToneIpnName == null) ? ToneSystem.DEFAULT_BASETONE_IPN_NAME : baseToneIpnName;
        this.modalScaleStartIpnName = (modalScaleStartIpnName == null) ? this.baseToneIpnName : modalScaleStartIpnName;
        this.octaves = checkForMaximumOctaves(octaves, this.modalScaleStartIpnName);
    }
    
    private int checkForMaximumOctaves(int octaves, String modalScaleStartIpnName) {
        final int startNoteOctave = getOctave(modalScaleStartIpnName);
        final String startNoteWithoutOctave = removeOctave(modalScaleStartIpnName);
        final boolean isC = startNoteWithoutOctave.equals(ToneSystem.IPN_BASE_NAMES[0]);
        final int maximumOctaves = ToneSystem.MAXIMUM_OCTAVES - startNoteOctave - (isC ? 0 : 1); // "D0" would need one less

        if (octaves < 0) // number of octaves was NOT specified, calculate possible maximum
            return maximumOctaves;
        
        if (octaves > 0 && octaves > maximumOctaves)
            throw new IllegalArgumentException(octaves+" octaves not possible for "+this.baseToneIpnName+", maximum is "+maximumOctaves);

        return octaves;
    }

    /** @return default is the simple class name. */
    @Override
    public String name() {
        return getClass().getSimpleName();
    }
    
    /** {@inheritDoc} */
    @Override
    public double referenceFrequency() {
        return frequencyOfA4;
    }
    
    /** {@inheritDoc} */
    @Override
    public String baseToneIpnName() {
        return baseToneIpnName;
    }
    
    /** {@inheritDoc} */
    @Override
    public Tone[] tones() {
        return AbstractToneSystem.tones(getOrCreateCachedTones(), baseToneIpnName(), octaves);
    }
    
    protected final Tone[] getOrCreateCachedTones() {
        final Object hashKey = getCacheKey();
        final Tone[] cachedTones = tonesCache.get(hashKey);
        if (cachedTones != null)
            return cachedTones;
            
        final Tone[] calculatedTones = createTones();
        tonesCache.put(hashKey, calculatedTones);
        
        return calculatedTones;
    }

    /** Sub-classes MUST define a cache-key for caching maximum-length 12-tone scales starting from zero-octave. */
    protected abstract Object getCacheKey();

    /**
     * @return a 12-tone scale with length <code>ToneSystem.MAXIMUM_OCTAVES</code>, 
     *      starting from zero-octave, to be cached. Mind that this is not what
     *      the constructor may request, but a basic full scale that is the base for it.
     *      The lowest tone depends on the type of <code>ToneSystem</code> implementation
     *      and possibly needs to be considered in <code>getCacheKey()</code>.
     */
    protected abstract Tone[] createTones();

    
    /** @return the given IPN-name without trailing octave-number. */
    protected final String removeOctave(String ipnName) {
        return ipnName.replaceAll("[0-9\\-]", "");
    }
    
    /** @return the octave-number from given IPN-name. */
    protected final int getOctave(String ipnName) {
        return Integer.valueOf(ipnName.replaceAll("[^0-9\\-]", ""));
    }
    
    /**
     * Delivers a sub-set of given tones.
     * @param tones the tone-system to filter.
     * @param lowestIpnName the start point of the requested tone array.
     * @param octaves the 0-n number of octaves + 1 to return.
     *      When zero, just the lowest tone is returned.
     * @return all tones from given IPN-name up to given octaves + 1.
     * @throws IllegalArgumentException when given number of octaves is too big for given tones stock.
     */
    public static Tone[] tones(Tone[] tones, String lowestIpnName, int octaves) {
        if (octaves < 0)
            throw new IllegalArgumentException("Number of octaves can not be negative: "+octaves);
        
        final int startIndex = IntStream.range(0, tones.length)
                .filter(i -> tones[i].ipnName.equals(lowestIpnName))
                .findFirst()
                .orElseThrow();
        
        final int endIndex = startIndex + (ToneSystem.SEMITONES_PER_OCTAVE * octaves) + 1;
        if (endIndex > tones.length)
            throw new IllegalArgumentException("Number of octaves is too big: "+octaves);
        
        return Arrays.copyOfRange(tones, startIndex, endIndex);
    }
    
    @Override
    public String toString() {
        final boolean baseIsScaleStart = baseToneIpnName().equals(modalScaleStartIpnName);
        final String baseToneInfo = baseIsScaleStart
                ? "lowest tone "+baseToneIpnName()
                : "built upon "+baseToneIpnName()+", lowest tone "+modalScaleStartIpnName;
        return name()+", "+
                baseToneInfo+
                ", "+octaves+" octaves"+
                ", calculated from "+ToneSystem.REFERENCE_FREQUENCY_IPN_NAME+
                " with frequency "+Tone.frequencyFormat.format(referenceFrequency())+" Hertz";
    }
}