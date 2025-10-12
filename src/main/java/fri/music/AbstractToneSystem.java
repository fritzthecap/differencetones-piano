package fri.music;

import java.util.Arrays;
import java.util.Hashtable;
import java.util.Map;
import java.util.stream.IntStream;
import fri.music.utils.StringUtil;

/**
 * Common implementations for all ToneSystem classes.
 */
public abstract class AbstractToneSystem implements ToneSystem
{
    /** The global cache for the fully built tone-arrays. */
    public static final Map<Object,Tone[]> tonesCache = new Hashtable<>();
    
    /** The actual start tone of this tone-system, can be different from <code>baseToneIpnName()</code>. */
    protected final String modalScaleStartIpnName;
    
    private final int octaves;
    private final double frequencyOfA4;
    private final String baseToneIpnName;

    /**
     * All parameters are optional and will be replaced by defaults when null or out of range.
     * @param frequencyOfA4 the desired frequency of tone "A4".
     * @param baseToneIpnName e.g. "E3" or "G1", the tone the chromatic scale is calculated from,
     *      using an equal-temperament scale at frequencyOfA4. The octave is not important, because
     *      the tone will be lowered to the zero octave, but in case the modalScaleStartIpnName is
     *      null, it will be its default.
     * @param modalScaleStartIpnName the lowest note of the tone array resulting from a tones() call.
     *      When baseToneIpnName is "C0" and modalScaleStartIpnName is "A0" you will get an A-minor scale,
     *      built upon a C-major scale (AEOLIAN mode), thus it is the specifier for the modal scale you
     *      want. This plays a role in just-intonation only. Default is <code>baseToneIpnName</code>.
     * @param octaves the 0-n number of octaves to return, plus one tone on top. When zero, just the lowest
     *      tone will be returned, when less than zero or more than ToneSystem.MAXIMUM_OCTAVES,
     *      ToneSystem.MAXIMUM_OCTAVES octaves will be returned.
     */
    protected AbstractToneSystem(double frequencyOfA4, String baseToneIpnName, String modalScaleStartIpnName, int octaves) {
        this.frequencyOfA4 = (frequencyOfA4 <= 0.0) ? ToneSystem.DEFAULT_REFERENCE_FREQUENCY : frequencyOfA4;
        this.baseToneIpnName = (baseToneIpnName == null) ? ToneSystem.DEFAULT_BASETONE_IPN_NAME : baseToneIpnName;
        this.modalScaleStartIpnName = (modalScaleStartIpnName == null) ? this.baseToneIpnName : modalScaleStartIpnName;
        this.octaves = checkForMaximumOctaves(octaves);
    }
    
    private int checkForMaximumOctaves(int octaves) {
        final int startNoteOctave = getOctave(modalScaleStartIpnName);
        final String startNoteWithoutOctave = removeOctave(modalScaleStartIpnName);
        final boolean isC = startNoteWithoutOctave.equals(ToneSystem.IPN_BASE_NAMES[0]);
        final int maximumOctaves = ToneSystem.MAXIMUM_OCTAVES - startNoteOctave - (isC ? 0 : 1); // "D0" would need one less

        if (octaves < 0) // number of octaves was NOT specified, calculate possible maximum
            return maximumOctaves;
        
        if (octaves > maximumOctaves)
            throwNumberOfOctavesTooBig(octaves, baseToneIpnName());

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
    public int octaves() {
        return octaves;
    }
    
    /** {@inheritDoc} */
    @Override
    public String baseToneIpnName() {
        return baseToneIpnName;
    }
    
    /** {@inheritDoc} */
    @Override
    public Tone[] tones() {
        return AbstractToneSystem.tones(getOrCreateCachedTones(), baseToneIpnName(), baseToneIpnName(), octaves);
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
        return StringUtil.getUntilFirstNumber(ipnName);
    }
    
    /** @return the octave-number from given IPN-name. */
    protected final int getOctave(String ipnName) {
        return StringUtil.getFirstNumber(ipnName);
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
    public static Tone[] tones(Tone[] tones, String lowestIpnName, String baseToneIpnName, int octaves) {
        if (octaves < 0)
            throw new IllegalArgumentException("Number of octaves can not be negative: "+octaves);
        
        final int startIndex = IntStream.range(0, tones.length)
                .filter(i -> tones[i].ipnName.equals(lowestIpnName))
                .findFirst()
                .orElseThrow();
        
        final int endIndex = startIndex + (ToneSystem.SEMITONES_PER_OCTAVE * octaves) + 1;
        if (endIndex > tones.length)
            throwNumberOfOctavesTooBig(octaves, baseToneIpnName);
        
        return Arrays.copyOfRange(tones, startIndex, endIndex);
    }

    private static void throwNumberOfOctavesTooBig(int octaves, String baseToneIpnName) {
        throw new IllegalArgumentException(
                octaves+" octaves not possible for "+baseToneIpnName+
                ", please choose a smaller number or change the tone!");
    }
    
    @Override
    public String toString() {
        final boolean baseIsScaleStart = baseToneIpnName().equals(modalScaleStartIpnName);
        final String baseToneInfo = baseIsScaleStart
                ? "lowest tone "+baseToneIpnName()
                : "built upon "+baseToneIpnName()+", modal start tone "+modalScaleStartIpnName;
        return name()+", "+
                baseToneInfo+
                ", "+octaves+" octaves"+
                ", calculated from "+ToneSystem.REFERENCE_FREQUENCY_IPN_NAME+
                " with frequency "+Tone.frequencyFormat.format(referenceFrequency())+" Hertz";
    }
}