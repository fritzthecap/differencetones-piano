package fri.music.differencetones;

import java.util.Objects;
import fri.music.Tone;
import fri.music.ToneSystem;
import fri.music.Tones;

/**
 * <p>
 * Methods to find difference tones. See <code>DifferenceTonesDemo</code>.
 * When two tones sound simultaneously, up to three difference tones occur, 
 * more than one mostly on intervals greater equal minor sixth. 
 * The primary difference tone ascends when tones move further apart from each other,
 * the secondary and tertiary descend in that case.
 * See <a href="https://www.sfu.ca/sonic-studio-webdav/handbook/Combination_Tones.html">web-image</a>.
 * </p><p>
 * Minor second and major seventh generate very low, inexact and unpredictable
 * difference tones, so I did not analyze them.
 * </p><hr/><p>
 * For <b>equal-temperament tuning</b>, none of the difference tones is exact, 
 * but they are predictable and consistent, i.e. any interval like e.g. major third
 * gives the same result. The following applies (biggest differences to just-intonation in bold):
 * </p><ul>
 * <li>Major second generates the lower tone 3 octaves lower: C6 - D6 = C3</li>
 * <li>Minor third generates the fifth of the lower tone 3 octaves lower: <b>C6 - D#6 = G3</b></li>
 * <li>Major third generates the sharpened lower tone 2 octaves lower: <b>C6 - E6 = C#4</b></li>
 * <li>Fourth generates the upper tone 2 octaves lower: C6 - F6 = F4</li>
 * <li>Tritone generates the major sixth of the lower tone 2 octaves lower: C6 - F#6 = A4,
 *     further it might also generate the minor third of the lower tone 1 octave lower: <b>C6 - F#6 = D#5</b></li>
 * <li>Fifth generates the lower tone 1 octave lower: C6 - G6 = C5</li>
 * <li>Minor sixth generates the minor third of the lower tone 1 octave lower: C6 - G#6 = D#5,
 *     further it generates the major sixth of the lower tone 2 octaves lower: <b>C6 - G#6 = A4</b></li>
 * <li>Major sixth generates the fourth of the lower tone 1 octave lower: C6 - A6 = F5,
 *     further it might also generate the major third of the lower tone 2 octaves lower: <b>C6 - A6 = E4</b></li>
 * <li>Minor seventh generates the minor sixth of the lower tone 1 octave lower: C6 - A#6 = G#5,
 *     further it might also generate the upper tone 3 octaves lower: C6 - A#6 = A#3</li>
 * </ul><p>
 * For <b>just-intonation tuning</b> LIMIT_5_SYMMETRIC_1, most difference tones are very precise, 
 * except the augmented fourth (tritone) and the minor seventh, but some are inconsistent
 * and hard to predict.
 * </p><ul>
 * <li>Major second may generate the lower tone 3 octaves lower: C6 - D6 = C3,
 *     but it can also generate the tone two semi-tones below lower tone: D6 - E6 = C3</li>
 * <li>Minor third generates the minor sixth of the lower tone 3 octaves lower: <b>C6 - D#6 = G#3</b>,
 *     but it can also generate the fifth of the lower tone 3 octaves lower: D6 - F6 = A3</li>
 * <li>Major third generates the lower tone 2 octaves lower: <b>C6 - E6 = C4</b>,
 *     but it can also generate the tone between the two, 2 octaves lower: A5 - C#6 = B3</li>
 * <li>Fourth generates the upper tone 2 octaves lower: C6 - F6 = F4</li>
 * <li>Tritone generates the major sixth of the lower tone 2 octaves lower: C6 - F#6 = A4,
 *     further it might also generate the major second of the lower tone 1 octave lower: <b>C6 - F#6 = D5</b>,
 *     but just C - F# is precise, all other tritones generate frequencies that are not in scale</li>
 * <li>Fifth generates the lower tone 1 octave lower: C6 - G6 = C5,
 *     although D6 - A6 and B5 - F#6 are not in scale</li>
 * <li>Minor sixth generates the minor third of the lower tone 1 octave lower: C6 - G#6 = D#5,
 *     further it generates the upper tone 2 octaves lower: <b>C6 - G#6 = G#4</b></li>
 * <li>Major sixth generates the fourth of the lower tone 1 octave lower: C6 - A6 = F5,
 *     further it might also generate the fourth of the lower tone 2 octaves lower: <b>C6 - A6 = F4</b>,
 *     but it can also generate the tritone of the lower tone 1 octave lower: E5 - C#6 = A#4</li>
 * <li>Minor seventh generates the major third below the lower tone: E5 - D6 = C5,
 *     further it might also generate the upper tone 3 octaves lower: C6 - A#6 = A#3</li>
 * </ul>
 */
public class DifferenceTones extends Tones
{
    /** The minimal deviation that lets find almost all EqualTemperament difference tones. */
    public static final double TOLERANT_DEVIATION_EDO_12 = 0.365; // 73 %
    /** The deviation that lets find JustIntonation difference tones exactly, but no TRITONE!. */
    public static final double PRECISE_DEVIATION_JI = 0.09; // 18 %
    /** The minimal deviation that lets also find JustIntonation TRITONE difference tones. */
    public static final double TOLERANT_DEVIATION_JI = 0.38; // 76 %
    /** The default deviation. */
    public static final double DEFAULT_DEVIATION = TOLERANT_DEVIATION_JI;
    /** The maximum deviation. */
    public static final double MAXIMUM_DEVIATION = 0.45;
    
    public final double deviationTolerance;
    private final boolean primaryDifferenceToneOnly;
    
    public DifferenceTones() {
        this(null);
    }
    public DifferenceTones(Tone[] toneSystem) {
        this(toneSystem, DEFAULT_DEVIATION);
    }
    /**
     * @param toneSystem the tones to find difference tones within.
     * @param deviationTolerance between 0 and 0.45, default is 0.37.
     *      A value of 0.5 would mean the difference tone is allowed to be
     *      in the middle between two tones (which is <b>not</b> a good strategy!).
     */
    public DifferenceTones(Tone[] toneSystem, double deviationTolerance) {
        this(toneSystem, deviationTolerance, true);
    }
    /**
     * @param toneSystem the tones to find difference tones within.
     * @param deviationTolerance between 0 and 0.45, default is 0.37.
     *      A value of 0.5 would mean the difference tone is allowed to be
     *      in the middle between two tones (which is <b>not</b> a good strategy!).
     * @param primaryDifferenceToneOnly default is true, 
     *      search the primary difference tone only, ignore secondary and tertiary.
     */
    public DifferenceTones(Tone[] toneSystem, double deviationTolerance, boolean primaryDifferenceToneOnly) {
        super(toneSystem);
        
        if (deviationTolerance > MAXIMUM_DEVIATION || deviationTolerance < 0.0)
            throw new IllegalArgumentException("Deviation tolerance must be between 0 and "+MAXIMUM_DEVIATION+", given was "+deviationTolerance);
        
        this.deviationTolerance = deviationTolerance;
        this.primaryDifferenceToneOnly = primaryDifferenceToneOnly;
    }
    
    /**
     * Finds difference tones for given interval.
     * The given interval must be smaller equal an octave, 
     * else an IllegalArgumenException will be thrown.
     * @param firstToneIpnName required, the first tone of the interval, can be upper or lower.
     * @param secondToneIpnName required, the second tone of the interval, can be upper or lower.
     * @return array of resulting difference tones, can be up to three.
     * @throws IllegalArgumenException when given interval is greater than an octave.
     */
    public Tone[] findDifferenceTones(String firstToneIpnName, String secondToneIpnName) {
        return findDifferenceTones(forIpnName(firstToneIpnName), forIpnName(secondToneIpnName));
    }
    
    /**
     * Finds difference tones for given interval.
     * The given interval must be smaller equal an octave, 
     * else an IllegalArgumenException will be thrown.
     * @param firstMidiNoteNumber required, the first tone of the interval, can be upper or lower.
     * @param secondMidiNoteNumber required, the second tone of the interval, can be upper or lower.
     * @return array of resulting difference tones, can be up to three.
     * @throws IllegalArgumenException when given interval is greater than an octave.
     */
    public Tone[] findDifferenceTones(int firstMidiNoteNumber, int secondMidiNoteNumber) {
        return findDifferenceTones(forMidiNoteNumber(firstMidiNoteNumber), forMidiNoteNumber(secondMidiNoteNumber));
    }
    
    /**
     * Finds difference tones for given interval.
     * The given interval must be smaller equal an octave, 
     * else an IllegalArgumenException will be thrown.
     * @param firstTone required, the first tone of the interval, can be upper or lower.
     * @param secondTone required, the second tone of the interval, can be upper or lower.
     * @return array of resulting difference tones, can be up to three, each can be null when not found.
     * @throws IllegalArgumentException when given interval is greater than an octave.
     */
    public Tone[] findDifferenceTones(Tone firstTone, Tone secondTone) {
        Objects.requireNonNull(firstTone);
        Objects.requireNonNull(secondTone);
        if (Math.abs(secondTone.midiNumber - firstTone.midiNumber) > ToneSystem.SEMITONES_PER_OCTAVE)
            throw new IllegalArgumentException("WARNING: The interval to find difference tones for must not be greater than an octave, given was "+firstTone.ipnName+" - "+secondTone.ipnName);
        
        // difference tones are at frequencies (y - x), (2x - y) and (3x - 2y), 
        // https://www.sfu.ca/sonic-studio-webdav/handbook/Combination_Tones.html
        final double primaryDifference = DifferenceToneMath.primaryDifference(secondTone.frequency, firstTone.frequency);
        final Tone primaryDifferenceTone = findDifferenceTone(primaryDifference);
        
        if (primaryDifferenceToneOnly)
            return new Tone[] { primaryDifferenceTone };
        
        final double secondaryDifference = DifferenceToneMath.secondaryDifference(firstTone.frequency, secondTone.frequency);
        final Tone secondaryDifferenceTone = findDifferenceTone(secondaryDifference);
        
        final double tertiaryDifference  = DifferenceToneMath.tertiaryDifference(firstTone.frequency, secondTone.frequency);
        final Tone tertiaryDifferenceTone = findDifferenceTone(tertiaryDifference);
        
        return new Tone[] { primaryDifferenceTone, secondaryDifferenceTone, tertiaryDifferenceTone };
    }


    private Tone findDifferenceTone(final double frequencyToMatch) {
       final Tone[] enclosingTones = getEnclosingTones(frequencyToMatch);
       final Tone lower = enclosingTones[0];
       final Tone upper = enclosingTones[1];
       
       if (upper == lower) // exact match was found, or both null
           return lower; // both are the same
       
       /*if (upper == null) { // frequency is below lowest tone
           final double gapToLower = lower.frequency - frequencyToMatch;
           final double assumedGap = getNextUpper(lower).frequency - lower.frequency;
           final double tolerance = assumedGap * deviationTolerance;
           return (gapToLower > tolerance) ? null : lower;
       }
       if (lower == null) { // frequency is above highest tone
           final double gapToUpper = frequencyToMatch - upper.frequency;
           final double assumedGap = getNextLower(upper).frequency - upper.frequency;
           final double tolerance = assumedGap * deviationTolerance;
           return (gapToUpper > tolerance) ? null : upper;
       }*/
       
       if (upper == null || // frequency is below lowest tone
               lower == null) // frequency is above highest tone
           return null; // can not calculate an exact result, assumptions lead to mismatches

       final double gapToLower = frequencyToMatch - lower.frequency;
       final double gapToUpper = upper.frequency - frequencyToMatch;
       final boolean preferLower = (gapToLower < gapToUpper);
       
       final double gap = upper.frequency - lower.frequency;
       final double tolerance = gap * deviationTolerance; // deviation is never negative
       final double smallerGap = preferLower ? gapToLower : gapToUpper;
       
       return (smallerGap > tolerance) ? null : preferLower ? lower : upper;
    }
}