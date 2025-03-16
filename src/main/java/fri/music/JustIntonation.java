package fri.music;

import java.util.HashSet;
import java.util.Hashtable;
import java.util.Map;
import java.util.Set;
import java.util.stream.IntStream;
import java.util.stream.Stream;

/**
 * 12-tone scales using different fractions for tone steps.
 */
public class JustIntonation extends AbstractToneSystem
{
    /** Fraction-driven interval for "just" 12-tone scales. */
    public interface Interval
    {
        /**
         * @return the name of this interval.
         *      Every name must start with one of the names used in <code>ToneSystem.INTERVAL_NAMES</code>.
         */
        String name();
        
        /**
         * Use <code>dividendForOctave()</code> (see below) to implement this!
         * @param octave the octave for the dividend of this interval, not the IPN-octave!
         * @return the dividend of this interval, according to given octave.
         */
        int dividend(int octave);
        
        /**
         * @return the divisor of this interval.
         */
        public int divisor();
        
        
        /**
         * Default implementation to calculate the interval's dividend.
         * @param baseDividend the interval's simple dividend, independent of octave.
         * @param octave the octave for the interval of the dividend, not the IPN-octave!
         * @return the dividend of this interval, according to given base-dividend and octave.
         */
        default int dividendForOctave(int baseDividend, int octave) {
            return baseDividend * (int) Math.pow(2, octave); // 2^0=1, 2^1=2, 2^2=4, 2^3=8,.... 
        }
        
        /**
         * Calculates the interval's frequency from a given base-tone frequency.
         * The base-tone is always the same for all tones of all octaves of a scale.
         * @param baseToneFrequency required, the base tone to calculate a next higher frequency for.
         * @param octave required, the 0-n octave for which to calculate a follower frequency, not the IPN-octave!
         * @return a frequency that would result from given frequency according to this interval.
         */
        default Double frequency(double baseToneFrequency, int octave) {
            return baseToneFrequency * (double) dividend(octave) / (double) divisor();
        }
        
        /** 
         * @param octave 0-n octave for which to calculate cents, not the IPN-octave!
         * @return cent for given octave.
         */
        default double cent(int octave) {
            return Interval.cent(dividend(octave), divisor());
        }
        
        /**
         * @param octave 0-n octave of the interval, not the IPN-octave!
         * @return the simplest possible fraction as string.
         */
        default String ratioString(int octave) {
            final int dividend = dividend(octave);
            final int divisor = divisor();
            final int gcd = MathUtils.greatestCommonDivisor(dividend, divisor);
            return (dividend / gcd)+"/"+(divisor / gcd);
        }
                
        /**
         * Finding the distance between 2 tones means dividing their ratios: upper / lower.
         * @param lower the lower tone's interval.
         * @param lowerOctave the lower tone's octave.
         * @param upper the upper tone's interval.
         * @param upperOctave the upper tone's octave.
         * @return the distance (upper / lower) as ratio in an int-array[2], 0 = dividend, 1 = divisor. 
         */
        static long[] distance(Interval lower, int lowerOctave, Interval upper, int upperOctave) {
            final int dividend = upper.dividend(upperOctave) * lower.divisor();
            final int divisor = upper.divisor() * lower.dividend(lowerOctave);
            return MathUtils.reduceFraction(dividend, divisor);
        }
        
        /**
         * Finding the difference tone of 2 tones means subtracting their ratios.
         * @param lower the lower tone's interval.
         * @param lowerOctave the lower tone's octave.
         * @param upper the upper tone's interval.
         * @param upperOctave the upper tone's octave.
         * @return the difference-tone (upper - lower) as ratio in an int-array[2], 0 = dividend, 1 = divisor. 
         */
        static long[] difference(Interval lower, int lowerOctave, Interval upper, int upperOctave) {
            final int commonDivisor = lower.divisor() * upper.divisor();
            final int lowerDividend = lower.dividend(lowerOctave) * upper.divisor();
            final int upperDividend = upper.dividend(upperOctave) * lower.divisor();
            final int subtractedDividend = upperDividend - lowerDividend; // subtract tones
            return MathUtils.reduceFraction(subtractedDividend, commonDivisor);
        }
        
        /**
         * Human perception limit is 4-6 cent. 1 octave = 1200 cent.
         * E.g. the fifth-tone is 3/2 or 1.5 of the frequency of the octave-tone below it.
         * 1200 * log2(3/2) = cent of fifth ~= 702
         */
        static double cent(long dividend, long divisor) {
            return 1200.0 * Math.log((double) dividend / (double) divisor) / Math.log(2.0);
        }
        /** Ratio = 2 pow cents/1200. */
        static double ratioFromCents(double cents) {
            return Math.pow(2, cents / 1200.0);
        }
    }   // end ScaleDivision
    
    
    /**
     * Collection of different 12-tone tuning intervals for just-intonation.
     * By convention every enum-name must start with a name given in <code>ToneSystem.INTERVAL_NAMES.</code>
     * <p/>
     * The "5-limit" tuning restricts both dividend and divisor to integer-powers of 2, 3 and 5.
     * @see https://en.wikipedia.org/wiki/Five-limit_tuning
     */
    public enum Intervals implements Interval
    {
        UNISON                 ( 1,  1),
        
        /** Defaults from 5-limit tuning. */
        MINOR_SECOND       (16, 15),
        MAJOR_SECOND_10_9  (10,  9), // symmetric to 9/5
        MAJOR_SECOND_9_8   ( 9,  8), // preferable, symmetric to 16/9
        MINOR_THIRD        ( 6,  5),
        MAJOR_THIRD        ( 5,  4),
        FOURTH             ( 4,  3),
        TRITONE_AUG4_45_32 (45, 32),
        TRITONE_DIM5_64_45 (64, 45),
        FIFTH              ( 3,  2),
        MINOR_SIXTH        ( 8,  5),
        MAJOR_SIXTH        ( 5,  3),
        MINOR_SEVENTH_16_9 (16,  9), // preferable, symmetric to 9/8
        MINOR_SEVENTH_9_5  ( 9,  5), // symmetric to 10/9
        MAJOR_SEVENTH      (15,  8),
        OCTAVE             ( 2,  1),
        
        /** Not part of 5-limit but quite consonant. */
        TRITONE_17_12          (17, 12),
        
        /** 5-limit asymmetric extended */
        TRITONE_AUG4_5LIMIT_ASYM_EXT   (25, 18),
        TRITONE_DIM5_5LIMIT_ASYM_EXT   (36, 25),
        
        /** 7-limit tuning, take missing from 5-limit. */
        MINOR_SECOND_7LIMIT       (15, 14),
        MAJOR_SECOND_7LIMIT       ( 8,  7),
        TRITONE_AUG4_7LIMIT       ( 7,  5), // Huygens' tritone
        TRITONE_DIM5_7LIMIT       (10,  7), // Eulers' tritone
        MINOR_SEVENTH_7LIMIT      ( 7,  4), // the "sweet" harmonic seventh
        
        /** 17-limit tuning, take missing from 7-limit. */
        MINOR_SECOND_17LIMIT      (14, 13),
        MAJOR_SEVENTH_17LIMIT     (13,  7),
        
        /** Harmonic series, take missing from 5-limit. See https://en.wikipedia.org/wiki/Harmonic_series_(music) */
        MINOR_SECOND_HARMONIC       (  17,  16),
        MAJOR_SECOND_HARMONIC       ( MAJOR_SECOND_9_8.dividend, MAJOR_SECOND_9_8.divisor), // 9/8
        MINOR_THIRD_HARMONIC        (  19,  16),
        FOURTH_HARMONIC             (  21,  16),
        TRITONE_AUG4_HARMONIC       (  11,   8),
        TRITONE_DIM5_HARMONIC       (  23,  16),
        MINOR_SIXTH_HARMONIC_25_16  (  25,  16),
        MINOR_SIXTH_HARMONIC_13_8   (  13,   8),
        MAJOR_SIXTH_HARMONIC        (  27,  16),
        MINOR_SEVENTH_HARMONIC_7_4  ( MINOR_SEVENTH_7LIMIT.dividend, MINOR_SEVENTH_7LIMIT.divisor), // 7/4, the "sweet" harmonic seventh
        MINOR_SEVENTH_HARMONIC_29_16(  29,  16),
        MAJOR_SEVENTH_HARMONIC_15_8 ( MAJOR_SEVENTH.dividend, MAJOR_SEVENTH.divisor), // 15/8
        MAJOR_SEVENTH_HARMONIC_31_16(  31,  16),
        
        /** Pythagorean tuning, take missing from 5-limit. This tuning was abandoned in 16th century due to dissonant thirds and sixths. */
        MINOR_SECOND_PYTHAG    ( 256, 243),
        MAJOR_SECOND_PYTHAG    ( MAJOR_SECOND_9_8.dividend, MAJOR_SECOND_9_8.divisor), // 9/8
        MINOR_THIRD_PYTHAG     (  32,  27),
        MAJOR_THIRD_PYTHAG     (  81,  64),
        TRITONE_DIM5_PYTHAG    (1024, 729),
        TRITONE_AUG4_PYTHAG    ( 729, 512),
        MINOR_SIXTH_PYTHAG     ( 128,  81),
        MAJOR_SIXTH_PYTHAG     ( MAJOR_SIXTH_HARMONIC.dividend, MAJOR_SIXTH_HARMONIC.divisor), // 27/16
        MINOR_SEVENTH_PYTHAG   ( MINOR_SEVENTH_16_9.dividend, MINOR_SEVENTH_16_9.divisor), // 16/9
        MAJOR_SEVENTH_PYTHAG   ( 243, 128),
        ;
        
        private final int dividend;
        private final int divisor;
        
        private Intervals(int dividend, int divisor) {
            if (dividend < divisor)
                throw new IllegalArgumentException("Intervals must point upwards, got downwards: "+dividend+"/"+divisor);
            
            final String name = name();
            if (Stream.of(ToneSystem.INTERVAL_NAMES).anyMatch(intervalName -> name.startsWith(intervalName)) == false)
                throw new IllegalArgumentException("Intervals must start with a name from ToneSystem.INTERVAL_NAMES, but this did not: "+name);
            
            this.dividend = dividend;
            this.divisor = divisor;
        }
        
        /** {@inheritDoc}. */
        public int dividend(int octave) {
            return dividendForOctave(dividend, octave);
        }
        
        /** {@inheritDoc}. */
        public int divisor() {
            return divisor;
        }
        
        @Override
        public String toString() {
            return name()+"("+ratioString(0)+", "+(int) Math.round(cent(0))+"¢)";
        }
    }

    
    /**
     * Responsibilities of a chromatic 12-tone scale in just-intonation.
     * A 12-tone scale starts with the interval from base note to second,
     * continues with the interval from base note to third note, and
     * always ends with the octave from base note to the base note one octave higher.
     */
    public interface ChromaticScale
    {
        /** @return the name of this chromatic scale. */
        String name();
        
        /**
         * @return the 12 intervals of this chromatic scale, starting at zero 
         *      with step from first to second tone (e.g. "C4" -> "C#4").
         */
        Interval[] intervals();
    }
    
    
    /**
     * Some standard 12-tone scales.
     * @see https://en.wikipedia.org/wiki/Five-limit_tuning#Twelve-tone_scale
     */
    public enum ChromaticScales implements ChromaticScale
    {
        /** 5-limit-tuning, good for major tunes. */
        LIMIT_5_SYMMETRIC_1(new Intervals[] {
                Intervals.MINOR_SECOND,
                Intervals.MAJOR_SECOND_9_8,
                Intervals.MINOR_THIRD,
                Intervals.MAJOR_THIRD,
                Intervals.FOURTH,
                Intervals.TRITONE_AUG4_45_32,
                Intervals.FIFTH,
                Intervals.MINOR_SIXTH,
                Intervals.MAJOR_SIXTH,
                Intervals.MINOR_SEVENTH_16_9,
                Intervals.MAJOR_SEVENTH,
                Intervals.OCTAVE,
            }),
        /** Another 5-limit-tuning, good for the parallel minor. */
        LIMIT_5_SYMMETRIC_2(new Intervals[] {
                Intervals.MINOR_SECOND,
                Intervals.MAJOR_SECOND_10_9,
                Intervals.MINOR_THIRD,
                Intervals.MAJOR_THIRD,
                Intervals.FOURTH,
                Intervals.TRITONE_AUG4_45_32,
                Intervals.FIFTH,
                Intervals.MINOR_SIXTH,
                Intervals.MAJOR_SIXTH,
                Intervals.MINOR_SEVENTH_9_5,
                Intervals.MAJOR_SEVENTH,
                Intervals.OCTAVE,
            }),
        /** Asymmetric: minor seventh and major second do not have same distance from tonic. */
        LIMIT_5_ASYMMETRIC(new Intervals[] {
                Intervals.MINOR_SECOND,
                Intervals.MAJOR_SECOND_9_8,
                Intervals.MINOR_THIRD,
                Intervals.MAJOR_THIRD,
                Intervals.FOURTH,
                Intervals.TRITONE_AUG4_45_32,
                Intervals.FIFTH,
                Intervals.MINOR_SIXTH,
                Intervals.MAJOR_SIXTH,
                Intervals.MINOR_SEVENTH_9_5,
                Intervals.MAJOR_SEVENTH,
                Intervals.OCTAVE,
            }),
        /** Overtone scale, all difference-tones fit perfectly. */
        HARMONIC_SERIES(new Intervals[] {
                Intervals.MINOR_SECOND_HARMONIC,
                Intervals.MAJOR_SECOND_HARMONIC,
                Intervals.MINOR_THIRD_HARMONIC,
                Intervals.MAJOR_THIRD,
                Intervals.FOURTH_HARMONIC,
                Intervals.TRITONE_AUG4_HARMONIC,
                Intervals.FIFTH,
                Intervals.MINOR_SIXTH_HARMONIC_13_8,
                Intervals.MAJOR_SIXTH_HARMONIC,
                Intervals.MINOR_SEVENTH_HARMONIC_7_4,
                Intervals.MAJOR_SEVENTH_HARMONIC_15_8,
                Intervals.OCTAVE,
            }),
        /** Pythagorean scale. */
        PYTHAGOREAN(new Intervals[] {
                Intervals.MINOR_SECOND_PYTHAG,
                Intervals.MAJOR_SECOND_PYTHAG,
                Intervals.MINOR_THIRD_PYTHAG,
                Intervals.MAJOR_THIRD_PYTHAG,
                Intervals.FOURTH,
                Intervals.TRITONE_AUG4_PYTHAG,
                Intervals.FIFTH,
                Intervals.MINOR_SIXTH_PYTHAG,
                Intervals.MAJOR_SIXTH_PYTHAG,
                Intervals.MINOR_SEVENTH_PYTHAG,
                Intervals.MAJOR_SEVENTH_PYTHAG,
                Intervals.OCTAVE,
            }),
//      /** 7-limit tuning is very similar to 5-limit. */
//      LIMIT_7(new Interval[] {
//              Interval.MINOR_SECOND_7LIMIT,
//              Interval.MAJOR_SECOND_7LIMIT,
//              Interval.MINOR_THIRD_6_5,
//              Interval.MAJOR_THIRD_5_4,
//              Interval.FOURTH_4_3,
//              Interval.TRITONE_AUG4_7LIMIT,
//              //Interval.TRITONE_DIM5_7LIMIT,
//              Interval.FIFTH_3_2,
//              Interval.MINOR_SIXTH_8_5,
//              Interval.MAJOR_SIXTH_5_3,
//              Interval.MINOR_SEVENTH_7LIMIT,
//              Interval.MAJOR_SEVENTH_15_8,
//              Interval.OCTAVE,
//          }),
//      /** 17-limit tuning is very similar to 7-limit. */
//      LIMIT_17(new Interval[] {
//              Interval.MINOR_SECOND_17LIMIT,
//              Interval.MAJOR_SECOND_7LIMIT,
//              Interval.MINOR_THIRD_6_5,
//              Interval.MAJOR_THIRD_5_4,
//              Interval.FOURTH_4_3,
//              //Interval.TRITONE_AUG4_7LIMIT,
//              //Interval.TRITONE_17_12,
//              Interval.TRITONE_DIM5_7LIMIT,
//              Interval.FIFTH_3_2,
//              Interval.MINOR_SIXTH_8_5,
//              Interval.MAJOR_SIXTH_5_3,
//              Interval.MINOR_SEVENTH_7LIMIT,
//              Interval.MAJOR_SEVENTH_17LIMIT,
//              Interval.OCTAVE,
//          }),
        ;
        
        private final Interval[] intervals;
        
        private ChromaticScales(Interval[] intervals) {
            if (intervals.length != 12)
                throw new IllegalArgumentException("Scales must have exactly 12 intervals: "+name());
            
            final Set<Interval> checkSet = new HashSet<>();
            for (Interval interval : intervals)
                if (checkSet.contains(interval))
                    throw new IllegalArgumentException("Found repeated interval in scale: "+interval+" in "+name());
                else
                    checkSet.add(interval);
            
            this.intervals = intervals;
        }
        
        public Interval[] intervals() {
            return intervals;
        }
        
        @Override
        public String toString() {
            StringBuilder sb = new StringBuilder(name()+":\t");
            for (Interval interval : intervals) {
                sb.append(interval);
                if (interval != intervals[intervals.length - 1])
                    sb.append(", ");
            }
            return sb.toString();
        }
    }

    
    private static final Map<Object,Tone[]> cachKeyToTones = new Hashtable<>();
    
    public final ChromaticScale chromaticScale;
    
    public JustIntonation() {
        this(null);
    }
    public JustIntonation(ChromaticScale chromaticScale) {
        this(-1.0, chromaticScale);
    }
    public JustIntonation(int octaves) {
        this((String) null, octaves);
    }
    public JustIntonation(double frequencyOfA4) {
        this(frequencyOfA4, (String) null);
    }
    public JustIntonation(double frequencyOfA4, String baseToneIpnName) {
        this(frequencyOfA4, baseToneIpnName, -1);
    }
    public JustIntonation(double frequencyOfA4, String baseToneIpnName, int octaves) {
        this(frequencyOfA4, baseToneIpnName, null, octaves, null);
    }
    public JustIntonation(double frequencyOfA4, ChromaticScale chromaticScale) {
        this(frequencyOfA4, null, null, -1, chromaticScale);
    }
    public JustIntonation(String baseToneIpnName, ChromaticScale chromaticScale) {
        this(-1.0, baseToneIpnName, null, -1, chromaticScale);
    }
    public JustIntonation(String baseToneIpnName, String modalScaleStartIpnName, ChromaticScale chromaticScale) {
        this(baseToneIpnName, modalScaleStartIpnName, -1, chromaticScale);
    }
    public JustIntonation(String baseToneIpnName, String modalScaleStartIpnName, int octaves, ChromaticScale chromaticScale) {
        this(-1.0, baseToneIpnName, modalScaleStartIpnName, octaves, chromaticScale);
    }
    public JustIntonation(String baseToneIpnName, int octaves) {
        this(baseToneIpnName, octaves, null);
    }
    public JustIntonation(String baseToneIpnName, int octaves, ChromaticScale chromaticScale) {
        this(-1.0, baseToneIpnName, null, octaves, chromaticScale);
    }
    /**
     * Tones of "just intonation" tuning (ancient system), 
     * where one octave is divided into 12 differently sized semi-tone steps.
     * @param frequencyOfA4 the desired frequency of tone "A4" (IPN-name).
     * @param baseToneIpnName the build-note for the requested tone array e.g. "E3" or "G1".
     *      It is the tone the chromaticScale is built upon, calculated from frequencyOfA4.
     *      This is NOT the <code>referenceIpnName</code>!
     * @param modalScaleStartIpnName the start-note of the tone array resulting from a tones() call.
     *      When baseToneIpnName is "C0" and modalScaleStartIpnName is "A0" you will get an A-minor scale,
     *      built upon a C-major scale (AEOLIAN mode). Thus it is the specifier for the modal scale you want.
     *      By default it is baseToneIpnName.
     * @param octaves the 0-n number of octaves + 1 to return.
     *      When zero, just the lowest tone is returned,
     *      when less than zero, 9 octaves will be returned.
     * @param chromaticScale the scale for calculating frequencies from intervals.
     */
    public JustIntonation(
            double frequencyOfA4, 
            String baseToneIpnName, 
            String modalScaleStartIpnName, 
            int octaves, 
            ChromaticScale chromaticScale)
    {
        super(frequencyOfA4, baseToneIpnName, modalScaleStartIpnName, octaves);
        
        this.chromaticScale = (chromaticScale == null) ? ChromaticScales.LIMIT_5_SYMMETRIC_1 : chromaticScale;
        
        if (this.chromaticScale.intervals().length != SEMITONES_PER_OCTAVE)
            throw new IllegalArgumentException("Can process only 12-tone scales: "+chromaticScale);
    }
    
    @Override
    public String name() {
        return super.name()+" "+chromaticScale.name();
    }
    
    @Override
    public Tone[] tones() {
        return tones(getOrCreateCachedTones(), modalScaleStartIpnName, octaves);
    }
    
    
    private record CacheKey(double frequencyOfA4, String lowestInZeroOctave, ChromaticScale chromaticScale) {
    }
    
    @Override
    protected Map<Object,Tone[]> tonesCache() {
        return cachKeyToTones;
    }
    
    @Override
    protected Object getCacheKey() {
        // use lowest start-note for caching tones
        final String lowestInZeroOctave = removeOctave(baseToneIpnName())+"0"; // "E0" from "E3"
        return new CacheKey(referenceFrequency(), lowestInZeroOctave, chromaticScale);
    }
    
    @Override
    protected Tone[] createTones() {
        final String lowestInZeroOctave = ((CacheKey) getCacheKey()).lowestInZeroOctave;
        final Interval[] intervals = chromaticScale.intervals();
        
        // for IPN-names and MIDI-numbers, get EDO-12 tones starting at lowest tone
        final Tone[] templateTones = new EqualTemperament(lowestInZeroOctave).tones();
        final double baseToneFrequency = calculateBaseToneFrequency(intervals, templateTones);
        
        final JustTone[] justIntonationTones = new JustTone[templateTones.length];
        // first tone as Interval.UNISON
        int octave = 0; // is relative to lowestBaseName, different from IPN-octave!
        int toneIndex = 0;
        justIntonationTones[toneIndex] = new JustTone(
                templateTones[toneIndex], baseToneFrequency, octave, Intervals.UNISON, 0, 0);
        
        double normalCent = 0.0;
        int intervalIndex = 0;
        for (toneIndex++; toneIndex < justIntonationTones.length; toneIndex++, intervalIndex++) {
            final Interval interval = intervals[intervalIndex % intervals.length];
            final double frequency = interval.frequency(baseToneFrequency, octave);
            
            normalCent += 100.0;
            final double cent = interval.cent(octave);
            final double centDeviation = cent - normalCent;
            
            final boolean octaveReached = (interval == Intervals.OCTAVE);
            final Interval calculationInterval;
            if (octaveReached) {
                octave++;
                calculationInterval = Intervals.UNISON; // avoid 2/1 because octave will give the power-number of 2
            }
            else {
                calculationInterval = interval;
            }
            
            justIntonationTones[toneIndex] = new JustTone(
                    templateTones[toneIndex],
                    frequency,
                    octave,
                    calculationInterval,
                    (int) Math.round(cent),
                    (int) Math.round(centDeviation));
        }
        
        return justIntonationTones;
    }
    
    private double calculateBaseToneFrequency(Interval[] intervals, Tone[] templateTones) {
        // Reference tone is A4, we need to calculate the frequency of lowestBaseName,
        // which is the first tone in templateTones
        final int lowestAIndex = findLowestIndex(ToneSystem.REFERENCE_FREQUENCY_IPN_NAME, templateTones);
        final Tone lowestA = templateTones[lowestAIndex];
        final int a4Index = findIndex(ToneSystem.REFERENCE_FREQUENCY_IPN_NAME, templateTones);
        final Tone a4 = templateTones[a4Index];
        
        final int octaveDistance = a4.ipnOctave - lowestA.ipnOctave; // will always be A0 or A1
        final int intervalOfAIndex = lowestAIndex - 1; // -1: intervals start with second note as targetNote
        final Interval intervalOfA = (intervalOfAIndex >= 0) ? intervals[intervalOfAIndex] : Intervals.UNISON;
        final double ratioOfA4 = (double) intervalOfA.dividend(octaveDistance) / (double) intervalOfA.divisor();
        
        return referenceFrequency() / ratioOfA4; // division because we go down from A4 to start tone A0 or A1
        // Example C0: divide 440 Hertz by (5/3 * 2^4), where 5/3 * 16 = 80/3 is ratio of A4, C0 then must be 1/1
    }
    
    private int findLowestIndex(String ipnName, Tone[] equalTemperamentTones) {
        final String baseName = removeOctave(ipnName);
        return IntStream.range(0, equalTemperamentTones.length)
                .filter(index -> removeOctave(equalTemperamentTones[index].ipnName).equals(baseName))
                .findFirst()
                .orElseThrow();
    }
    private int findIndex(String ipnName, Tone[] equalTemperamentTones) {
        return IntStream.range(0, equalTemperamentTones.length)
            .filter(index -> equalTemperamentTones[index].ipnName.equals(ipnName))
            .findFirst()
            .orElseThrow();
    }


    /** Holds data specific to a tone in just-intonation. */
    public static class JustTone extends Tone
    {
        /** The <code>modalScaleStartIpnName</code>-based octave number of the tone (not IPN-octave!). */
        public final int octave;
        /** The interval this tone was calculated from (as upper tone). **/
        public final Interval interval;
        /** The deviation from the standard cent count where 1 semi-tone = 100 cent. **/
        public final int centDeviation;
        
        /**
         * @param tone the tone from equal-temperament to represent.
         * @param frequency the newly calculated just-intonation frequency for this tone.
         * @param octave the octave number of the tone, based on the lowest tone of the built scale (not IPN).
         * @param interval the interval this tone has been built from (as the interval's upper tone).
         */
        public JustTone(Tone tone, double frequency, int octave, Interval interval, int cent, int centDeviation) {
            super(tone.ipnName, frequency, tone.midiNumber, cent);
            this.octave = octave;
            this.interval = interval;
            this.centDeviation = centDeviation;
        }
        
        @Override
        public String toString() {
            return super.baseToString()+",\t"+
                    interval.ratioString(octave)+"\t"+
                    centToString()+"\t("+(centDeviation >= 0 ? "+" : "")+centDeviation+")";
        }
    }
}