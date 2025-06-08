package fri.music;

import java.util.HashSet;
import java.util.Set;
import java.util.stream.Stream;

/**
 * Various 12-tone scales using different fractions for tone steps.
 * This class contains as much 12-tone-systems (called "tunings") 
 * as <code>ChromaticScales</code> elements were implemented. 
 * To distinguish their <code>name()</code> implementations, the
 * <code>chromaticScale.name()</code> from super-class gets appended
 * to the simple class-name.
 */
public class JustIntonation extends AbstractJustIntonation
{
    /**
     * Collection of different 12-tone tuning intervals for just-intonation.
     * By convention every enum-name must start with a name given in <code>ToneSystem.INTERVAL_NAMES.</code>
     * <p/>
     * The "5-limit" tuning restricts both dividend and divisor to integer-powers of 2, 3 and 5.
     * @see https://en.wikipedia.org/wiki/Five-limit_tuning
     */
    public enum Intervals implements Interval
    {
        UNISON             ( 1,  1),
        
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
        TRITONE_17_12      (17, 12),
        
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
        @Override
        public int basicDividend() {
            return dividend;
        }
        
        /** {@inheritDoc}. */
        @Override
        public int divisor() {
            return divisor;
        }
        
        @Override
        public String toString() {
            return name()+"("+ratioString(0)+", "+(int) Math.round(cent(0))+"¢)";
        }
    }
    
    
    /**
     * Some standard 12-tone scales.
     * @see https://en.wikipedia.org/wiki/Five-limit_tuning#Twelve-tone_scale
     */
    public enum ChromaticScales implements ChromaticScale
    {
        /** 5-limit-tuning, good for major tunes. */
        LIMIT_5_SYMMETRIC_1(new Interval[] {
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
        LIMIT_5_SYMMETRIC_2(new Interval[] {
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
        LIMIT_5_ASYMMETRIC(new Interval[] {
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
        HARMONIC_SERIES(new Interval[] {
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
        PYTHAGOREAN(new Interval[] {
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
        /** 7-limit tuning is very similar to 5-limit. */
        LIMIT_7(new Interval[] {
                Intervals.MINOR_SECOND_7LIMIT,
                Intervals.MAJOR_SECOND_7LIMIT,
                Intervals.MINOR_THIRD,
                Intervals.MAJOR_THIRD,
                Intervals.FOURTH,
                Intervals.TRITONE_AUG4_7LIMIT,
                //Interval.TRITONE_DIM5_7LIMIT,
                Intervals.FIFTH,
                Intervals.MINOR_SIXTH,
                Intervals.MAJOR_SIXTH,
                Intervals.MINOR_SEVENTH_7LIMIT,
                Intervals.MAJOR_SEVENTH,
                Intervals.OCTAVE,
            }),
        /** 17-limit tuning is very similar to 7-limit. */
        LIMIT_17(new Interval[] {
                Intervals.MINOR_SECOND_17LIMIT,
                Intervals.MAJOR_SECOND_7LIMIT,
                Intervals.MINOR_THIRD,
                Intervals.MAJOR_THIRD,
                Intervals.FOURTH,
                //Intervals.TRITONE_AUG4_7LIMIT,
                //Intervals.TRITONE_17_12,
                Intervals.TRITONE_DIM5_7LIMIT,
                Intervals.FIFTH,
                Intervals.MINOR_SIXTH,
                Intervals.MAJOR_SIXTH,
                Intervals.MINOR_SEVENTH_7LIMIT,
                Intervals.MAJOR_SEVENTH_17LIMIT,
                Intervals.OCTAVE,
            }),
        ;
        
        private final Interval[] intervals;
        
        private ChromaticScales(Interval[] intervals) {
            if (intervals.length != ToneSystem.SEMITONES_PER_OCTAVE)
                throw new IllegalArgumentException(
                        "A ChromaticScale must have exactly "+ToneSystem.SEMITONES_PER_OCTAVE+
                        " intervals, received "+intervals.length+" in "+name());
            
            final Set<Interval> checkSet = new HashSet<>();
            for (Interval interval : intervals)
                if (checkSet.contains(interval))
                    throw new IllegalArgumentException("Found repeated interval in scale: "+interval+" in "+name());
                else
                    checkSet.add(interval);
            
            this.intervals = intervals;
        }
        
        @Override
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

    /** Uses ChromaticScales.LIMIT_5_SYMMETRIC_1 */
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
     * Tones of "just intonation" tuning, where one octave is divided into 
     * 12 differently sized semi-tone steps.
     * For documentation of parameters see super-class.
     */
    public JustIntonation(
            double frequencyOfA4, 
            String baseToneIpnName, 
            String modalScaleStartIpnName, 
            int octaves, 
            ChromaticScale chromaticScale)
    {
        super(
            frequencyOfA4, 
            baseToneIpnName, 
            modalScaleStartIpnName, 
            octaves,
            (chromaticScale == null) ? ChromaticScales.LIMIT_5_SYMMETRIC_1 : chromaticScale
        );
    }
    
    @Override
    protected Tone[] createTones() {
        return createTones(Intervals.UNISON, Intervals.OCTAVE);
    }
}