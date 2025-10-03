package fri.music;

import java.util.Objects;
import java.util.stream.IntStream;

/**
 * Interfaces and default implementations for just-intonations.
 * Use this as base-class when your 12-tone scale builds upon fractions,
 * like exposed by <code>dividend()</code> and <code>divisor()</code>
 * in <code>Interval</code>.
 */
public abstract class AbstractJustIntonation extends AbstractToneSystem
{
    /** Fraction-driven interval for "just" 12-tone scales. */
    public interface Interval
    {
        /**
         * @return the name of this interval. Every such name must start with
         *      one of the names used in <code>ToneSystem.INTERVAL_NAMES</code>.
         */
        String name();
        
        /**
         * @return the interval's simple dividend, independent of octave.
         */
        int basicDividend();
        
        /**
         * @return the divisor of this interval.
         */
        int divisor();
        
        
        /**
         * Calculates the interval's dividend from octave and <code>basicDividend()</code>.
         * @param octave the 0-n octave for the dividend of this interval (this is NOT the IPN-octave!).
         * @return the dividend of this interval, according to given octave.
         */
        default int dividend(int octave) {
            return basicDividend() * (int) Math.pow(2, octave); // 2^0=1, 2^1=2, 2^2=4, 2^3=8, .... 
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
    }   // end interface Interval
    
    
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
         *      with step from first to second semi-tone (e.g. "C4" -> "C#4"),
         *      continuing with first to third, first to fourth, and so on,
         *      up to first to octave.
         */
        Interval[] intervals();
    }
    
    
    /** The interval collection this 12-tone tuning is built from. */
    public final ChromaticScale chromaticScale;
    
    /**
     * @param frequencyOfA4 the desired frequency of tone "A4" (IPN-name).
     * @param baseToneIpnName the build-note for the requested tone array e.g. "E3" or "G1".
     *      It is the tone the chromaticScale is built upon, calculated from <code>frequencyOfA4</code>.
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
    protected AbstractJustIntonation(
            double frequencyOfA4, 
            String baseToneIpnName, 
            String modalScaleStartIpnName, 
            int octaves, 
            ChromaticScale chromaticScale)
    {
        super(frequencyOfA4, baseToneIpnName, modalScaleStartIpnName, octaves);
        
        this.chromaticScale = Objects.requireNonNull(chromaticScale);
        
        if (this.chromaticScale.intervals().length != SEMITONES_PER_OCTAVE)
            throw new IllegalArgumentException("Can process only 12-tone scales: "+chromaticScale);
    }
    
    @Override
    public String name() {
        return super.name()+" "+chromaticScale.name();
    }
    
    @Override
    public Tone[] tones() {
        return AbstractToneSystem.tones(getOrCreateCachedTones(), modalScaleStartIpnName, octaves);
    }
    
    
    protected record CacheKey(double frequencyOfA4, String lowestInZeroOctave, ChromaticScale chromaticScale) {
    }
    
    @Override
    protected Object getCacheKey() {
        // use lowest start-note for caching tones
        final String lowestInZeroOctave = removeOctave(baseToneIpnName())+"0"; // e.g. "E0" from "E3"
        return new CacheKey(referenceFrequency(), lowestInZeroOctave, chromaticScale);
    }
    
    protected Tone[] createTones(Interval unisonInterval, Interval octaveInterval) {
        final String lowestInZeroOctave = ((CacheKey) getCacheKey()).lowestInZeroOctave();
        final Interval[] intervals = chromaticScale.intervals();
        
        // for IPN-names and MIDI-numbers, get ET-12 tones starting at lowest tone
        final Tone[] templateTones = new EqualTemperament(lowestInZeroOctave).tones();
        final double baseToneFrequency = calculateBaseToneFrequency(intervals, templateTones, unisonInterval);
        
        final JustTone[] justIntonationTones = new JustTone[templateTones.length];
        // first tone as Interval.UNISON
        int octave = 0; // is relative to lowestBaseName, different from IPN-octave!
        int toneIndex = 0;
        justIntonationTones[toneIndex] = new JustTone(
                templateTones[toneIndex], baseToneFrequency, octave, unisonInterval, 0, 0);
        
        double normalCent = 0.0;
        int intervalIndex = 0;
        for (toneIndex++; toneIndex < justIntonationTones.length; toneIndex++, intervalIndex++) {
            final Interval interval = intervals[intervalIndex % intervals.length];
            final double frequency = interval.frequency(baseToneFrequency, octave);
            
            normalCent += 100.0;
            final double cent = interval.cent(octave);
            final double centDeviation = cent - normalCent;
            
            final boolean octaveReached = (interval == octaveInterval);
            final Interval calculationInterval;
            if (octaveReached) {
                octave++;
                calculationInterval = unisonInterval; // avoid 2/1 because *octave* will give the power-number for 2
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
    
    private double calculateBaseToneFrequency(Interval[] intervals, Tone[] templateTones, Interval unisonInterval) {
        // Reference tone is A4, we need to calculate the frequency of lowestBaseName,
        // which is the first tone in templateTones
        final int lowestAIndex = findLowestIndex(ToneSystem.REFERENCE_FREQUENCY_IPN_NAME, templateTones);
        final Tone lowestA = templateTones[lowestAIndex];
        final int a4Index = findIndex(ToneSystem.REFERENCE_FREQUENCY_IPN_NAME, templateTones);
        final Tone a4 = templateTones[a4Index];
        
        final int octaveDistance = a4.ipnOctave - lowestA.ipnOctave; // will always be A0 or A1
        final int intervalOfAIndex = lowestAIndex - 1; // -1: intervals start with second note as targetNote
        final Interval intervalOfA = (intervalOfAIndex >= 0) ? intervals[intervalOfAIndex] : unisonInterval;
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
        private final int octave;
        /** The interval this tone was calculated from (as upper tone). **/
        private final Interval interval;
        /** The deviation from the standard cent count where 1 semi-tone = 100 cent. **/
        private final int centDeviation;
        
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
        
        /** @return the distance of this lower tone to given upper tone as fraction. */
        public long[] distance(JustTone upperTone) {
            return Interval.distance(interval, octave, upperTone.interval, upperTone.octave);
        }
        
        @Override
        public String toString() {
            return super.baseToString()+",\t"+
                    interval.ratioString(octave)+"\t"+
                    centToString()+"\t("+(centDeviation >= 0 ? "+" : "")+centDeviation+")";
        }
    }

}