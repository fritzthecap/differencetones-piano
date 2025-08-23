package fri.music.differencetones;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import fri.music.AbstractToneSystem;
import fri.music.Tone;
import fri.music.ToneSystem;

/**
 * Finds one or more intervals that can generate a certain difference tone.
 * <p/>
 * Mind that due to a limited octave range the higher difference-tones will be
 * represented by less intervals than the lower ones.
 * For a tone-range of 4 octaves you can get about 1.3 low difference-tone
 * octaves of various intervals that range from MINOR_THIRD up to MAJOR_SIXTH.
 */
public class DifferenceToneInversions extends DifferenceTones
{
    /**
     * Builds a sufficient range of tones to model given melody with difference tones.
     * A melody with 1.3 octaves tone-range requires about 4 octaves above its lowest note.
     * @param melody required, the melody to model with difference tones.
     * @param toneStock required, the 12-tone system to be used for the melody and its difference tones.
     * @param smallestSemitoneDistance optional, the number of semi-tones representing the 
     *      smallest difference-tone interval to provide in returned tone-inversions.
     *      Default is MINOR_THIRD.
     * @param biggestSemitoneDistance optional, the number of semi-tones representing the 
     *      biggest difference-tone interval to provide in returned tone-inversions.
     *      Default is MAJOR_SIXTH.
     * @param deviationTolerance required, the tolerance for finding difference-tones.
     * @return the intervals (inversions) that can represent given melody.
     * @throws IllegalArgumentException when melody and its inversions do not fit into toneStock.
     */
    public static DifferenceToneInversions toneRangeFor(
            Tone[] melody, 
            Tone[] toneStock, 
            int smallestSemitoneDistance,
            int biggestSemitoneDistance,
            double deviationTolerance)
    {
        // Find octave range of melody and calculate a subset of tones fitting to that range.
        final Tone[] sortedMelody = Arrays.copyOf(melody, melody.length);
        Arrays.sort(sortedMelody, (n1, n2) -> n1.midiNumber - n2.midiNumber);
        final Tone lowest  = findNonRest(sortedMelody, true);
        final Tone highest = findNonRest(sortedMelody, false);
        final int numberOfSemitones = highest.midiNumber - lowest.midiNumber;
        final double melodyOctaves = (double) numberOfSemitones / (double) ToneSystem.SEMITONES_PER_OCTAVE;
        
        // we need the tone below lowest melody note to make deviation work also for bottom
        int lowestIndexInToneStock = Arrays.binarySearch(toneStock, lowest, (t1, t2) -> t1.midiNumber - t2.midiNumber);
        lowestIndexInToneStock--; // go one deeper
        if (lowestIndexInToneStock < 0)
            throw new IllegalArgumentException("Tone stock is too small for lowest melody note "+lowest.ipnName+", its lowest is "+toneStock[0].ipnName);
        
        final String lowestIpnName = toneStock[lowestIndexInToneStock].ipnName;
        
        // melodyOctaves up to 1.3 -> 4 octaves, up to 2.3 -> 5, up to 3.3 -> 6, ...
        final int additionalOctavesTo4 = Math.min(0, (int) Math.ceil(melodyOctaves - 1.3));
        toneStock = AbstractToneSystem.tones(toneStock, lowestIpnName, 4 + additionalOctavesTo4);
        
        return new DifferenceToneInversions(
            new DifferenceToneInversions.Configuration(
                toneStock, 
                (smallestSemitoneDistance > 0) ? smallestSemitoneDistance : Configuration.DEFAULT_SMALLEST_SEMITONE_STEPS,
                (biggestSemitoneDistance > 0) ? biggestSemitoneDistance : Configuration.DEFAULT_BIGGEST_SEMITONE_STEPS,
                deviationTolerance
            )
        );
    }

    private static Tone findNonRest(Tone[] sortedMelody, boolean lowest) {
        final int minimumIndex = 0;
        final int maximumIndex = sortedMelody.length - 1;
        final int startIndex = lowest ? minimumIndex : maximumIndex;
        final int endIndex = lowest ? maximumIndex : minimumIndex;
        for (int i = startIndex; i != endIndex; i += (lowest ? 1 : -1)) {
            final Tone tone = sortedMelody[i];
            if (tone.ipnName.equals(ToneSystem.REST_SYMBOL) == false)
                return tone;
        }
        throw new IllegalArgumentException("Only rests in melody?");
    }


    /**
     * Configuration of this class, to be passed to constructor.
     * @param tones the tone-system the difference-tones should be generated from.
     * @param smallestSemitoneDistance the smallest number of semi-tones the interval-tones may be apart.
     * @param biggestSemitoneDistance the biggest number (inclusive) of semi-tones the interval-tones may be apart.
     * @param deviationTolerance the maximum deviation when searching for difference tones, between 0.0 and 0.45.
     */
    public record Configuration(
            Tone[] tones, 
            int smallestSemitoneDistance, 
            int biggestSemitoneDistance, 
            double deviationTolerance)
    {
        /** Smallest recommendable interval for difference-tone inversions is MINOR_THIRD. */
        public static final int DEFAULT_SMALLEST_SEMITONE_STEPS = 
                ToneSystem.semitoneSteps(ToneSystem.MINOR_THIRD);
        /** Biggest recommendable interval for difference-tone inversions is MAJOR_SIXTH. */
        public static final int DEFAULT_BIGGEST_SEMITONE_STEPS = 
                ToneSystem.semitoneSteps(ToneSystem.MAJOR_SIXTH); // the MINOR_SEVENTH difference tone is very hard to hear!
        
        public Configuration(Tone[] tones) {
            this(tones, DEFAULT_DEVIATION);
        }
        public Configuration(Tone[] tones, double deviationTolerance) {
            this(tones, 
                DEFAULT_SMALLEST_SEMITONE_STEPS, 
                DEFAULT_BIGGEST_SEMITONE_STEPS, 
                (deviationTolerance < 0.0) ? DEFAULT_DEVIATION : deviationTolerance);
        }
        public Configuration(Tone[] tones, int smallestSemitoneDistance, int biggestSemitoneDistance) {
            this(tones, 
                (smallestSemitoneDistance <= 0) ? DEFAULT_SMALLEST_SEMITONE_STEPS : smallestSemitoneDistance, 
                (biggestSemitoneDistance  <= 0) ? DEFAULT_BIGGEST_SEMITONE_STEPS : biggestSemitoneDistance,
                DEFAULT_DEVIATION);
        }
    }
    
    
    /** Representation of a difference-tone through two tones. */
    public record TonePair(Tone lowerTone, Tone upperTone)
    {
        /** Rest constructor. */
        public TonePair() {
            this(null, null);
        }
        public int semitoneDistance() {
            if (isRest())
                return 0;
            return upperTone.midiNumber - lowerTone.midiNumber;
        }
        public String intervalName() {
            if (isRest())
                return "NONE";
            return ToneSystem.intervalName(semitoneDistance());
        }
        public boolean isRest() {
            return lowerTone == null;
        }
        @Override
        public final String toString() {
            if (isRest())
                return ToneSystem.REST_SYMBOL;
            return intervalName()+" "+lowerTone.ipnName+"-"+upperTone.ipnName;
        }
    }
    
    
    private final Configuration config;
    private final Map<Tone,List<TonePair>> differenceToneToGeneratingTones = new LinkedHashMap<>();
    
    /**
     * Constructs data for finding difference-tone inversions.
     * Mind that <code>>removeDissonant()</code> is <b>not</b> done automatically on construction,
     * you need to call this explicitly! (To avoid e.g. dirty MAJOR_THIRD in EDO-12.)
     * @param config the configuration to use when searching difference-tone inversions.
     */
    public DifferenceToneInversions(Configuration config) {
        super(config.tones, config.deviationTolerance, true); // true: need to find only primary difference tones
        this.config = config;
    }
    
    /**
     * @param differenceToneIpnName the IPN-name of the difference-tone to find generating intervals for.
     * @return the intervals that would generate given difference-tone, smaller intervals and higher pitch sorted at head.
     */
    public List<TonePair> getIntervalsGenerating(String differenceToneIpnName) {
        return getIntervalsGenerating(forIpnName(differenceToneIpnName));
    }
    
    /**
     * @param differenceToneMidiNumber the MIDI-number of the difference-tone to find generating intervals for.
     * @return the intervals that would generate given difference-tone, smaller intervals and higher pitch sorted at head.
     */
    public List<TonePair> getIntervalsGenerating(int differenceToneMidiNumber) {
        return getIntervalsGenerating(forMidiNoteNumber(differenceToneMidiNumber));
    }
    
    /**
     * @param differenceTone the difference-tone to find generating intervals for.
     * @return the intervals that would generate given difference-tone, smaller intervals and higher pitch sorted at head.
     */
    public List<TonePair> getIntervalsGenerating(Tone differenceTone) {
       return differenceToneToGeneratingTones().get(Objects.requireNonNull(differenceTone));
    }
    
    /**
     * @return all difference-tones that all tones in all octaves of this tone-system can generate,
     *      sorted by MIDI-number (lowest tone at head).
     */
    public List<Tone> differenceTones() {
        final List<Tone> keys = new ArrayList<>(differenceToneToGeneratingTones().keySet());
        Collections.sort(keys, (tone1, tone2) -> tone1.midiNumber - tone2.midiNumber);
        return keys;
    }
    
    /**
     * Removes all intervals that have upper or lower tone just one semi-tone away from their difference-tone,
     * independent of octave. Thus C4 = B6-D#6 (dirty MAJOR_THIRD in EDO-12) would be removed, as B is next to C.
     * @param alsoInFifthRange remove also intervals whose tones are too close to the fifth of the difference tone.
     */
    public void removeDissonant(boolean alsoInFifthRange) {
        for (final Map.Entry<Tone,List<TonePair>> entry : differenceToneToGeneratingTones().entrySet()) {
            final Tone differenceTone = entry.getKey();
            
            final Iterator<TonePair> iterator = entry.getValue().iterator();
            while (iterator.hasNext()) {
                final TonePair interval = iterator.next();
                if (isOneSemitoneAway(differenceTone, interval.upperTone()) ||
                        isOneSemitoneAway(differenceTone, interval.lowerTone()) ||
                        (alsoInFifthRange &&
                            (isFifthOneSemitoneAway(differenceTone, interval.upperTone()) ||
                            isFifthOneSemitoneAway(differenceTone, interval.lowerTone()))))
                    iterator.remove();

            }
        }
    }
    
    private Map<Tone,List<TonePair>> differenceToneToGeneratingTones() {
        if (differenceToneToGeneratingTones.isEmpty())
            initialize();
        return differenceToneToGeneratingTones;
    }

    private void initialize() {
        // in just-intonation, any tone-pair can give another difference tone, nearly no predictions are possible
        // build index of intervals from given tones
        // C-D, C-D#, C-E ... C-A#; C#-#D C#-E, ...
        for (int lowerIndex = 2; // there won't be a difference tone below third semi-tone
                lowerIndex < tones.length - config.smallestSemitoneDistance; // stop one before last
                lowerIndex++) 
        {
            final Tone lowerTone = tones[lowerIndex];
            
            for (int semitoneOffset = config.smallestSemitoneDistance;
                    semitoneOffset <= config.biggestSemitoneDistance && lowerIndex + semitoneOffset < tones.length; 
                    semitoneOffset++)
            {
                final Tone upperTone = tones[lowerIndex + semitoneOffset];
                final Tone differenceTone = findDifferenceTones(lowerTone, upperTone)[0]; // we requested just one
                
                if (differenceTone != null) {
                    final TonePair tonePair = new TonePair(lowerTone, upperTone);
                    
                    List<TonePair> list = differenceToneToGeneratingTones.get(differenceTone);
                    if (list == null) {
                        list = new ArrayList<TonePair>();
                        differenceToneToGeneratingTones.put(differenceTone, list);
                    }
                    list.add(0, tonePair); // add to head, smaller intervals to front
                }
            }
        }
    }
    
    private boolean isOneSemitoneAway(Tone differenceTone, Tone intervalTone) {
        final int minorSecond = ToneSystem.semitoneSteps(ToneSystem.MINOR_SECOND);
        final int majorSeventh = ToneSystem.semitoneSteps(ToneSystem.MAJOR_SEVENTH);
        final int distance = (intervalTone.midiNumber - differenceTone.midiNumber) % ToneSystem.SEMITONES_PER_OCTAVE;
        return distance == minorSecond || distance == majorSeventh;
    }
    
    private boolean isFifthOneSemitoneAway(Tone differenceTone, Tone intervalTone) {
        final int tritone = ToneSystem.semitoneSteps(ToneSystem.TRITONE);
        final int minorSixth = ToneSystem.semitoneSteps(ToneSystem.MINOR_SIXTH);
        final int distance = (intervalTone.midiNumber - differenceTone.midiNumber) % ToneSystem.SEMITONES_PER_OCTAVE;
        return distance == tritone || distance == minorSixth;
    }
}