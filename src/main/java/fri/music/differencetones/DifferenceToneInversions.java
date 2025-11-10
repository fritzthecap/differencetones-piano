package fri.music.differencetones;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
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
        public TonePair(Tone lowerTone) {
            this(lowerTone, null);
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
            return upperTone == null;
        }
        @Override
        public final String toString() {
            if (isRest())
                return lowerTone.ipnName;
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
     * CAUTION: do not pass a <code>Note</code> in case <code>Note.hashCode()</code> 
     * or <code>Note.equals()</code> are different from <code>Tone</code>!
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
        // build map of intervals for all tones of given tone-range,
        // for example with MINOR_THIRD and MAJOR_SIXTH: C-D#, C-E, C-F ... C-A; C#-E, C#-F, ...
        final int lowerLimit = tones.length - config.smallestSemitoneDistance; // stop when inner loop would run empty
        for (int lowerIndex = 0; lowerIndex < lowerLimit; lowerIndex++) {
            final Tone lowerTone = tones[lowerIndex];
            
            for (int semitoneOffset = config.smallestSemitoneDistance;
                    semitoneOffset <= config.biggestSemitoneDistance && lowerIndex + semitoneOffset < tones.length; 
                    semitoneOffset++)
            {
                final Tone upperTone = tones[lowerIndex + semitoneOffset];
                final Tone differenceTone = findDifferenceTones(lowerTone, upperTone)[0]; // [0]: just primary difference-tone
                
                if (differenceTone != null) {
                    final TonePair tonePair = new TonePair(lowerTone, upperTone);
                    
                    List<TonePair> list = differenceToneToGeneratingTones.get(differenceTone);
                    if (list == null) {
                        list = new ArrayList<>();
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