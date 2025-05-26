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
 * Finds intervals that generate a certain difference tone.
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
     * @param melody the melody to model with difference tones.
     * @param toneSystem the tuning system to be used for the melody and its difference tones.
     * @param deviationTolerance the tolerance for finding difference-tones.
     * @return the sufficient range of tones for given melody.
     */
    public static DifferenceToneInversions toneRangeFor(
            Tone[] melody, 
            AbstractToneSystem toneSystem, 
            int smallestSemitoneDistance,
            int biggestSemitoneDistance,
            double deviationTolerance)
    {
        // Find octave range of melody and calculate a subset of tones fitting to that range.
        // An 1.3 octaves melody requires about 4 octaves above its lowest note. 
        final Tone[] sortedMelody = Arrays.copyOf(melody, melody.length);
        Arrays.sort(sortedMelody, (n1, n2) -> n1.midiNumber - n2.midiNumber);
        final Tone lowest  = sortedMelody[0];
        final Tone highest = sortedMelody[sortedMelody.length - 1];
        final int numberOfSemitones = highest.midiNumber - lowest.midiNumber;
        final double melodyOctaves = (double) numberOfSemitones / (double) ToneSystem.SEMITONES_PER_OCTAVE;
        
        // up to 1.3 -> 4 octaves, up to 2.3 -> 5, up to 3.3 -> 6, ...
        final int additionalOctavesTo4 = Math.min(0, (int) Math.ceil(melodyOctaves - 1.3));
        final Tone[] tones = toneSystem.tones(toneSystem.tones(), lowest.ipnName, 4 + additionalOctavesTo4);
        
        return new DifferenceToneInversions(
            new DifferenceToneInversions.Configuration(
                tones, 
                (smallestSemitoneDistance > 0) ? smallestSemitoneDistance : Configuration.DEFAULT_SMALLEST_SEMITONE_STEPS,
                (biggestSemitoneDistance > 0) ? biggestSemitoneDistance : Configuration.DEFAULT_BIGGEST_SEMITONE_STEPS,
                deviationTolerance
            )
        );
    }


    /**
     * Configuration of this class, to be passed to constructor.
     * @param tones the tone-system the difference-tones should be generated from.
     * @param smallestSemitoneDistance the smallest number of semi-tones the interval-tones may be apart.
     * @param biggestSemitoneDistance the biggest number (inclusive) of semi-tones the interval-tones may be apart.
     * @param deviationTolerance allowed range is 0.0 - 0.45, the maximum deviation when searching for difference tones.
     */
    public record Configuration(
            Tone[] tones, 
            int smallestSemitoneDistance, 
            int biggestSemitoneDistance, 
            double deviationTolerance)
    {
        public static final int DEFAULT_SMALLEST_SEMITONE_STEPS = 
                ToneSystem.semitoneSteps(ToneSystem.MINOR_THIRD);
        public static final int DEFAULT_BIGGEST_SEMITONE_STEPS = 
                ToneSystem.semitoneSteps(ToneSystem.MAJOR_SIXTH); // the MINOR_SEVENTH difference tone is very hard to hear!
        
        public Configuration(Tone[] tones) {
            this(tones, DEFAULT_DEVIATION_TOLERANCE);
        }
        public Configuration(Tone[] tones, double deviationTolerance) {
            this(tones, 
                DEFAULT_SMALLEST_SEMITONE_STEPS, 
                DEFAULT_BIGGEST_SEMITONE_STEPS, 
                (deviationTolerance < 0.0) ? DEFAULT_DEVIATION_TOLERANCE : deviationTolerance);
        }
        public Configuration(Tone[] tones, int smallestSemitoneDistance, int biggestSemitoneDistance) {
            this(tones, 
                (smallestSemitoneDistance <= 0) ? DEFAULT_SMALLEST_SEMITONE_STEPS : smallestSemitoneDistance, 
                (biggestSemitoneDistance  <= 0) ? DEFAULT_BIGGEST_SEMITONE_STEPS : biggestSemitoneDistance,
                DEFAULT_DEVIATION_TOLERANCE);
        }
    }
    
    /** Representation of the difference-tone inversion. */
    public record TonePair(Tone lowerTone, Tone upperTone)
    {
        public int semitoneDistance() {
            return upperTone.midiNumber - lowerTone.midiNumber;
        }
        public String intervalName() {
            return ToneSystem.intervalName(semitoneDistance());
        }
        @Override
        public final String toString() {
            return intervalName()+" "+lowerTone.ipnName+"-"+upperTone.ipnName;
        }
    }
    
    
    private final Configuration config;
    private final Map<Tone,List<TonePair>> differenceToneToGeneratingTones = new LinkedHashMap<>();
    
    public DifferenceToneInversions(Configuration config) {
        super(config.tones, config.deviationTolerance, true); // true: need to find only primary difference tones
        this.config = config;
        initialize();
    }
    
    private void initialize() {
        // in just-intonation, any tone-pair can give another difference tone, nearly no prediction-rules are possible
        // build index of intervals from given tones
        // C-D, C-D#, C-E ... C-A#; C#-#D C#-E, ...
        for (int lowerIndex = 2; // there won't be a difference tone below second semi-tone
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
                    //System.err.println(differenceTone.ipnName+" => "+tonePair);
                    
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
       return differenceToneToGeneratingTones.get(Objects.requireNonNull(differenceTone));
    }
    
    /**
     * @return all difference-tones that all tones in all octaves of this tone-system can generate,
     *      sorted by MIDI-number (lowest tone at head).
     */
    public List<Tone> differenceTones() {
        final List<Tone> keys = new ArrayList<>(differenceToneToGeneratingTones.keySet());
        Collections.sort(keys, (tone1, tone2) -> tone1.midiNumber - tone2.midiNumber);
        return keys;
    }
    
    /**
     * Removes all intervals that have upper or lower tone just one semi-tone away from their difference-tone,
     * independent of octave. Thus C4 = B6-D#6 (occurs in EDO-12) would be removed, as B is next to C.
     * @param alsoFifthRange remove also intervals whose tones are too close to the fifth of the difference tone.
     */
    public void removeDissonant(boolean alsoFifthRange) {
        for (final Map.Entry<Tone,List<TonePair>> entry : differenceToneToGeneratingTones.entrySet()) {
            final Tone differenceTone = entry.getKey();
            
            final Iterator<TonePair> iterator = entry.getValue().iterator();
            while (iterator.hasNext()) {
                final TonePair interval = iterator.next();
                if (isOneSemitoneAway(differenceTone, interval.upperTone()) ||
                        isOneSemitoneAway(differenceTone, interval.lowerTone()) ||
                        (alsoFifthRange &&
                            (isFifthOneSemitoneAway(differenceTone, interval.upperTone()) ||
                            isFifthOneSemitoneAway(differenceTone, interval.lowerTone()))))
                    iterator.remove();

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