package fri.music.justintonation;

import static fri.music.ToneSystem.*;
import java.util.ArrayList;
import java.util.List;
import fri.music.JustIntonation.Intervals;
import fri.music.AbstractJustIntonation.Interval;
import fri.music.AbstractJustIntonation.ChromaticScale;
import fri.music.ToneSystem;

/**
 * Used for checking consistency of diatonic scales in just-intonation.
 */
public class DiatonicScaleOctaves
{
    public final String scaleName;
    /**
     * The modal scale starting from its start note, taken from a C-based chromatic scale. 
     * For instance PHRYGIAN will be E, F, G, A ...
     */
    public final List<IntervalWithOctave> whiteKeyIntervals = new ArrayList<>();
    /**
     * The modal scale starting from C, taken from a C-based chromatic scale. 
     * For instance PHRYGIAN will be C, C#, D#, F ...
     */
    public final List<IntervalWithOctave> cBasedIntervals = new ArrayList<>();
    /**
     * The chromatic scale from which this diatonic scale has been built.
     */
    protected final Interval[] chromaticIntervals;
    
    public DiatonicScaleOctaves(
            String scaleName, 
            boolean[] scaleLayout, // white key = true, black key = false
            ChromaticScale chromaticScale, 
            int scaleBaseSemitoneOffset,
            boolean alsoBuildCBasedScale)
    {
        this.scaleName = scaleName;
        
        final int chromaticScaleLength = chromaticScale.intervals().length; // 12
        if (chromaticScaleLength != ToneSystem.SEMITONES_PER_OCTAVE)
            throw new IllegalArgumentException("Given scale must have "+ToneSystem.SEMITONES_PER_OCTAVE+" steps but has "+chromaticScaleLength);
        
        // Interval array is 0-11, leaving out first tone, for a C-major scale:
        // C/C#=0, C/D=1, C/D#=2, C/E=3, C/F=4, C/F#=5, C/G=6, C/G#=7, C/A=8, C/A#=9, C/B=10, C/C=11
        // For calculations we need the UNISON base note at index 0
        chromaticIntervals = new Interval[chromaticScaleLength + 1];
        chromaticIntervals[0] = Intervals.UNISON;
        System.arraycopy(chromaticScale.intervals(), 0, chromaticIntervals, 1, chromaticScaleLength);
        
        final int octaves = 3; // needed when checking the MAJOR_SIXTH above highest note of LOCRIAN
        int previousWhiteKeyIndex = 0;
        // pick out those intervals that belong to diatonic scales (white keys)
        for (int semitoneIndex = 0; semitoneIndex < scaleLayout.length * octaves; semitoneIndex++) {
            if (scaleLayout[semitoneIndex % scaleLayout.length]) { // is a white key
                final int semitoneSteps = semitoneIndex - previousWhiteKeyIndex; // will be 1 or 2
                previousWhiteKeyIndex = semitoneIndex;
                
                whiteKeyIntervals.add(newDiatonicInterval(
                        semitoneIndex,
                        scaleBaseSemitoneOffset, 
                        chromaticScaleLength, 
                        semitoneSteps));
                
                if (alsoBuildCBasedScale) {
                    cBasedIntervals.add(newDiatonicInterval(
                            semitoneIndex,
                            0,
                            chromaticScaleLength,
                            semitoneSteps));
                }
            }
        }
    }

    private IntervalWithOctave newDiatonicInterval(
            int semitoneIndex, 
            int scaleBaseSemitoneOffset,
            int chromaticScaleLength, 
            int semitoneSteps)
    {
        final int currentIntervalIndex = semitoneIndex + scaleBaseSemitoneOffset;
        final int noteNameIndex = currentIntervalIndex % chromaticScaleLength;
        
        final boolean reachedCBasedOctave = (noteNameIndex == 0 && semitoneSteps > 0); // semitoneSteps for UNISON is 0
        // when reached 12, don't return to UNISON at 0, instead take OCTAVE at 12
        final int intervalIndex = reachedCBasedOctave ? chromaticScaleLength : noteNameIndex;
        // when reached 12, the OCTAVE still belongs to the lower octave
        final int octave = (currentIntervalIndex - (reachedCBasedOctave ? 1 : 0)) / chromaticScaleLength;
        
        // add also a C-based octave number for displaying IPN note-names
        final int ipnOctave = currentIntervalIndex / chromaticScaleLength;
        
        return new IntervalWithOctave(
                chromaticIntervals[intervalIndex],
                octave,
                ToneSystem.IPN_BASE_NAMES[noteNameIndex],
                semitoneSteps,
                ipnOctave);
    }
    
    /**
     * This does not cover MINOR_SECOND and MAJOR_SEVENTH!
     * @param interval the interval for which to return the number of semi-tone steps.
     * @return the number of semi-tone steps for given interval,
     *      e.g. 2 for MAJOR_SECOND.
     */
    protected int semitones(Interval interval) {
        if (isMajorSecond(interval)) return semitoneSteps(MAJOR_SECOND);
        if (isMinorThird(interval)) return semitoneSteps(MINOR_THIRD);
        if (isMajorThird(interval)) return semitoneSteps(MAJOR_THIRD);
        if (isFourth(interval)) return semitoneSteps(FOURTH);
        if (isFifth(interval)) return semitoneSteps(FIFTH);
        if (isMinorSixth(interval)) return semitoneSteps(MINOR_SIXTH);
        if (isMajorSixth(interval)) return semitoneSteps(MAJOR_SIXTH);
        if (isMinorSeventh(interval)) return semitoneSteps(MINOR_SEVENTH);
        if (isOctave(interval)) return semitoneSteps(OCTAVE);
        throw new IllegalArgumentException("Semitones for interval not implemented: "+interval);
    }

    /**
     * The maximum number of occurrences, without octaved repetitions,
     * of given interval in any diatonic scale.
     * This does NOT cover MINOR_SECOND and MAJOR_SEVENTH!
     * @param interval the interval for which to return the number of occurrences without octaved repetitions.
     * @return the maximum number of occurrences, without octaved repetitions,
     *      of given interval in any diatonic scale,
     *      e.g. 3 for MAJOR_THIRD (CE, FA, GB).
     */
    protected int maxOccurrences(Interval interval) {
        if (isMajorSecond(interval)) return 5; // CD, DE, FG, GA, AB
        if (isMinorThird(interval)) return 4; // DF, EG, AC, BD
        if (isMajorThird(interval)) return 3; // CE, FA, GB
        if (isFourth(interval)) return 6; // CF, DG, EA, GC, AD, BE
        if (isFifth(interval)) return 6; // CG, DA, EB, FC, GD, AE
        if (isMinorSixth(interval)) return 3; // EC, AF, BG
        if (isMajorSixth(interval)) return 4; // CA, DB, FD, GE
        if (isMinorSeventh(interval)) return 5; // DC, ED, GF, AG, BA
        if (isOctave(interval)) return 7; // CC, DD, EE, FF, GG, AA, BB
        throw new IllegalArgumentException("MaxOccurrences for interval not implemented: "+interval);
    }

    protected boolean isMajorSecond(Interval interval) {
        return interval.name().startsWith(MAJOR_SECOND);
    }
    protected boolean isMinorThird(Interval interval) {
        return interval.name().startsWith(MINOR_THIRD);
    }
    protected boolean isMajorThird(Interval interval) {
        return interval.name().startsWith(MAJOR_THIRD);
    }
    protected boolean isFourth(Interval interval) {
        return interval.name().startsWith(FOURTH);
    }
    protected boolean isFifth(Interval interval) {
        return interval.name().startsWith(FIFTH);
    }
    protected boolean isMinorSixth(Interval interval) {
        return interval.name().startsWith(MINOR_SIXTH);
    }
    protected boolean isMajorSixth(Interval interval) {
        return interval.name().startsWith(MAJOR_SIXTH);
    }
    protected boolean isMinorSeventh(Interval interval) {
        return interval.name().startsWith(MINOR_SEVENTH); 
    }
    protected boolean isOctave(Interval interval) {
        return interval.name().startsWith(OCTAVE);
    }
}