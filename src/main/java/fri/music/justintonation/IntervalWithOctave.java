package fri.music.justintonation;

import java.util.List;
import fri.music.JustIntonation.Interval;
import fri.music.MathUtils;

/**
 * Represents an interval and its octave number, which determines the calculation of its ratio.
 * Mind that the octave-number of <code>Interval.OCTAVE(2/1)</code> must be the one of its predecessor, 
 * not one more, while its successor (9/8) must have one more in its octave-number.
 */
public class IntervalWithOctave
{
    public static long[] distance(IntervalWithOctave lower, IntervalWithOctave upper) {
        return Interval.distance(lower.interval, lower.octave, upper.interval, upper.octave);
    }
    
    public static long[] difference(IntervalWithOctave lower, IntervalWithOctave upper) {
        return Interval.difference(lower.interval, lower.octave, upper.interval, upper.octave);
    }
    
    /**
     * @param scaleIntervals the scale (1-octave) to check if it contains the given difference-tone.
     * @param differenceRatio the difference-tone to check if one of its octaves is contained in given scale.
     * @return true when differenceTone, possibly multiplied by powers of 2, is in given scale.
     */
    public static IntervalWithOctave existsDifferenceToneInScale(
            List<IntervalWithOctave> scaleIntervals, 
            long[] differenceRatio)
    {
        final IntervalWithOctave firstTone = scaleIntervals.get(0);
        final double firstToneRatio = (double) firstTone.dividend() / (double) firstTone.divisor();
        final int divisor = (int) differenceRatio[1];
        int dividend = (int) differenceRatio[0];
        int octave = 0;
        // increase octave by multiplying with powers of 2 until greater equal first tone
        for (; (double) dividend / (double) divisor < firstToneRatio; octave++)
            dividend *= 2;
        
        final long[] reducedDifferenceTone = MathUtils.reduceFraction(dividend, divisor);
        
        for (int j = 0; j < scaleIntervals.size(); j++) {
            final IntervalWithOctave tone = scaleIntervals.get(j);
            final long[] reducedTone = MathUtils.reduceFraction(tone.dividend(), tone.divisor());
            
            if (reducedTone[0] == reducedDifferenceTone[0] && reducedTone[1] == reducedDifferenceTone[1])
                return new IntervalWithOctave(
                        tone.interval,
                        tone.octave - octave,
                        tone.targetNoteName,
                        tone.semitoneStepsFromPredecessor,
                        tone.ipnOctave - octave);
        }
        return null;
    }
    
    
    public final int octave;
    public final String targetNoteName;
    public final int semitoneStepsFromPredecessor;
    public final int ipnOctave;
    
    private final Interval interval; // wrap interval!
    
    /**
     * @param interval the interval to represent.
     * @param octave the C-based 0-n octave number of the interval.
     *      Mind that Interval.OCTAVE(2/1) has the octave-number of its predecessor,
     *      while its successor (9/8) has an incremented octave-number!
     * @param targetNoteName for the first MAJOR_SECOND interval of a C-major scale this will be "D",
     *      but for the leading Interval.UNISON of a C-major scale this will be "C".
     * @param semitoneStepsFromPredecessor 1 or 2, 0 for Interval.UNISON, the number of semi-tone steps from predecessor to this.
     * @param ipnOctave the C-based 0-n octave number where the interval's targetNote is located,
     *      different from octave only on Interval.OCTAVE.
     */
    public IntervalWithOctave(
            Interval interval, 
            int octave, 
            String targetNoteName, 
            int semitoneStepsFromPredecessor, 
            int ipnOctave) {
        this.interval = interval;
        this.octave = octave;
        this.targetNoteName = targetNoteName;
        this.semitoneStepsFromPredecessor = semitoneStepsFromPredecessor;
        this.ipnOctave = ipnOctave;
    }

    /** Considers the octave of this interval. */
    public int dividend() {
        return interval.dividend(octave);
    }
    /** The interval's divisor. */
    public int divisor() {
        return interval.divisor();
    }
    /** The interval's ratio in octave. */
    public String ratioString() {
        return interval.ratioString(octave);
    }
    
    @Override
    public String toString() {
        return targetNoteName+"["+ipnOctave+"] "+ratioString()+" ("+(int) Math.round(interval.cent(octave))+"¢)";
    }
}