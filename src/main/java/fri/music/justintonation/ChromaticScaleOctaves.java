package fri.music.justintonation;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.IntStream;
import fri.music.JustIntonation.Intervals;
import fri.music.JustIntonation.Interval;
import fri.music.JustIntonation.ChromaticScale;
import fri.music.ToneSystem;

/**
 * Chromatic scale octaves, starting with just one Interval.UNISON, 
 * having subsequent Interval.OCTAVE instances.
 */
public class ChromaticScaleOctaves
{
    private final List<IntervalWithOctave> intervals;
    
    public ChromaticScaleOctaves(ChromaticScale chromaticScale, int octaves) {
        final int scaleLength = chromaticScale.intervals().length;
        final int listLength = scaleLength * octaves;
        this.intervals = new ArrayList<>(listLength);
        
        intervals.add(new IntervalWithOctave(Intervals.UNISON, 0, ToneSystem.IPN_BASE_NAMES[0], 0, 0));
        
        for (int i = 0; i < listLength; i++) {
            final int intervalIndex = i % scaleLength; // always 0-11
            final Interval interval = chromaticScale.intervals()[intervalIndex];
            
            final int indexPlusOne = i + 1; // C already added, first note is C#
            final int noteNameIndex = indexPlusOne % scaleLength;
            final String targetNoteName = ToneSystem.IPN_BASE_NAMES[noteNameIndex];
            
            final int octave = (indexPlusOne - (noteNameIndex == 0 ? 1 : 0)) / scaleLength;
            final int ipnOctave = indexPlusOne / scaleLength;
            
            intervals.add(new IntervalWithOctave(interval, octave, targetNoteName, 1, ipnOctave));
        }
    }

    public IntervalWithOctave existsDifferenceToneInScale(String lowerNote, String upperNote) {
        final int lowerIndex = IntStream.range(0, intervals.size())
                .filter(index -> intervals.get(index).targetNoteName.equals(lowerNote))
                .findFirst()
                .orElseThrow();
        final int upperIndex = IntStream.range(lowerIndex + 1, intervals.size())
                .filter(index -> intervals.get(index).targetNoteName.equals(upperNote))
                .findFirst()
                .orElseThrow();
        
        final IntervalWithOctave lower = intervals.get(lowerIndex);
        final IntervalWithOctave upper = intervals.get(upperIndex);
        final long[] differenceRatio = IntervalWithOctave.difference(lower, upper);
        
        return IntervalWithOctave.existsDifferenceToneInScale(intervals, differenceRatio);
    }
}
