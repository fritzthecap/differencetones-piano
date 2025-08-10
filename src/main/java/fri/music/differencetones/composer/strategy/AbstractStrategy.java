package fri.music.differencetones.composer.strategy;

import java.util.List;
import fri.music.Tone;
import fri.music.differencetones.DifferenceToneInversions.TonePair;

/**
 * Common functionality for strategies, 
 * usable by calling <code>initialize(StrategyContext)</code>.
 */
public abstract class AbstractStrategy implements Strategy
{
    /** Contains intervals that can generate the current note. */
    protected List<TonePair> generatingIntervals;
    /** The last index of generatingIntervals, size - 1. */
    protected int lastIndex;
    /** True when generatingIntervals contains more than one and a previous note exists. */
    protected boolean considerAlternatives;
    /** True when the preceding note is the same as the current one. */
    protected boolean isRepeatedNote;
    
    /** You MUST call this method to evaluate all fields visible for a sub-class! */
    protected void initialize(StrategyContext context) {
        // get list of possible intervals, sorted by wideness of interval and pitch
        // narrow interval first, high pitch first
        generatingIntervals = context.inversions().getIntervalsGenerating(context.note());
        if (generatingIntervals == null || generatingIntervals.size() <= 0)
            throw new IllegalArgumentException("Note '"+context.note()+"' could not be mapped to an interval!");
        
        lastIndex = generatingIntervals.size() - 1;
        considerAlternatives = (lastIndex > 0 && context.previousInterval() != null);
        isRepeatedNote = context.note().equals(context.previousNote());
    }


    /** @return true when given tone-pairs is a parallel move, i.e. both have the same semi-tone distance. */
    protected final boolean isParallel(TonePair tonePair1, TonePair tonePair2) {
        if (tonePair1 == null || tonePair2 == null)
            return false;
        return tonePair1.semitoneDistance() == tonePair2.semitoneDistance();
    }

    /** @return the number of semi-tones of the move, negative when downwards. */
    protected final int semitoneMove(Tone previousNote, Tone noteToMap) {
        return noteToMap.midiNumber - previousNote.midiNumber;
    }
    
    /** @return true when given semitones is smaller than zero. */
    protected final boolean movesDownward(int semitones) {
        return semitones < 0;
    }
    
    
    /**
     * Helper for search loops.
     * @param result the nearest solution found so far, or null when none was found.
     * @param minimalDistance the minimal distance found so far.
     */
    protected record NearestSearchResult(
            /** The solution found so far, or null when none was found. */
            TonePair result, 
            /** The minimal distance found so far. */
            int minimalDistance)
    {
        public NearestSearchResult() {
            this(null, Integer.MAX_VALUE);
        }
    }

    /**
     * For definition of nearness see <code>distance()</code>.
     * @param searchResult the current search state.
     * @param previousInterval the interval directly preceding the current note.
     * @param intervalToCheck an interval from generatingIntervals that should be checked for nearness to previousInterval.
     * @return given searchResult when previousInterval and intervalToCheck do not have a smaller distance
     *      than given searchResult, else a new result containing intervalToCheck and the smaller distance.
     */
    protected final NearestSearchResult getNearest(NearestSearchResult searchResult, TonePair previousInterval, TonePair intervalToCheck) {
        final int distance = distance(previousInterval, intervalToCheck);
        return (distance < searchResult.minimalDistance)
                ? new NearestSearchResult(intervalToCheck, distance)
                : searchResult;
    }

    /**
     * Minimal move of difference-tone intervals from tone to tone of the melody.
     * @return an average distance of given tone-pairs,
     *      calculated as the non-negative sum of the semi-tone distances
     *      of both the upper and the lower tones of the given pairs.
     */
    protected final int distance(TonePair previous, TonePair interval) {
        final int distanceUpper = semitoneMove(previous.upperTone(), interval.upperTone());
        final int distanceLower = semitoneMove(previous.lowerTone(), interval.lowerTone());
        return Math.abs(distanceUpper + distanceLower);
    }
    
    
//    /**
//     * This method will never return an interval that is parallel to given previousInterval.
//     * @return a tone-pair from intervals list that does not contain given tone, ignoring its octave.
//     */
//    protected final TonePair findIntervalNotContaining(Tone tone, TonePair previousInterval) {
//        final String ipnNameWithoutOctave = tone.ipnNameWithoutOctave();
//        final TonePair toBeNear = (previousInterval == null) ? bestByPitch : previousInterval;
//        NearestSearchResult searchResult = new NearestSearchResult();
//        for (final TonePair tonePair : generatingIntervals) {
//            if (containsIgnoringOctave(tonePair, ipnNameWithoutOctave) == false &&
//                    isParallel(tonePair, previousInterval) == false)
//                searchResult = getNearest(searchResult, toBeNear, tonePair);
//        }
//        return searchResult.result();
//    }
//    
//    /**
//     * @return true when given tone-pair contains given tone, ignoring its octave.
//     */
//    protected final boolean containsIgnoringOctave(TonePair tonePair, String ipnNameWithoutOctave) {
//        return tonePair.lowerTone().ipnNameWithoutOctave().equals(ipnNameWithoutOctave) ||
//               tonePair.upperTone().ipnNameWithoutOctave().equals(ipnNameWithoutOctave);
//    }
//
//    protected final boolean isDirection(int semitoneMove,  TonePair previousInterval, TonePair intervalToCheck) {
//        final int upperMove = semitoneMove(previousInterval.upperTone(), intervalToCheck.upperTone());
//        final int lowerMove = semitoneMove(previousInterval.lowerTone(), intervalToCheck.lowerTone());
//        final int bothMoves = upperMove + lowerMove;
//        
//        if (semitoneMove == 0) // no melody move
//            return bothMoves == 0; // both zero, or both go different directions by the same amount
//        
//        if (bothMoves == 0) // no interval move
//            return false; // semitoneMove is non-zero here, thus melody moves
//        
//        return movesDownward(bothMoves) == movesDownward(semitoneMove);
//    }
//
//    protected final TonePair directedMove(int semitoneMove, TonePair previousInterval, List<TonePair> intervals) {
//        NearestSearchResult searchResult = new NearestSearchResult(null, Integer.MAX_VALUE);
//        for (final TonePair tonePair : intervals)
//            if (isDirection(semitoneMove, previousInterval, tonePair) && isParallel(previousInterval, tonePair) == false)
//                searchResult = getNearest(searchResult, previousInterval, tonePair);
//        return searchResult.result();
//    }
//    
//    protected TonePair move(int semitoneMove, TonePair previousInterval, List<TonePair> intervals) {
//        final TonePair counterMove = directedMove(-semitoneMove, previousInterval, intervals);
//        final TonePair move = directedMove(semitoneMove, previousInterval, intervals);
//        return leastDistance(previousInterval, counterMove, move); // take the one with least semitone distance from previous
//    }
//
//    protected TonePair directedMove(int semitoneMove, TonePair previousInterval, List<TonePair> intervals) {
//        NearestSearchResult searchResult = new NearestSearchResult();
//        for (final TonePair tonePair : intervals)
//            if (isDirection(semitoneMove, previousInterval, tonePair) && isParallel(previousInterval, tonePair) == false)
//                searchResult = getNearest(searchResult, previousInterval, tonePair);
//
//        return searchResult.result();
//    }
//    
//    private TonePair leastDistance(TonePair previousInterval, TonePair interval1, TonePair interval2) {
//        if (interval1 == null && interval2 == null)
//            throw new IllegalArgumentException("Can not move down or up from "+previousInterval+", both successors are null!");
//        
//        if (interval2 == null)
//            return interval1; // counter-move
//        else if (interval1 == null)
//            return interval2; // move
//            
//        final int distance1 = distance(previousInterval, interval1);
//        final int distance2 = distance(previousInterval, interval2);
//        
//        return (distance1 < distance2) ? interval1 : interval2;
//    }
}