package fri.music.differencetones.composer.strategy;

import java.util.List;
import fri.music.Tone;
import fri.music.differencetones.DifferenceToneInversions.TonePair;

public abstract class AbstractStrategy implements Strategy
{
    protected List<TonePair> generatingIntervals;
    protected int lastIndex;
    protected boolean considerAlternatives;
    protected boolean isRepeatedNote;
    
    protected void initialize(StrategyContext context) {
        // get list of possible intervals, sorted by wideness of interval and pitch
        // narrow interval first, high pitch first
        generatingIntervals = context.inversions().getIntervalsGenerating(context.note());
        if (generatingIntervals == null || generatingIntervals.size() <= 0)
            throw new IllegalArgumentException("Following note could not be mapped to an interval: "+context.note());
        
        lastIndex = generatingIntervals.size() - 1;
        considerAlternatives = (lastIndex > 0 && context.previousInterval() != null);
        isRepeatedNote = context.note().equals(context.previousNote());
    }


    protected final boolean isParallel(TonePair tonePair1, TonePair tonePair2) {
        return tonePair1.semitoneDistance() == tonePair2.semitoneDistance();
    }

    /** @return the number of semi-tones of the move, negative when downwards. */
    protected final int semitoneMove(Tone previousNote, Tone noteToMap) {
        return noteToMap.midiNumber - previousNote.midiNumber;
    }
    
    protected final boolean movesDownward(int semitones) {
        return semitones < 0;
    }
    
    
    protected record NearestSearchResult(TonePair result, int minimalDistance)
    {
        public NearestSearchResult() {
            this(null, Integer.MAX_VALUE);
        }
    }

    protected final NearestSearchResult getNearest(NearestSearchResult searchResult, TonePair previousInterval, TonePair intervalToCheck) {
        final int distance = distance(previousInterval, intervalToCheck);
        return (distance < searchResult.minimalDistance) ? new NearestSearchResult(intervalToCheck, distance) : searchResult;
    }

    protected final TonePair findIntervalWithOneNoteInCommon(TonePair previousInterval, List<TonePair> intervals) {
        final Tone previousUpper = previousInterval.upperTone();
        final Tone previousLower = previousInterval.lowerTone();
        NearestSearchResult searchResult = new NearestSearchResult();
        for (final TonePair tonePair : intervals) {
            final boolean upperEqual = tonePair.upperTone().equals(previousUpper);
            final boolean lowerEqual = tonePair.lowerTone().equals(previousLower);
            if (upperEqual != lowerEqual) // only one of them is true
                searchResult = getNearest(searchResult, previousInterval, tonePair);
        }
        return searchResult.result;
    }

    protected final int distance(TonePair previous, TonePair interval) {
        final int distanceUpper = semitoneMove(previous.upperTone(), interval.upperTone());
        final int distanceLower = semitoneMove(previous.lowerTone(), interval.lowerTone());
        return Math.abs(distanceUpper + distanceLower);
    }

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
//    protected final TonePair findIntervalNotContaining(Note note, List<TonePair> intervals) {
//        for (final TonePair tonePair : intervals)
//            if (containsBaseNote(tonePair, note) == false)
//                return tonePair;
//        return null;
//    }
//    
//    protected final boolean containsBaseNote(TonePair tonePair, Note noteToMap) {
//        final String ipnNameWithoutOctave = noteToMap.ipnNameWithoutOctave();
//        return tonePair.lowerTone().ipnNameWithoutOctave().equals(ipnNameWithoutOctave) ||
//               tonePair.upperTone().ipnNameWithoutOctave().equals(ipnNameWithoutOctave);
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