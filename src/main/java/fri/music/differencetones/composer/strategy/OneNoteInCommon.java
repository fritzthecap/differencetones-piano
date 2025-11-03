package fri.music.differencetones.composer.strategy;

import fri.music.Tone;
import fri.music.differencetones.DifferenceToneInversions.TonePair;

/**
 * Looks for a tone-pair in generatingIntervals that has at least 
 * one note in common with the preceding solution (when present).
 * <p/>
 * Sort-order 20.
 */
public class OneNoteInCommon extends AbstractStrategy
{
    @Override
    public TonePair solution(StrategyContext context) {
        initialize(context);
        
        return considerAlternatives 
                ? findIntervalWithOneNoteInCommon(context.previousInterval())
                : null;
    }
    
    @Override
    public int suggestedPriority() {
        return 20;
    }
    
    /**
     * This method will never return an interval that is parallel to given previousInterval.
     * @return the nearest tone-pair to previousInterval in intervals list
     *      that also has one note but not both notes in common with previousInterval.
     */
    private final TonePair findIntervalWithOneNoteInCommon(TonePair previousInterval) {
        final Tone previousUpper = previousInterval.upperTone();
        final Tone previousLower = previousInterval.lowerTone();
        NearestSearchResult searchResult = new NearestSearchResult();
        for (final TonePair tonePair : generatingIntervals) {
            final boolean upperEqual = tonePair.upperTone().ipnName.equals(previousUpper.ipnName);
            final boolean lowerEqual = tonePair.lowerTone().ipnName.equals(previousLower.ipnName);
            if (upperEqual != lowerEqual) // only one of them is true
                searchResult = getNearest(searchResult, previousInterval, tonePair);
        }
        return searchResult.result();
    }
}