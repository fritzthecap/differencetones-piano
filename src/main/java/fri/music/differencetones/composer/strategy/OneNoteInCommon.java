package fri.music.differencetones.composer.strategy;

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
}