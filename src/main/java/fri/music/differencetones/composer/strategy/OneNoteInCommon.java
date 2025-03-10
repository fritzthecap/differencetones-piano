package fri.music.differencetones.composer.strategy;

import fri.music.differencetones.DifferenceToneInversions.TonePair;

public class OneNoteInCommon extends AbstractStrategy
{
    @Override
    public TonePair solution(StrategyContext context) {
        initialize(context);
        
        return considerAlternatives 
                ? findIntervalWithOneNoteInCommon(context.previousInterval(), generatingIntervals)
                : null;
    }
    
    @Override
    public int sortOrder() {
        return 20;
    }
}