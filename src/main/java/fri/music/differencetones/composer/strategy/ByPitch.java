package fri.music.differencetones.composer.strategy;

import fri.music.differencetones.DifferenceToneInversions.TonePair;

public class ByPitch extends AbstractNonParallel
{
    @Override
    public TonePair solution(StrategyContext context) {
        initialize(context);
        
        return (mustAvoidParallelMove == false) ? bestByPitch : null;
    }
    
    @Override
    public int sortOrder() {
        return 30;
    }
}