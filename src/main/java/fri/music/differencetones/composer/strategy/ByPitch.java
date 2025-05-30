package fri.music.differencetones.composer.strategy;

import fri.music.differencetones.DifferenceToneInversions.TonePair;

/**
 * Delivers <code>bestByPitch</code> as solution when 
 * <code>mustAvoidParallelMove</code> is false, else null.
 * <p/>
 * Sort-order 30.
 */
public class ByPitch extends AbstractNonParallel
{
    @Override
    public TonePair solution(StrategyContext context) {
        initialize(context);
        
        return (mustAvoidParallelMove == false) ? bestByPitch : null;
    }
    
    @Override
    public int suggestedPriority() {
        return 30;
    }
}