package fri.music.differencetones.composer.strategy;

import fri.music.differencetones.DifferenceToneInversions.TonePair;

/**
 * When <code>mustAvoidParallelMove</code> is false, this delivers null.
 * Else it decrements <code>indexByPitch</code> by one when the melody is going downwards,
 * which suggests to use a narrower interval than <code>bestByPitch</code>, 
 * or it increments <code>indexByPitch</code> by one when the melody is going upwards,
 * which suggests to use a wider interval than <code>bestByPitch</code>. 
 * In case this violates array bounds, the opposite is done to avoid a parallel solution.
 * <p/>
 * Sort-order 40.
 */
public class NonParallel extends AbstractNonParallel
{
    @Override
    public TonePair solution(StrategyContext context) {
        initialize(context);
        
        if (mustAvoidParallelMove == true) {
            final boolean downward = movesDownward(semitoneMove(context.previousNote(), context.note()));
            // prefer a narrower/higher interval when going down
            int betterIndex = downward ? indexByPitch - 1 : indexByPitch + 1;
            // but do the contrary when violating limits
            betterIndex = (betterIndex > lastIntervalIndex) ? indexByPitch - 1 : (betterIndex < 0) ? indexByPitch + 1 : betterIndex;
            return generatingIntervals.get(betterIndex);
        }
        return null;
    }
    
    @Override
    public int suggestedPriority() {
        return 40;
    }
}