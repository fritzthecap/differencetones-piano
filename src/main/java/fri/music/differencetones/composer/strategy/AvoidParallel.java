package fri.music.differencetones.composer.strategy;

import fri.music.differencetones.DifferenceToneInversions.TonePair;

public class AvoidParallel extends AbstractNonParallel
{
    @Override
    public TonePair solution(StrategyContext context) {
        initialize(context);
        
        if (mustAvoidParallelMove == true) {
            final boolean downward = movesDownward(semitoneMove(context.previousNote(), context.note()));
            // prefer a narrower/higher interval when going down
            int betterIndex = downward ? indexByPitch - 1 : indexByPitch + 1;
            // but do the contrary when violating limits
            betterIndex = (betterIndex > lastIndex) ? indexByPitch - 1 : (betterIndex < 0) ? indexByPitch + 1 : betterIndex;
            return generatingIntervals.get(betterIndex);
        }
        return null;
    }
    
    @Override
    public int sortOrder() {
        return 40;
    }
}