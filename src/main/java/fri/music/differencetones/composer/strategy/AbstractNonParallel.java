package fri.music.differencetones.composer.strategy;

public abstract class AbstractNonParallel extends AbstractByPitch
{
    protected boolean mustAvoidParallelMove;
    
    @Override
    protected void initialize(StrategyContext context) {
        super.initialize(context);
        
        mustAvoidParallelMove = 
                isRepeatedNote == false &&
                considerAlternatives && 
                isParallel(context.previousInterval(), bestByPitch);
    }
}