package fri.music.differencetones.composer.strategy;

/**
 * Tells sub-classes whether they must avoid parallel solutions.
 * The <code>mustAvoidParallelMove</code> field will be true
 * when (1) current note is not a repeated note 
 * and (2) alternatives are present 
 * and (3) the previous solution is parallel to <code>bestByPitch</code>.
 */
public abstract class AbstractNonParallel extends AbstractByPitch
{
    /** Sub-classes must search alternatives to <code>bestByPitch</code>. */
    protected boolean mustAvoidParallelMove;
    
    /** You MUST call this method to evaluate all fields visible for a sub-class! */
    @Override
    protected void initialize(StrategyContext context) {
        super.initialize(context);
        
        mustAvoidParallelMove = 
                isRepeatedNote == false &&
                considerAlternatives && 
                isParallel(context.previousInterval(), bestByPitch);
    }
}