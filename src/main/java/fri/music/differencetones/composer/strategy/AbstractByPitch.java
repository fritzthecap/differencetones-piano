package fri.music.differencetones.composer.strategy;

import fri.music.differencetones.DifferenceToneInversions.TonePair;

public abstract class AbstractByPitch extends AbstractStrategy
{
    protected int indexByPitch;
    protected TonePair bestByPitch;
    
    protected void initialize(StrategyContext context) {
        super.initialize(context);
        
        final double pitchFraction = 
                (double) context.semitoneDistanceFromLowest() / (double) context.maximumSemitoneDistance();
        // will be 0 for lowest note, 1 for highest
        indexByPitch = (int) Math.round(pitchFraction * (double) lastIndex);
        bestByPitch = generatingIntervals.get(indexByPitch);
    }
}