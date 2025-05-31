package fri.music.differencetones.composer.strategy;

import fri.music.differencetones.DifferenceToneInversions.TonePair;

/**
 * Tries to find the best solution in generatingIntervals for the current note's pitch. 
 * This rule cares about the global balance of the solutions describing the melody.
 * <p/> 
 * Wide intervals are good for high difference-tones, narrow intervals good for low ones.
 * The generatingIntervals list is sorted by narrow intervals first and wide intervals last.
 * The bestByPitch field will be the tone-pair in generatingIntervals
 * that is on index <code>lastIndex * (semitoneDistanceFromLowest / maximumSemitoneDistance)</code>.
 * The <code>(semitoneDistanceFromLowest / maximumSemitoneDistance)</code> fraction describes
 * the ratio within the global melody context where the current note resides.
 */
public abstract class AbstractByPitch extends AbstractStrategy
{
    /** The index of <code>bestByPitch</code> in generatingIntervals. */
    protected int indexByPitch;
    /** The best solution in generatingIntervals considering the balance of the whole melody. */
    protected TonePair bestByPitch;
    
    /** You MUST call this method to evaluate all fields visible for a sub-class! */
    @Override
    protected void initialize(StrategyContext context) {
        super.initialize(context);
        
        final double pitchFraction = 
                (double) context.semitoneDistanceFromLowest() / (double) context.maximumSemitoneDistance();
        // will be 0 for lowest note, 1 for highest
        
        indexByPitch = (int) Math.round(pitchFraction * (double) lastIndex);
        bestByPitch = generatingIntervals.get(indexByPitch);
    }
}