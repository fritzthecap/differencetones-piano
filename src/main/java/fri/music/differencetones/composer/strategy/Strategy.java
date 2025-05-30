package fri.music.differencetones.composer.strategy;

import fri.music.differencetones.DifferenceToneInversions.TonePair;

/**
 * 
 * Suggestions for strategies when choosing a difference-tone interval for a melody-note:
 * <ul>
 * <li>Low notes require narrow intervals, high notes require wide intervals,
 *     thus a global approximation calculation for every melody-note has to be done in the beginning</li>
 * <li>The upper tone of the interval should do smaller moves than the lower one</li>
 * <li>When a note repeats, do not use a different interval, use the preceding solution</li>
 * <li>On a melody-move, never do a "parallel" move by using the same interval as before</li>
 * <li>Do minimal moves, ideally one tone of the interval stays on same value as before</li>
 * <li>Prefer counter-moves, when melody goes up, interval should go down, or interval should get wider</li>
 * <li>(TODO) Emphasized melody-notes should use intervals that contain the melody-note,
 *     others should prefer intervals that do not</li>
 * </ul>
 */
public interface Strategy
{
    /**
     * @return a difference-tone interval for the melody-note in context, 
     *      or null if none found according to this strategy.
     */
    TonePair solution(StrategyContext context);
    
    /**
     * Strategies will not sort themselves by priority, this must be done explicitly!
     * @return the priority of the strategy, 1 is highest (first to try), N is lowest (last to try).
     */
    int suggestedPriority();
}
