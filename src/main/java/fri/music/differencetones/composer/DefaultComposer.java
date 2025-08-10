package fri.music.differencetones.composer;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import fri.music.Tone;
import fri.music.differencetones.composer.strategy.ByPitch;
import fri.music.differencetones.composer.strategy.NonParallel;
import fri.music.differencetones.composer.strategy.OneNoteInCommon;
import fri.music.differencetones.composer.strategy.PrecedingNearPitch;
import fri.music.differencetones.composer.strategy.Strategy;

/**
 * Translates a melody (sequence of notes) into its difference-tones representation.
 */
public class DefaultComposer extends AbstractStrategicComposer
{
    private final List<Strategy> strategies = new ArrayList<>();

    /**
     * @param tones required, the tone-system to use.
     * @param narrowestInterval optional, the smallest allowed difference-tone inversion interval.
     * @param widestInterval optional, the biggest allowed difference-tone inversion interval.
     * @param deviationTolerance optional, the allowed deviation of a difference-tone from the real tone,
     *      0.4 would mean the difference-tone is allowed to be 0.4 of a semi-tone
     *      away from a real tone of the given tone-system. Mind that 0.45 is the maximum allowed.
     *      Pass -1.0 to get a default deviation.
     */
    public DefaultComposer(
            Tone[] tones, 
            String narrowestInterval, 
            String widestInterval, 
            double deviationTolerance)
    {
        super(tones, narrowestInterval, widestInterval, deviationTolerance);
    }
    
    @Override
    protected List<Strategy> getSortedStrategies() {
        if (strategies.size() <= 0) {
            strategies.add(new PrecedingNearPitch()); // priority 10
            strategies.add(new OneNoteInCommon()); // priority 20
            strategies.add(new ByPitch()); // priority 30
            strategies.add(new NonParallel()); // priority 40
            Collections.sort(strategies, (rule1, rule2) -> rule1.suggestedPriority() - rule2.suggestedPriority());
        }
        return strategies;
    }
}