package fri.music.differencetones.composer;

import java.util.ArrayList;
import java.util.List;
import fri.music.AbstractToneSystem;
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

    public DefaultComposer(AbstractToneSystem toneSystem, double deviationTolerance) {
        super(toneSystem, deviationTolerance);
    }
    
    @Override
    protected List<Strategy> getStrategies() {
        if (strategies.size() <= 0) {
            strategies.add(new PrecedingNearPitch()); // priority 10
            strategies.add(new OneNoteInCommon()); // priority 20
            strategies.add(new ByPitch()); // priority 30
            strategies.add(new NonParallel()); // priority 40
        }
        return strategies;
    }
}