package fri.music.differencetones.composer.strategy;

import java.util.List;
import java.util.stream.Collectors;
import fri.music.differencetones.DifferenceToneInversions.TonePair;

public class PrecedingNearPitch extends AbstractByPitch
{
    @Override
    public TonePair solution(StrategyContext context) {
        initialize(context);
        
        // There could be several preceding solutions, none of them must be parallel.
        // Find the one that is nearest to current pitch, or the most recent.
        final List<TonePair> precedingMappings = context.result().reversed().entrySet().stream()
                .filter(entry -> entry.getKey().tone().equals(context.note()))
                .map(entry -> entry.getValue())
                .collect(Collectors.toList());
        
        NearestSearchResult nearest = new NearestSearchResult();
        for (final TonePair precedingMapping : precedingMappings)
            // parallel is allowed for a repeated note
            if (isRepeatedNote || false == isParallel(context.previousInterval(), precedingMapping))
                nearest = getNearest(nearest, bestByPitch, precedingMapping);

        return nearest.result();
    }
    
    @Override
    public int sortOrder() {
        return 10;
    }
}