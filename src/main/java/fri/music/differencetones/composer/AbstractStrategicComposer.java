package fri.music.differencetones.composer;

import java.util.List;
import java.util.SequencedMap;
import fri.music.Tone;
import fri.music.differencetones.DifferenceToneInversions;
import fri.music.differencetones.DifferenceToneInversions.TonePair;
import fri.music.differencetones.composer.strategy.Strategy;
import fri.music.differencetones.composer.strategy.StrategyContext;
import fri.music.player.Note;

/**
 * Strategy-based translation of a melody into difference-tone intervals.
 */
public abstract class AbstractStrategicComposer extends AbstractComposer
{
    protected AbstractStrategicComposer(
            Tone[] tones, 
            String narrowestInterval, 
            String widestInterval, 
            double deviationTolerance)
    {
        super(tones, narrowestInterval, widestInterval, deviationTolerance);
    }

    /**
     * Sub-classes must decide about strategies how to compose, and,
     * optionally, about the set of interval-possibilities to use for it.
     * @return a sorted list of <code>Strategy</code> objects to find 
     *      difference-tone intervals for a given melody.
     */
    protected abstract List<Strategy> getSortedStrategies();

    /** {@inheritDoc} */
    @Override
    protected TonePair mapNote(
            DifferenceToneInversions inversions,
            int maximumSemitoneDistance,
            int semitoneDistanceFromLowest,
            Note previousNote,
            TonePair previousInterval, 
            Note note,
            SequencedMap<NoteWithIndex,TonePair> result) 
    {
        final StrategyContext strategyContext = new StrategyContext(
                inversions,
                maximumSemitoneDistance,
                semitoneDistanceFromLowest,
                previousNote,
                previousInterval, 
                note,
                result);
        
        final List<Strategy> strategies = getSortedStrategies();
        
        for (Strategy strategy : strategies) {
            final TonePair solution = strategy.solution(strategyContext);
            if (solution != null) {
                logNoteMapping(note, strategy, solution, strategyContext);
                return solution;
            }
        }
        
        throw new IllegalStateException("No rule could find a solution for "+strategyContext.note());
    }
    
    
    private static final boolean LOG = false;
    
    private void logNoteMapping(Note note, Strategy strategy, TonePair interval, StrategyContext strategyContext) {
        if (LOG == false)
            return;
        
        System.err.println(
                "Note "+note+
                " was mapped by "+strategy.getClass().getSimpleName()+
                " to interval "+interval+
                " from "+strategyContext.inversions().getIntervalsGenerating(note.ipnName));
    }
}