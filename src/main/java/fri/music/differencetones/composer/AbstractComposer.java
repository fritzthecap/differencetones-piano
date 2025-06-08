package fri.music.differencetones.composer;

import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.SequencedMap;
import fri.music.AbstractToneSystem;
import fri.music.ToneSystem;
import fri.music.differencetones.DifferenceToneInversions;
import fri.music.differencetones.DifferenceToneInversions.TonePair;
import fri.music.differencetones.composer.strategy.Strategy;
import fri.music.differencetones.composer.strategy.StrategyContext;
import fri.music.player.Note;

/**
 * Strategy-based translation of a melody into difference-tone intervals.
 * Playing the intervals should outline the melody.
 */
public abstract class AbstractComposer
{
    protected final AbstractToneSystem toneSystem;
    protected final double deviationTolerance;
    
    protected AbstractComposer(AbstractToneSystem toneSystem, double deviationTolerance) {
        this.toneSystem = toneSystem;
        this.deviationTolerance = deviationTolerance;
    }

    /**
     * @param melody the melody to translate into high-note intervals.
     * @return the composed intervals to play given melody as difference-tones.
     */
    public abstract Note[][] compose(Note[] melody);

    protected final DifferenceToneInversions createInversions(Note[] melody) {
        return createInversions(melody, null, null);
    }
    
    protected final DifferenceToneInversions createInversions(
            Note[] melody, 
            String narrowestInterval, 
            String widestInterval)
    {
        final DifferenceToneInversions inversions = DifferenceToneInversions.toneRangeFor(
                melody, 
                toneSystem, 
                (narrowestInterval != null) ? ToneSystem.semitoneSteps(narrowestInterval) : -1,
                (widestInterval != null)    ? ToneSystem.semitoneSteps(widestInterval)    : -1,
                deviationTolerance);
        inversions.removeDissonant(false);
        return inversions;
    }
    
    /** To be called for applying given strategies on given melody. */
    protected final Note[][] compose(Note[] melody, DifferenceToneInversions inversions, List<Strategy> strategies) {
        final Map<NoteWithIndex,TonePair> noteToInterval = buildMap(melody, inversions, strategies);

        final Note[][] result = prepareResultArray(melody);
        for (int i = 0; i < melody.length; i++) {
            final Note note = melody[i];
            assignInterval(result, i, note, noteToInterval.get(new NoteWithIndex(note, i)));
        }
        return result;
    }
    
    private final Map<NoteWithIndex,TonePair> buildMap(Note[] melody, DifferenceToneInversions inversions, List<Strategy> strategies) {
        final List<Note> melodyList = Arrays.asList(melody);
        final Note lowest  = melodyList.stream().min((note1, note2) -> note1.midiNumber - note2.midiNumber).orElseThrow();
        final Note highest = melodyList.stream().max((note1, note2) -> note1.midiNumber - note2.midiNumber).orElseThrow();
        final int maximumSemitoneDistance = highest.midiNumber - lowest.midiNumber;
        
        final SequencedMap<NoteWithIndex,TonePair> result = new LinkedHashMap<>();
        TonePair previousInterval = null;
        for (int i = 0; i < melody.length; i++) {
            final Note note = melody[i];
            final Note previousNote = (i > 0) ? melody[i - 1] : null;
            final int semitoneDistanceFromLowest = note.midiNumber - lowest.midiNumber;
            
            final TonePair bestInterval = mapNote(
                    strategies,
                    new StrategyContext(
                        inversions,
                        maximumSemitoneDistance,
                        semitoneDistanceFromLowest,
                        previousNote,
                        previousInterval, 
                        note,
                        result)
                );
            result.put(new NoteWithIndex(note, i), previousInterval = bestInterval);
        }
        return result;
    }

    private TonePair mapNote(List<Strategy> strategies, StrategyContext context) {
        for (Strategy strategy : strategies) {
            final TonePair solution = strategy.solution(context);
            if (solution != null)
                return solution;
        }
        throw new IllegalStateException("No rule could find a solution for "+context.note());
    }
    
    private Note[][] prepareResultArray(Note[] melody) {
        return new Note[melody.length][2];
    }

    private void assignInterval(Note[][] result, int resultIndex, Note note, TonePair tonePair) {
        if (tonePair == null)
            throw new IllegalArgumentException("Following note at index "+resultIndex+" could not be mapped to an interval: "+note);
        
        result[resultIndex][0] = new Note(tonePair.lowerTone(), note.durationMilliseconds, note.volume, note.emphasized);
        result[resultIndex][1] = new Note(tonePair.upperTone(), note.durationMilliseconds, note.volume, note.emphasized);
    }
}