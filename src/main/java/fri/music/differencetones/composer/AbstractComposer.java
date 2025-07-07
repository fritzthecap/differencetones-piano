package fri.music.differencetones.composer;

import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.SequencedMap;
import fri.music.AbstractToneSystem;
import fri.music.ToneSystem;
import fri.music.differencetones.DifferenceToneInversions;
import fri.music.differencetones.DifferenceToneInversions.TonePair;
import fri.music.player.Note;

/**
 * Translation of a melody into difference-tone intervals.
 */
public abstract class AbstractComposer
{
    protected final AbstractToneSystem toneSystem;
    protected final double deviationTolerance;
    
    protected AbstractComposer(AbstractToneSystem toneSystem, double deviationTolerance) {
        this.toneSystem = Objects.requireNonNull(toneSystem);
        this.deviationTolerance = deviationTolerance;
    }

    /**
     * Provides all possibilities of difference-tone intervals ("inversions")
     * for given melody. That means, it will measure the pitch range and
     * then build fitting difference-tone intervals for all semi-tones in that range.
     * @param melody the melody that should be translated into a sequence of intervals
     *      generating difference-tones.
     * @param narrowestInterval optional, the smallest <code>ToneSystem.*</code> interval
     *      provide in returned tone-inversions. Default is MINOR_THIRD.
     * @param widestInterval optional, the biggest <code>ToneSystem.*</code> interval
     *      provide in returned tone-inversions. Default is MAJOR_SIXTH.
     * @return the DifferenceToneInversions that can represent given melody.
     */
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
    
    /**
     * Shorthand for default behavior, calling <code>compose(melody, null)</code>.
     * @param melody the melody to translate into (higher note) intervals.
     * @return the composed intervals that represent given melody as difference-tones.
     */
    public Note[][] compose(Note[] melody) {
        return compose(melody, null);
    }

    /**
     * Tries to find a good difference-tone representation of given melody.
     * There are many ways to do that. It even depends on which <code>ToneSystem</code> (tuning)
     * is used, so you will get different results for equal-temperament and just-intonations.
     * @param melody the melody to translate into (higher note) intervals.
     * @param inversions optional, the result of <code>createInversions(....)</code>.
     * @return the composed intervals that represent given melody as difference-tones.
     */
    public final Note[][] compose(Note[] melody, DifferenceToneInversions inversions) {
        final Map<NoteWithIndex,TonePair> noteToInterval = buildMap(melody, inversions);

        final Note[][] result = prepareResultArray(melody);
        for (int i = 0; i < melody.length; i++) {
            final Note note = melody[i];
            assignInterval(result, i, note, noteToInterval.get(new NoteWithIndex(note, i)));
        }
        return result;
    }
    
    /** Maps given note in given context to a difference-tone <code>TonePair</code>. */
    protected abstract TonePair mapNote(
            DifferenceToneInversions inversions,
            int maximumSemitoneDistance,
            int semitoneDistanceFromLowest,
            Note previousNote,
            TonePair previousInterval, 
            Note note,
            SequencedMap<NoteWithIndex,TonePair> result);

    /**
     * @param melody the melody to translate.
     * @param inversions optional, the result of <code>createInversions(....)</code>.
     */
    private final Map<NoteWithIndex,TonePair> buildMap(Note[] melody, DifferenceToneInversions inversions) {
        if (inversions == null) // call default when null
            inversions = createInversions(melody, null, null);
        
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
                        inversions,
                        maximumSemitoneDistance,
                        semitoneDistanceFromLowest,
                        previousNote,
                        previousInterval, 
                        note,
                        result);
            result.put(new NoteWithIndex(note, i), previousInterval = bestInterval);
        }
        return result;
    }

    private Note[][] prepareResultArray(Note[] melody) {
        return new Note[melody.length][2];
    }

    /** Puts Note objects into result, built from given tonePair and note. */
    private void assignInterval(Note[][] result, int resultIndex, Note note, TonePair tonePair) {
        if (tonePair == null)
            throw new IllegalArgumentException("Following note at index "+resultIndex+" could not be mapped to an interval: "+note);
        
        result[resultIndex][0] = new Note(tonePair.lowerTone(), note);
        result[resultIndex][1] = new Note(tonePair.upperTone(), note);
    }
}