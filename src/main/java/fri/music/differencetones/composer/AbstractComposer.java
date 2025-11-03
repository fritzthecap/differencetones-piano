package fri.music.differencetones.composer;

import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.SequencedMap;
import fri.music.AbstractToneSystem;
import fri.music.Tone;
import fri.music.ToneSystem;
import fri.music.differencetones.DifferenceToneInversions;
import fri.music.differencetones.DifferenceToneInversions.TonePair;
import fri.music.player.Note;

/**
 * Translation of a melody into difference-tone intervals.
 */
public abstract class AbstractComposer
{
    protected final Tone[] tones;
    protected final double deviationTolerance;
    protected final String narrowestInterval; 
    protected final String widestInterval; 
    
    protected AbstractComposer(
            Tone[] tones, 
            String narrowestInterval, 
            String widestInterval, 
            double deviationTolerance)
    {
        this.tones = Objects.requireNonNull(tones);
        this.narrowestInterval = narrowestInterval;
        this.widestInterval = widestInterval;
        this.deviationTolerance = deviationTolerance;
    }

    /**
     * Tries to find a good difference-tone representation of given melody.
     * There are many ways to do that. It even depends on which <code>ToneSystem</code> (tuning)
     * is used, so you will get different results for equal-temperament and just-intonations.
     * @param melody the melody to translate into (higher note) intervals.
     * @return the composed intervals that represent given melody as difference-tones.
     */
    public Note[][] compose(Note[] melody) {
        final Map<NoteWithIndex,TonePair> noteToInterval = buildMap(melody);

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
     * @param melody the melody to translate into difference-tone intervals.
     */
    private Map<NoteWithIndex,TonePair> buildMap(Note[] melody) {
        final List<Note> notesWithoutRests = Arrays.stream(melody).filter(n -> (false == n.isRest())).toList();
        final Note lowest  = notesWithoutRests.stream().min((note1, note2) -> note1.midiNumber - note2.midiNumber).orElseThrow();
        final Note highest = notesWithoutRests.stream().max((note1, note2) -> note1.midiNumber - note2.midiNumber).orElseThrow();
        final int maximumSemitoneDistance = highest.midiNumber - lowest.midiNumber;
        
        final DifferenceToneInversions inversions = createInversions(lowest, highest);
        final SequencedMap<NoteWithIndex,TonePair> result = new LinkedHashMap<>();
        
        TonePair previousInterval = null;
        for (int i = 0; i < melody.length; i++) {
            final Note note = melody[i];
            final boolean isRest = note.isRest();
            
            final TonePair bestInterval;
            if (isRest) {
                bestInterval = new TonePair();
            }
            else {
                final Note previousNote = (i > 0) ? melody[i - 1] : null;
                final int semitoneDistanceFromLowest = note.midiNumber - lowest.midiNumber;
                
                bestInterval = mapNote(
                            inversions,
                            maximumSemitoneDistance,
                            semitoneDistanceFromLowest,
                            previousNote,
                            previousInterval, 
                            note,
                            result);
            }
            
            result.put(new NoteWithIndex(note, i), bestInterval);
            
            if (isRest == false)
                previousInterval = bestInterval;
        }
        
        return result;
    }

    private Note[][] prepareResultArray(Note[] melody) {
        return new Note[melody.length][];
    }

    /** Puts Note objects into result, built from given tonePair and note. */
    private void assignInterval(Note[][] intervalsResult, int resultIndex, Note note, TonePair tonePair) {
        if (tonePair == null)
            throw new IllegalArgumentException("Note '"+note+"' at index "+resultIndex+" could not be mapped to an interval!");
        
        if (tonePair.isRest()) {
            intervalsResult[resultIndex] = new Note[1];
            intervalsResult[resultIndex][0] = note;
        }
        else {
            intervalsResult[resultIndex] = new Note[2];
            intervalsResult[resultIndex][0] = new Note(tonePair.lowerTone(), note);
            intervalsResult[resultIndex][1] = new Note(tonePair.upperTone(), note);
        }
    }
    
    /**
     * Provides all possibilities of difference-tone intervals ("inversions")
     * for given melody. That means, it will measure the pitch range and
     * then build fitting difference-tone intervals for all semi-tones in that range.
     * @param melody the melody that should be translated into a sequence of intervals
     *      generating difference-tones.
     * @return the DifferenceToneInversions that can represent given melody.
     */
    private DifferenceToneInversions createInversions(Note lowest, Note highest) {
        final DifferenceToneInversions inversions = toneRangeFor(
                lowest,
                highest,
                tones, 
                (narrowestInterval != null) ? ToneSystem.semitoneSteps(narrowestInterval) : -1,
                (widestInterval != null)    ? ToneSystem.semitoneSteps(widestInterval)    : -1,
                deviationTolerance);
        
        if (ToneSystem.MINOR_SECOND.equals(narrowestInterval) == false &&
                ToneSystem.MAJOR_SEVENTH.equals(widestInterval) == false)
            inversions.removeDissonant(false);
        
        return inversions;
    }
    
    /**
     * Builds a sufficient range of tones to model given melody with difference tones.
     * A melody with 1.3 octaves tone-range requires about 4 octaves above its lowest note.
     * @param melody required, the melody to model with difference tones.
     * @param toneStock required, the 12-tone system to be used for the melody and its difference tones.
     * @param smallestSemitoneDistance optional, the number of semi-tones representing the 
     *      smallest difference-tone interval to provide in returned tone-inversions.
     *      Default is MINOR_THIRD.
     * @param biggestSemitoneDistance optional, the number of semi-tones representing the 
     *      biggest difference-tone interval to provide in returned tone-inversions.
     *      Default is MAJOR_SIXTH.
     * @param deviationTolerance required, the tolerance for finding difference-tones.
     * @return the intervals (inversions) that can represent given melody.
     * @throws IllegalArgumentException when melody and its inversions do not fit into toneStock.
     */
    private DifferenceToneInversions toneRangeFor(
            Tone lowest,
            Tone highest, 
            Tone[] toneStock, 
            int smallestSemitoneDistance,
            int biggestSemitoneDistance,
            double deviationTolerance)
    {
        // Find octave range of melody and calculate a subset of tones fitting to that range.
        final int numberOfSemitones = highest.midiNumber - lowest.midiNumber;
        final double melodyOctaves = (double) numberOfSemitones / (double) ToneSystem.SEMITONES_PER_OCTAVE;
        
        // we need the tone below lowest melody note to make deviation work also for bottom
        int lowestIndexInToneStock = Arrays.binarySearch(toneStock, lowest, (t1, t2) -> t1.midiNumber - t2.midiNumber);
        lowestIndexInToneStock--; // go one deeper
        if (lowestIndexInToneStock < 0)
            throw new IllegalArgumentException("Tone stock is too small for lowest melody note "+lowest.ipnName+", its lowest is "+toneStock[0].ipnName);
        
        final String lowestIpnName = toneStock[lowestIndexInToneStock].ipnName;
        
        // melodyOctaves up to 1.3 -> 4 octaves, up to 2.3 -> 5, up to 3.3 -> 6, ...
        final int additionalOctavesTo4 = Math.min(0, (int) Math.ceil(melodyOctaves - 1.3));
        toneStock = AbstractToneSystem.tones(toneStock, lowestIpnName, lowestIpnName, 4 + additionalOctavesTo4);
        
        return new DifferenceToneInversions(
            new DifferenceToneInversions.Configuration(
                toneStock, 
                (smallestSemitoneDistance > 0) ? smallestSemitoneDistance : DifferenceToneInversions.Configuration.DEFAULT_SMALLEST_SEMITONE_STEPS,
                (biggestSemitoneDistance > 0) ? biggestSemitoneDistance : DifferenceToneInversions.Configuration.DEFAULT_BIGGEST_SEMITONE_STEPS,
                deviationTolerance
            )
        );
    }
}