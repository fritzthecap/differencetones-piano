package fri.music.differencetones.composer.strategy;

import java.util.SequencedMap;
import fri.music.differencetones.DifferenceToneInversions;
import fri.music.differencetones.DifferenceToneInversions.TonePair;
import fri.music.differencetones.composer.NoteWithIndex;
import fri.music.player.Note;

/**
 * To be used in AbstractComposer.
 * Such a context is created newly for every note to be translated.
 * @param inversions to be used for finding tone-pairs that can generate a certain difference-tone.
 * @param melodyOctaves 0.5 for melody range 1/2 octave, 1.0 for 1 octave, 1.5 for 1+1/2 ...
 * @param maximumSemitoneDistance number of semi-tones between highest and lowest note.
 * @param semitoneDistanceFromLowest number of semi-tones between current and lowest note.
 * @param previousNote the preceding note of the current one.
 * @param previousInterval the difference-tone generating interval of the preceding note.
 * @param note the current note.
 * @param result the result where notes are mapped to difference-tone generating intervals.
 */
public record StrategyContext(
        /** To be used for finding tone-pairs that can generate a certain difference-tone. */
        DifferenceToneInversions inversions,
        /** 0.5 for melody range 1/2 octave, 1.0 for 1 octave, 1.5 for 1+1/2, ... */
        double melodyOctaves,
        /** Number of semi-tones between highest and lowest note. */
        int maximumSemitoneDistance,
        /** Number of semi-tones between current and lowest note. */
        int semitoneDistanceFromLowest,
        /** The preceding note of the current one. */
        Note previousNote,
        /** The difference-tone generating interval of the preceding note. */
        TonePair previousInterval, 
        /** The current note. */
        Note note,
        /** The result where notes are mapped to difference-tone generating intervals. */
        SequencedMap<NoteWithIndex,TonePair> result) 
{
}
