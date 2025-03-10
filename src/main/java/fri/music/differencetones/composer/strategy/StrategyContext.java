package fri.music.differencetones.composer.strategy;

import java.util.SequencedMap;
import fri.music.differencetones.DifferenceToneInversions;
import fri.music.differencetones.DifferenceToneInversions.TonePair;
import fri.music.differencetones.composer.NoteWithIndex;
import fri.music.player.Note;

public record StrategyContext(
            DifferenceToneInversions inversions,
            int maximumSemitoneDistance,
            int semitoneDistanceFromLowest,
            Note previousNote,
            TonePair previousInterval, 
            Note note,
            SequencedMap<NoteWithIndex,TonePair> result) 
{
}
