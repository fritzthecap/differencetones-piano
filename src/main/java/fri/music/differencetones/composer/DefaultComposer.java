package fri.music.differencetones.composer;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import fri.music.AbstractToneSystem;
import fri.music.ToneSystem;
import fri.music.differencetones.DifferenceToneInversions;
import fri.music.differencetones.composer.strategy.*;
import fri.music.player.Note;

/**
 * Translates a melody (sequence of notes) into its representation as difference tones.
 */
public class DefaultComposer extends AbstractComposer
{
    private final List<Strategy> strategies = new ArrayList<>();

    public DefaultComposer(AbstractToneSystem toneSystem, double deviationTolerance) {
        super(toneSystem, deviationTolerance);
        
        strategies.add(new PrecedingNearPitch()); // priority 10
        strategies.add(new OneNoteInCommon()); // priority 20
        strategies.add(new ByPitch()); // priority 30
        strategies.add(new NonParallel()); // priority 40
        Collections.sort(strategies, (rule1, rule2) -> rule1.suggestedPriority() - rule2.suggestedPriority());
    }
    
    @Override
    public Note[][] compose(Note[] melody) {
        final DifferenceToneInversions inversions = createInversions(
                melody,
                ToneSystem.MINOR_THIRD, //MAJOR_SECOND
                ToneSystem.MAJOR_SIXTH); //FIFTH
        return super.compose(melody, inversions, strategies);
    }
}