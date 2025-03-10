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
public class Composer extends AbstractComposer
{
    private final List<Strategy> strategies = new ArrayList<>();

    public Composer(AbstractToneSystem toneSystem, double deviationTolerance) {
        super(toneSystem, deviationTolerance);
        
        strategies.add(new PrecedingNearPitch());
        strategies.add(new OneNoteInCommon());
        strategies.add(new ByPitch());
        strategies.add(new AvoidParallel());
        Collections.sort(strategies, (rule1, rule2) -> rule1.sortOrder() - rule2.sortOrder());
    }
    
    @Override
    public Note[][] compose(Note[] melody) {
        final DifferenceToneInversions inversions = createInversions(
                melody,
                ToneSystem.MINOR_THIRD, //MAJOR_SECOND),
                ToneSystem.MAJOR_SIXTH); //FIFTH);
        return super.compose(melody, inversions, strategies);
    }
}