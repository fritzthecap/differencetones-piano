package fri.music.instrument.notespiano.wave;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import fri.music.differencetones.composer.AbstractComposer;
import fri.music.differencetones.composer.DefaultComposer;
import fri.music.instrument.notespiano.NotesPianoPlayer;
import fri.music.instrument.notespiano.PlayController;
import fri.music.instrument.wave.DifferenceToneForNotesPiano;
import fri.music.player.Note;
import fri.music.player.NotesUtil;

/**
 * Plays the tune in notes-area as difference-tones.
 * This can be switched off via a checkbox.
 * Different tunings and parameters can be chosen, but not during play.
 */
public class NotesWithDifferenceTonePianoPlayer extends NotesPianoPlayer
{
    private JComponent playerPanel; // the component
    private JCheckBox convertToDifferenceTones;
    
    public NotesWithDifferenceTonePianoPlayer(DifferenceToneForNotesPiano piano) {
        super(piano);
    }
    
    /** Adds a "Difference Tones" checkbox and others on control-panel. */
    @Override
    public JComponent getPlayer(String melody) {
        if (this.playerPanel != null)
            return this.playerPanel;
        
        final JComponent playerPanel = super.getPlayer(melody);
        
        this.convertToDifferenceTones = new JCheckBox("Play Difference Tones", true);
        convertToDifferenceTones.setToolTipText("Play Written Notes as Difference Tones");
        final ActionListener resetPlayerListener = new ActionListener() {
            /** Reload player when changing. */
            @Override
            public void actionPerformed(ActionEvent e) {
                readNotesFromTextAreaCatchExceptions(getPlayController(), view());
            }
        };
        convertToDifferenceTones.addActionListener(resetPlayerListener);
        
        getDifferenceTonePiano().addIntervalRangeActionListener(resetPlayerListener);
        
        convertToDifferenceTones.setAlignmentX(JComponent.CENTER_ALIGNMENT);
        view().notesControlPanel.add(convertToDifferenceTones, 1); // add below "Play" button
        
        convertToDifferenceTones.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                getDifferenceTonePiano().setDifferenceToneParametersEnabled(convertToDifferenceTones.isSelected());
            }
        });
        
        return this.playerPanel = playerPanel;
    }
    
    /** Overridden to disable several <code>DifferenceToneForNotesPiano</code> controls on playing. */
    @Override
    protected void enableUiOnPlaying(boolean isStop) {
        super.enableUiOnPlaying(isStop);
        
        getDifferenceTonePiano().setTuningControlsEnabled(isStop);
        convertToDifferenceTones.setEnabled(isStop);
        getDifferenceTonePiano().setDifferenceToneParametersEnabled(isStop);
    }
    
    /** Overridden to disallow chords. */
    @Override
    protected PlayController newPlayController() {
        final PlayController playController = super.newPlayController();
        playController.setDisallowChords(true);
        return playController;
    }
    
    /** Overridden to alternatively generate difference-tone intervals when playing. */
    @Override
    protected Note[][] convertNotes(Note[][] notesArray) {
        if (convertToDifferenceTones.isSelected()) {
            final DifferenceToneForNotesPiano differenceTonePiano = getDifferenceTonePiano();
            final AbstractComposer composer = new DefaultComposer(
                    differenceTonePiano.getWaveSoundChannel().getTones(),
                    differenceTonePiano.narrowestAllowedInterval(),
                    differenceTonePiano.widestAllowedInterval(),
                    differenceTonePiano.getDeviation());
            try {
                view().error.setText("");
                return composer.compose(NotesUtil.toSingleNotesArray(notesArray));
            }
            catch (Exception e) { // some tunings like HARMONIC_SERIES can not generate certain difference-tones
                view().error.setText(
                        e.getMessage()+
                        " Tuning "+differenceTonePiano.getSelectedTuning()+
                        ", deviation "+Math.round(differenceTonePiano.getDeviation() * 2.0 * 100.0)+
                        ", bounds "+differenceTonePiano.narrowestAllowedInterval()+
                        " to "+differenceTonePiano.widestAllowedInterval());
                return null;
            }
        }
        return super.convertNotes(notesArray);
    }

    private DifferenceToneForNotesPiano getDifferenceTonePiano() {
        return (DifferenceToneForNotesPiano) piano;
    }
}