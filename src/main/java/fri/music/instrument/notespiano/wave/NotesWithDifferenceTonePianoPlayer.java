package fri.music.instrument.notespiano.wave;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import fri.music.differencetones.composer.AbstractComposer;
import fri.music.differencetones.composer.DefaultComposer;
import fri.music.instrument.notespiano.NotesPianoPlayer;
import fri.music.instrument.notespiano.NotesTextPanelBase;
import fri.music.instrument.notespiano.PlayController;
import fri.music.instrument.notespiano.abc.AbcExportComponent;
import fri.music.instrument.wave.DifferenceToneForNotesPiano;
import fri.music.player.Note;
import fri.music.player.NotesUtil;
import fri.music.player.notelanguage.MelodyFactory;
import fri.music.player.notelanguage.abc.AbcExport;
import fri.music.player.notelanguage.abc.AbcTunesCombiner;

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
        
        this.convertToDifferenceTones = new JCheckBox("Play as Difference Tones", true);
        convertToDifferenceTones.setToolTipText("Play Notes in Textarea as Difference Tones, Turn This OFF to Hear Simple Notes");
        final ActionListener resetPlayerListener = new ActionListener() {
            /** Reload player when changing. */
            @Override
            public void actionPerformed(ActionEvent e) {
                readNotesFromTextAreaCatchExceptions(getPlayController(), melodyView());
            }
        };
        convertToDifferenceTones.addActionListener(resetPlayerListener);
        
        getDifferenceTonePiano().addIntervalRangeActionListener(resetPlayerListener);
        
        convertToDifferenceTones.setAlignmentX(JComponent.CENTER_ALIGNMENT);
        melodyView().notesControlPanel.add(convertToDifferenceTones, 1); // add below "Play" button
        
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
    protected void enableUiOnPlaying(boolean isStop, Note[][] sounds, int currentSoundIndex, NotesTextPanelBase view) {
        super.enableUiOnPlaying(isStop, sounds, currentSoundIndex, view);
        
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
    
    @Override
    protected AbcExportComponent newAbcExportComponent(NotesTextPanelBase view) {
        return new AbcExportComponent(melodyView().notesText.getText(), newMelodyFactory(), true) {
            @Override
            protected String export(AbcExport.Configuration configuration, String notesText, boolean includeTuning) {
                final String intervalsText = generateIntervals(notesText, melodyFactory);
                final String intervalsAbcText = super.export(configuration, intervalsText, true); // intervals always need tuning
                if (includeMelody == null || includeMelody.isSelected() == false)
                    return intervalsAbcText;
                
                final String melodyAbcText = super.export(configuration, notesText, true);
                return new AbcTunesCombiner().combine(
                        "Intervals name=\"Intervals\"", intervalsAbcText, 
                        "Melody name=\"Difference Tones\"", melodyAbcText);
            }
        };
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
                melodyView().error.setText("");
                return composer.compose(NotesUtil.toSingleNotesArray(notesArray));
            }
            catch (Exception e) { // some tunings like HARMONIC_SERIES can not generate certain difference-tones
                melodyView().error.setText(
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


    private String generateIntervals(String notesText, MelodyFactory melodyFactory) {
        final Note[][] melodyNotes = melodyFactory.translate(notesText);
        final Note[][] intervalNotes = convertNotes(melodyNotes);
        final String intervalsText = melodyFactory.formatBarLines(intervalNotes, false, false);
        return intervalsText;
    }
    
    private DifferenceToneForNotesPiano getDifferenceTonePiano() {
        return (DifferenceToneForNotesPiano) piano;
    }
}