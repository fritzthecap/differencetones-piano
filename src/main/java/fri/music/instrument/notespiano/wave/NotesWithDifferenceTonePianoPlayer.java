package fri.music.instrument.notespiano.wave;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.BorderFactory;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import fri.music.ToneSystem;
import fri.music.differencetones.composer.AbstractComposer;
import fri.music.differencetones.composer.DefaultComposer;
import fri.music.instrument.notespiano.NotesPianoPlayer;
import fri.music.player.Note;

/**
 * Plays the tune in notes-area as difference-tones.
 * This can be switched off via a checkbox.
 * Different tunings can be chosen, but not during play.
 */
public class NotesWithDifferenceTonePianoPlayer extends NotesPianoPlayer
{
    private static final String[] narrowestIntervalNames = new String[] {
            ToneSystem.MAJOR_SECOND,
            ToneSystem.MINOR_THIRD,
            ToneSystem.MAJOR_THIRD,
            ToneSystem.FOURTH
        };
    private static final String[] widestIntervalNames = new String[] {
            ToneSystem.FIFTH,
            ToneSystem.MINOR_SIXTH,
            ToneSystem.MAJOR_SIXTH,
            ToneSystem.MINOR_SEVENTH
        };

    private JComponent playerPanel; // the component
    private JCheckBox convertToDifferenceTones;
    private JComboBox<String> narrowestIntervalChoice;
    private JComboBox<String> widestIntervalChoice;
    
    public NotesWithDifferenceTonePianoPlayer(DifferenceToneForNotesPiano piano) {
        super(piano);
    }
    
    /** Adds a "Difference Tones" checkbox on control-panel. */
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
                readNotesFromTextAreaCatchExceptions();
            }
        };
        convertToDifferenceTones.addActionListener(resetPlayerListener);
        
        this.narrowestIntervalChoice = new JComboBox<String>(narrowestIntervalNames);
        narrowestIntervalChoice.setBorder(BorderFactory.createTitledBorder("Narrowest Interval"));
        narrowestIntervalChoice.setToolTipText("Narrowest Allowed Interval for Generating Difference Tones");
        narrowestIntervalChoice.setSelectedItem(ToneSystem.MINOR_THIRD); // make good default
        narrowestIntervalChoice.addActionListener(resetPlayerListener);
        
        this.widestIntervalChoice = new JComboBox<String>(widestIntervalNames);
        widestIntervalChoice.setBorder(BorderFactory.createTitledBorder("Widest Interval"));
        widestIntervalChoice.setToolTipText("Widest Allowed Interval for Generating Difference Tones");
        widestIntervalChoice.setSelectedItem(ToneSystem.MAJOR_SIXTH);
        widestIntervalChoice.addActionListener(resetPlayerListener);
        
        convertToDifferenceTones.setAlignmentX(Component.CENTER_ALIGNMENT);
        getNotesControlPanel().add(convertToDifferenceTones, 1); // add below "Play" button
        narrowestIntervalChoice.setAlignmentX(Component.CENTER_ALIGNMENT);
        getNotesControlPanel().add(narrowestIntervalChoice, 2);
        widestIntervalChoice.setAlignmentX(Component.CENTER_ALIGNMENT);
        getNotesControlPanel().add(widestIntervalChoice, 3);
        
        convertToDifferenceTones.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                final boolean playDifferenceTones = ((JCheckBox) e.getSource()).isSelected();
                narrowestIntervalChoice.setEnabled(playDifferenceTones);
                widestIntervalChoice.setEnabled(playDifferenceTones);
            }
        });
        
        return this.playerPanel = playerPanel;
    }
    
    /** Overridden to disable several <code>DifferenceTonePiano</code> controls on playing. */
    @Override
    protected void enableUiOnPlaying(boolean isStop) {
        super.enableUiOnPlaying(isStop);
        
        final DifferenceToneForNotesPiano differenceTonePiano = getDifferenceTonePiano();
        differenceTonePiano.setTuningControlsEnabled(isStop);
        
        convertToDifferenceTones.setEnabled(isStop);
    }
    
    /** Overridden to alternatively generate difference-tone intervals when playing. */
    @Override
    protected Note[][] convertNotesToChords(Note[] notesArray) {
        if (convertToDifferenceTones.isSelected()) {
            final DifferenceToneForNotesPiano differenceTonePiano = getDifferenceTonePiano();
            final AbstractComposer composer = new DefaultComposer(
                    differenceTonePiano.getWaveSoundChannel().getTones(),
                    narrowestAllowedInterval(),
                    widestAllowedInterval(),
                    differenceTonePiano.getDeviation());
            try {
                return composer.compose(notesArray);
            }
            catch (Exception e) { // some tunings like HARMONIC_SERIES can not generate certain difference-tones
                getErrorArea().setText(e.getMessage()+". Used tuning: "+differenceTonePiano.getSelectedTuning());
                return new Note[0][];
            }
        }
        return super.convertNotesToChords(notesArray);
    }

    private String narrowestAllowedInterval() {
        return (String) narrowestIntervalChoice.getSelectedItem();
    }

    private String widestAllowedInterval() {
        return (String) widestIntervalChoice.getSelectedItem();
    }

    private DifferenceToneForNotesPiano getDifferenceTonePiano() {
        return (DifferenceToneForNotesPiano) piano;
    }
}