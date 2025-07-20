package fri.music.instrument.notespiano;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import fri.music.differencetones.composer.AbstractComposer;
import fri.music.differencetones.composer.DefaultComposer;
import fri.music.instrument.wave.DifferenceTonePiano;
import fri.music.player.Note;

/**
 * Plays the tune in notes-area as difference-tones.
 * This can be switched off via a checkbox.
 * Different tunings can be chosen, but not during play.
 */
public class NotesDifferenceTonesPiano extends NotesPiano
{
    private JComponent playerPanel; // the component
    private JCheckBox convertToDifferenceTones;

    public NotesDifferenceTonesPiano(DifferenceTonePiano piano) {
        super(piano);
    }
    
    @Override
    public JComponent getPlayer(String melody) {
        if (this.playerPanel != null)
            return this.playerPanel;
        
        final JComponent playerPanel = super.getPlayer(melody);
        
        this.convertToDifferenceTones = new JCheckBox("Difference Tones", true);
        convertToDifferenceTones.setToolTipText("Play Difference Tones for Written Notes");
        final ActionListener actionListener = new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                final boolean enable = (convertToDifferenceTones.isSelected() == false);
                getDifferenceTonePiano().setHoldCheckboxEnabled(enable);
                getDifferenceTonePiano().setIntervalChooserEnabled(enable);
            }
        };
        convertToDifferenceTones.addActionListener(actionListener);
        actionListener.actionPerformed(null); // trigger enabling
        
        convertToDifferenceTones.setAlignmentX(Component.CENTER_ALIGNMENT);
        getNotesControlPanel().add(convertToDifferenceTones, 1); // add below "Play" button
        
        return this.playerPanel = playerPanel;
    }
    
    @Override
    protected void enableUiOnPlaying(boolean isStop) {
        super.enableUiOnPlaying(isStop);
        
        final DifferenceTonePiano differenceTonePiano = getDifferenceTonePiano();
        differenceTonePiano.getTuningChoice().setEnabled(isStop);
        differenceTonePiano.setDeviationSliderEnabled(isStop);
        
        convertToDifferenceTones.setEnabled(isStop);
    }
    
    @Override
    protected Note[][] convertNotesToChords(Note[] notesArray) {
        if (convertToDifferenceTones.isSelected()) {
            final DifferenceTonePiano differenceTonePiano = getDifferenceTonePiano();
            final AbstractComposer composer = new DefaultComposer(
                    differenceTonePiano.getWaveSoundChannel().getTones(), 
                    differenceTonePiano.getDeviation());
            try {
                return composer.compose(notesArray);
            }
            catch (Exception e) { // some tunings can not generate certain tones
                getErrorArea().setText(e.getMessage()+". Used tuning: "+differenceTonePiano.getTuningChoice().getSelectedItem());
                return new Note[0][];
            }
        }
        return super.convertNotesToChords(notesArray);
    }

    private DifferenceTonePiano getDifferenceTonePiano() {
        return (DifferenceTonePiano) piano;
    }
}