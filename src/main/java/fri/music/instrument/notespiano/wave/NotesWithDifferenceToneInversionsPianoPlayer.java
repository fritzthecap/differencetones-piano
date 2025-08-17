package fri.music.instrument.notespiano.wave;

import javax.swing.JComponent;
import fri.music.instrument.notespiano.NotesPianoPlayer;
import fri.music.instrument.notespiano.NotesTextPanel;
import fri.music.instrument.wave.DifferenceToneInversionsPiano;

public class NotesWithDifferenceToneInversionsPianoPlayer extends NotesPianoPlayer
{
    private JComponent playerPanel; // the component
    
    public NotesWithDifferenceToneInversionsPianoPlayer(DifferenceToneInversionsPiano piano) {
        super(piano);
    }

    @Override
    public JComponent getPlayer(String melody) {
        if (this.playerPanel != null)
            return this.playerPanel;
        
        final JComponent playerPanel = super.getPlayer(melody);
        
        return this.playerPanel = playerPanel;
    }
    
    @Override
    protected void enableUiOnPlaying(boolean isStop, NotesTextPanel view) {
        super.enableUiOnPlaying(isStop, view);
    }
    
    private DifferenceToneInversionsPiano getDifferenceTonePiano() {
        return (DifferenceToneInversionsPiano) piano;
    }
}