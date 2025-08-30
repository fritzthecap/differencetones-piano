package fri.music.instrument.notespiano;

/**
 * Manages the player for full NotesTextPanel.
 */
public class PlayController extends PlayControllerBase
{
    private NotesTextPanel view;
    
    public PlayController(NotesPianoPlayer notesPianoPlayer) {
        super(notesPianoPlayer);
    }
    
    void setView(NotesTextPanel view) {
        super.setBaseView(view);
        this.view = view;
    }
    
    @Override
    protected void onEmptyNotes() {
        view.timeSignatureChoice.setEnabled(true); // let user choose tempo and bar-type
        view.tempoSpinner.setEnabled(true);
        view.transposeMenu.setEnabled(false);
    }
    
    @Override
    protected void onNonEmptyNotes(String timeSignatureOnTop, Integer tempoOnTop) {
        // get optional BPM and time-signature extracted from top
        if (timeSignatureOnTop != null) {
            view.timeSignatureChoice.setSelectedItem(timeSignatureOnTop);
            view.timeSignatureChoice.setEnabled(false); // will be managed in text-area now
        }
        else {
            view.timeSignatureChoice.setEnabled(true);
        }
        
        if (tempoOnTop != null) {
            view.tempoSpinner.setValue(tempoOnTop);
            view.tempoSpinner.setEnabled(false); // will be managed in text-area now
        }
        else {
            view.tempoSpinner.setEnabled(true);
        }
        
        view.transposeMenu.setEnabled(true);
    }

    @Override
    protected void onStartPlayer() {
        view.timeSignatureChoice.setEnabled(false);
        view.tempoSpinner.setEnabled(false);
        view.transposeMenu.setEnabled(false);
    }
    
    @Override
    protected void onEnableUiOnPlaying(boolean isStop) {
        view.writeToNotesCheckbox.setEnabled(isStop);
        view.transposeMenu.setEnabled(isStop);
    }
}