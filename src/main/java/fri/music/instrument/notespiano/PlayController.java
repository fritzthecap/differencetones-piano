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
        super.onEmptyNotes();
        
        view.timeSignatureChoice.setEnabled(true); // let user choose tempo and bar-type
        view.tempoSpinner.setEnabled(true);
        view.transposeMenu.setEnabled(false);
    }
    
    @Override
    protected void onNonEmptyNotes(String timeSignatureOnTop, Integer tempoOnTop) {
        super.onNonEmptyNotes(timeSignatureOnTop, tempoOnTop);
        
        // get optional BPM and time-signature extracted from top
        if (timeSignatureOnTop != null) {
            // avoid triggering event when same value
            if (timeSignatureOnTop.equals(view.timeSignatureChoice.getSelectedItem()) == false)
                view.timeSignatureChoice.setSelectedItem(timeSignatureOnTop);
            view.timeSignatureChoice.setEnabled(false); // will be managed in text-area now
        }
        else {
            view.timeSignatureChoice.setEnabled(true);
        }
        
        if (tempoOnTop != null) {
            // avoid triggering event when same value
            if (tempoOnTop.equals(view.tempoSpinner.getValue()) == false)
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
        super.onStartPlayer();
        
        view.timeSignatureChoice.setEnabled(false);
        view.tempoSpinner.setEnabled(false);
        view.transposeMenu.setEnabled(false);
    }
    
    @Override
    protected void onEnableUiOnPlaying(boolean isStop) {
        super.onEnableUiOnPlaying(isStop);
        
        view.writeToNotesCheckbox.setEnabled(isStop);
        view.transposeMenu.setEnabled(isStop);
    }
}