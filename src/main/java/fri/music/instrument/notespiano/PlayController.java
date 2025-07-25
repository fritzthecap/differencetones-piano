package fri.music.instrument.notespiano;

import java.awt.Color;
import java.util.List;
import javax.swing.SwingUtilities;
import fri.music.SoundChannel;
import fri.music.instrument.PianoWithSound;
import fri.music.player.Note;
import fri.music.player.Player;
import fri.music.player.notelanguage.MelodyFactory;

/**
 * Manages the "Start" button with its player-thread.
 */
class PlayController
{
    private final NotesPianoPlayer view;
    private final Object playerLock = new Object();
    private Player player;
    
    PlayController(NotesPianoPlayer view) {
        this.view = view;
    }
    
    /** "Start" button callback. */
    void playButtonPressed() {
        synchronized(playerLock) {
            final boolean isStop = (player != null);
            startOrStopPlayer(isStop);
        }
    }

    /** 
     * Enable time-signature and tempo-chooser, optionally clear errors.
     * This is called on any text input.
     */
    Note[] readNotesFromTextAreaCatchExceptions(boolean clearErrors) {
        try {
            final Note[] notes = readNotesFromTextArea();
            if (clearErrors)
                view.error.setText(""); // no exception was thrown, so clear errors
            return notes;
        }
        catch (Exception e) {
            view.error.setText(e.getMessage());
            view.play.setEnabled(false);
            view.formatBars.setEnabled(false);
            
            if (e instanceof IllegalArgumentException == false)
                e.printStackTrace();
        }
        return null;
    }
    
    /** Get a new MelodyFactory with current parameters from UI. */
    MelodyFactory newMelodyFactory() {
        final Integer[] timeSignature = view.timeSignatureParts();
        return new MelodyFactory(
                null,
                (Integer) view.tempoSpinner.getValue(),
                timeSignature[0],
                timeSignature[1]);
    }

    
    /** Make sure to always call this synchronized(playerLock)! */
    private void startOrStopPlayer(boolean isStop) {
        view.play.setText(isStop ? "Play" : "Stop");
        view.play.setBackground(isStop ? Color.GREEN : Color.decode("0xff9999")); // bright red
        
        view.enableUiOnPlaying(isStop);
    
        if (isStop) {
            if (player != null) {
                player.close();
                player = null;
            }
            readNotesFromTextAreaCatchExceptions(false);
            view.notesText.requestFocus();
        }
        else { // is start
            final Note[] notesArray = readNotesFromTextAreaCatchExceptions(true);
            
            // disable controls that may have been enabled by readNotesFromTextArea()
            view.timeSignatureChoice.setEnabled(false);
            view.tempoSpinner.setEnabled(false);
            view.formatBars.setEnabled(false);
            
            startPlayer(notesArray); // thread is running now
        }
    }
    
    /** This method is synchronized(playerLock) because called from startOrStopPlayer() only. */
    private void startPlayer(Note[] notesArray) {
        final Note[][] chords = view.convertNotesToChords(notesArray); // optional override
        
        final SoundChannel pianoKeyConnector = new PianoKeyConnector(view.piano);
        this.player = new Player(pianoKeyConnector); // is synchronized because called from startOrStop()

        // to allow "Stop" while playing, all playing happens in a background thread
        final Thread playerThread = new Thread(() -> playNotes(chords));
        playerThread.start();
    }
    
    private Note[] readNotesFromTextArea() throws IllegalArgumentException { // exceptions coming from MelodyFactory
        final String notesString = view.notesText.getText();
        final boolean enable;
        final Note[] notes;
        
        if (notesString.trim().isEmpty()) { // nothing in text-area
            notes = new Note[0];
            enable = false; // no actions can be performed when empty
            view.timeSignatureChoice.setEnabled(true); // let user choose tempo and bar-type
            view.tempoSpinner.setEnabled(true);
        }
        else { // parse notes
            final MelodyFactory melodyFactory = newMelodyFactory();
            notes = melodyFactory.translate(notesString); // throws exceptions
            checkNotesRange(notes); // throws exceptions
            
            enable = true; // no exception was thrown until here, so enable actions
            
            // get optional BPM and time-signature extracted from top
            final String timeSignatureOnTop = melodyFactory.getTimeSignatureOnTop();
            if (timeSignatureOnTop != null) {
                view.timeSignatureChoice.setSelectedItem(timeSignatureOnTop);
                view.timeSignatureChoice.setEnabled(false); // will be managed in text-area now
            }
            else {
                view.timeSignatureChoice.setEnabled(true);
            }
            
            final Integer tempoOnTop = melodyFactory.getTempoOnTop();
            if (tempoOnTop != null) {
                view.tempoSpinner.setValue(tempoOnTop);
                view.tempoSpinner.setEnabled(false); // will be managed in text-area now
            }
            else {
                view.tempoSpinner.setEnabled(true);
            }
        }
        
        view.play.setEnabled(enable);
        view.formatBars.setEnabled(enable);
        
        return notes;
    }
    
    private void checkNotesRange(Note[] notesArray) {
        if (notesArray.length <= 0)
            throw new IllegalArgumentException("No notes were given!");
        
        final List<PianoWithSound.Keyboard.Key> keys = view.piano.getKeys();
        final int lowestMidiNumber  = keys.get(0).midiNoteNumber;
        final int highestMidiNumber = keys.get(keys.size() - 1).midiNoteNumber;
        for (Note note : notesArray)
            if (note.isRest() == false)
                if (lowestMidiNumber > note.midiNumber || highestMidiNumber < note.midiNumber)
                    throw new IllegalArgumentException("Note is not in keyboard range: "+note.ipnName);
    }

    /** 
     * Plays given chords as sound and strikes according piano keys.
     * This method runs in a background-thread (to be interruptable), 
     * so any call to Swing must happen via SwingUtilities.invokeXXX().
     * @param chordsArray the intervals or chords to play on piano.
     */
    private void playNotes(Note[][] chordsArray) {
        final Player myPlayer = this.player; // remember which player to use
        
        boolean interrupted = false;
        RuntimeException exception = null;
        
        for (int i = 0; interrupted == false && exception == null && i < chordsArray.length; i++) { // play all notes
            final Note[] chord = chordsArray[i];
            try {
                SwingUtilities.invokeAndWait(() -> { // waits synchronously for each note
                    synchronized(playerLock) {
                        if (myPlayer == this.player) // Start/Stop button could be clicked several times!
                            myPlayer.playSimultaneously(chord); // this causes Swing UI updates and thus must run in EDT
                    }
                });
            }
            catch (Exception e) {
                exception = new RuntimeException(e);
            }
            
            synchronized(playerLock) {
                interrupted = (myPlayer != this.player); // "Stop" button was pressed
            }
        }
        
        SwingUtilities.invokeLater(() -> { // causes Swing UI updates and thus must run in EDT
            synchronized(playerLock) {
                if (myPlayer == this.player) // still not stopped, do self-stop
                    startOrStopPlayer(true); // enable "Play" and close player, false = do not clear any error
            }
        });
        
        if (exception != null) // throw only after correct termination
            throw exception;
    }
}