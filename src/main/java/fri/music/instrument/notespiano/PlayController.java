package fri.music.instrument.notespiano;

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
class PlayController implements PlayControlButtons.Listener
{
    private final NotesPianoPlayer view;
    private final Object playerLock = new Object();
    private Player player;
    private Note[][] sounds; // playing notes, possibly polyphonic
    private int currentSoundIndex;
    
    PlayController(NotesPianoPlayer view) {
        this.view = view;
    }
    
    // interface PlayControlButtons.Listener
    
    @Override
    public void fastBackwardPressed() {
        synchronized(playerLock) {
            if (player != null)
                startOrStopPlayer(true);
            else
                currentSoundIndex = -1; // forward will increment
        }
    }
    
    @Override
    public void backwardPressed() {
        synchronized(playerLock) {
            if (player != null)
                startOrStopPlayer(true);
            else
                skip(false);
        }
    }
    
    @Override
    public void playPressed() {
        synchronized(playerLock) {
            final boolean isStop = (player != null);
            if (isStop == false)
                skipCurrentSoundIndex(true);
            startOrStopPlayer(isStop);
        }
    }
    
    @Override
    public void forwardPressed() {
        synchronized(playerLock) {
            if (player != null)
                startOrStopPlayer(true);
            else
                skip(true);
        }
    }
    
    @Override
    public void fastForwardPressed() {
        synchronized(playerLock) {
            if (player != null)
                startOrStopPlayer(true);
            else if (sounds != null)
                currentSoundIndex = sounds.length; // backward will decrement
        }
    }
    
    // methods called by view
    
    /** Get a new MelodyFactory with current parameters from UI. */
    MelodyFactory newMelodyFactory() {
        final Integer[] timeSignature = view.timeSignatureParts();
        return new MelodyFactory(
                null,
                (Integer) view.tempoSpinner.getValue(),
                timeSignature[0],
                timeSignature[1]);
    }

    /** 
     * Enable time-signature and tempo-chooser, optionally clear errors.
     * This is called on any text input, and also when starting or stopping melody.
     * @return null when error, else the notes-array from text area.
     */
    Note[] readNotesFromTextAreaCatchExceptions() {
        try {
            final Note[] notes = readNotesFromTextArea();
            view.getErrorArea().setText(""); // no exception was thrown, so clear errors
            return notes;
        }
        catch (Exception e) {
            view.getErrorArea().setText(e.getMessage());
            view.playButtons.setEnabled(false);
            view.formatBars.setEnabled(false);
            
            if (e instanceof IllegalArgumentException == false)
                e.printStackTrace();
        }
        return null;
    }
    
    // privates
    
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
        
        view.playButtons.setEnabled(enable);
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

    
    /** Make sure to always call this synchronized(playerLock)! */
    private void startOrStopPlayer(boolean isStop) {
        view.playButtons.setPlaying(isStop == false);
        
        view.enableUiOnPlaying(isStop);
    
        if (isStop) {
            if (player != null) {
                player.close();
                player = null;
            }
            readNotesFromTextArea(); // enable note controls
            view.notesText.requestFocus();
        }
        else { // is start
            final Note[] notesArray = readNotesFromTextArea();
            final Note[][] sounds = view.convertNotesToChords(notesArray); // optional override
            startPlayer(sounds, true); // thread is running now
        }
    }
    
    /** Make sure to always call this synchronized(playerLock)! */
    private void startPlayer(Note[][] sounds, boolean setSoundsAndIndex) {
        // disable controls that may have been enabled by readNotesFromTextArea()
        view.timeSignatureChoice.setEnabled(false);
        view.tempoSpinner.setEnabled(false);
        view.formatBars.setEnabled(false);
        
        final SoundChannel pianoKeyConnector = new PianoKeyConnector(view.piano);
        this.player = new Player(pianoKeyConnector); // is synchronized because called from startOrStop()

        // to allow "Stop" while playing, all playing happens in a background thread
        final Thread playerThread = new Thread(() -> playNotes(sounds, setSoundsAndIndex));
        playerThread.start();
    }
    
    /** 
     * Plays given sounds and strikes according piano keys.
     * This method runs in a background-thread (to be interruptable), 
     * so any call to Swing must happen via SwingUtilities.invokeXXX().
     * @param sounds the intervals or chords to play on piano.
     */
    private void playNotes(Note[][] sounds, boolean setSoundsAndIndex) {
        if (setSoundsAndIndex)
            this.sounds = sounds;
        
        final Player myPlayer = this.player; // remember which player to use
        
        boolean interrupted = false;
        RuntimeException exception = null;
        int startIndex = setSoundsAndIndex ? this.currentSoundIndex : 0; // 0 = single note play
        
        for (int i = startIndex; interrupted == false && exception == null && i < sounds.length; i++) { // play all notes
            if (setSoundsAndIndex)
                this.currentSoundIndex = i;
            
            final Note[] currentSound = sounds[i];
            try {
                SwingUtilities.invokeAndWait(() -> { // waits synchronously for each note
                    synchronized(playerLock) {
                        if (myPlayer == this.player) // Start/Stop button could be clicked several times!
                            myPlayer.playSimultaneously(currentSound); // this causes Swing UI updates and thus must run in EDT
                    }
                });
            }
            catch (Exception e) { // unlikely invokeAndWait() problems
                exception = new RuntimeException(e);
            }
            
            synchronized(playerLock) {
                interrupted = (myPlayer != this.player); // "Stop" button was pressed
            }
        }
        
        if (interrupted)
            this.currentSoundIndex--;

        SwingUtilities.invokeLater(() -> { // causes Swing UI updates and thus must run in EDT
            synchronized(playerLock) {
                if (myPlayer == this.player) { // not stopped by user, do self-stop
                    startOrStopPlayer(true); // stop true
                    
                    if (setSoundsAndIndex) // rewind
                        this.currentSoundIndex = -1;
                }
            }
        });
        
        if (exception != null) // throw only after correct termination
            throw exception;
    }
    
    
    private void skip(boolean forward) {
        if (sounds == null) {
            final Note[] notesArray = readNotesFromTextArea();
            sounds = view.convertNotesToChords(notesArray); 
            currentSoundIndex = -1;
        }

        int startIndex = currentSoundIndex;
        do { // search for a non-rest note
            skipCurrentSoundIndex(forward);
        }
        while (startIndex != currentSoundIndex && sounds[currentSoundIndex][0].isRest());
        
        if (sounds[currentSoundIndex][0].isRest() == false)
            startPlayer(new Note[][] { sounds[currentSoundIndex] }, false);
    }

    private void skipCurrentSoundIndex(boolean increment) {
        if (sounds == null)
            return;
        
        if (increment) {
            currentSoundIndex++;
            if (currentSoundIndex >= sounds.length)
                currentSoundIndex = 0;
        }
        else {
            currentSoundIndex--;
            if (currentSoundIndex < 0)
                currentSoundIndex = sounds.length - 1;
        }
    }
}