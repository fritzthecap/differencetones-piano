package fri.music.instrument.notespiano;

import java.util.List;
import javax.swing.SwingUtilities;
import fri.music.SoundChannel;
import fri.music.instrument.PianoWithSound;
import fri.music.player.Note;
import fri.music.player.Player;
import fri.music.player.notelanguage.MelodyFactory;

/**
 * Manages the player control buttons and the player-thread.
 */
class PlayController implements PlayControlButtons.Listener
{
    private final NotesPianoPlayer view;
    
    private final Object playerLock = new Object();
    private Player player;
    
    private Note[][] sounds; // playing notes, possibly polyphonic
    private int currentSoundIndex;
    private boolean playingReverse;
    
    PlayController(NotesPianoPlayer view) {
        this.view = view;
    }
    
    // interface PlayControlButtons.Listener
    
    @Override
    public void fastBackwardPressed() {
        gotoStartOrEnd(true);
    }
    
    @Override
    public void backwardPressed() {
        playStep(false);
    }
    
    @Override
    public void reversePressed() {
        playContinually(true);
    }
    
    @Override
    public void playPressed() {
        playContinually(false);
    }
    
    @Override
    public void forwardPressed() {
        playStep(true);
    }
    
    @Override
    public void fastForwardPressed() {
        gotoStartOrEnd(false);
    }
    
    // methods called by view
    
    Note[] readNotesFromTextArea(boolean clearCurrentSounds) throws IllegalArgumentException {
        if (clearCurrentSounds)
            sounds = null;
        
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
            final MelodyFactory melodyFactory = view.newMelodyFactory();
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
    
    // helpers
    
    /** "Play" or "Reverse" callback. */
    private void playContinually(boolean reverse) {
        synchronized(playerLock) {
            final boolean isStart = (player == null);
            if (isStart == false) { // currently playing, either reverse or forward
                startOrStopPlayer(false, playingReverse); // stop current playing
                if (playingReverse == reverse)
                    return; // not changing direction, no need to restart
            }
            else {
                skipCurrentSoundIndex( ! reverse ); // else it would repeat current note
            }
            startOrStopPlayer(true, reverse); // start, or restart when changing direction
        }
    }
    
    /** Single step callback. */
    private void playStep(boolean forward) {
        synchronized(playerLock) {
            if (player != null)
                startOrStopPlayer(false, playingReverse); // stop when currently playing
            else
                skip(forward); // play single note
        }
    }
    
    /** Fast-forward or -backward. */
    private void gotoStartOrEnd(boolean toStart) {
        synchronized(playerLock) {
            if (player != null)
                startOrStopPlayer(false, playingReverse);
            
            if (sounds != null)
                if (toStart) // fastBackard
                    currentSoundIndex = -1; // forward will increment
                else // fastForward
                    currentSoundIndex = sounds.length; // backward will decrement
        }
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
    private void startOrStopPlayer(boolean isStart, boolean reverse) {
        view.playButtons.setPlaying(isStart == true, reverse);
        
        view.enableUiOnPlaying(isStart == false);
    
        if (isStart) {
            final Note[] notesArray = readNotesFromTextArea(false);
            final Note[][] sounds = view.convertNotesToChords(notesArray); // optional override
            startPlayer(sounds, true, reverse); // thread is running now
        }
        else { // is stop
            if (player != null) {
                player.close();
                player = null;
            }
            readNotesFromTextArea(false); // enable note controls
            view.notesText.requestFocus();
        }
    }
    
    /** Make sure to always call this synchronized(playerLock)! */
    private void startPlayer(Note[][] sounds, boolean setSoundsAndIndex, boolean reverse) {
        // disable controls that may have been enabled by readNotesFromTextArea()
        view.timeSignatureChoice.setEnabled(false);
        view.tempoSpinner.setEnabled(false);
        view.formatBars.setEnabled(false);
        
        final SoundChannel pianoKeyConnector = new PianoKeyConnector(view.piano);
        this.player = new Player(pianoKeyConnector); // is synchronized because called from startOrStop()

        // to allow "Stop" while playing, all playing happens in a background thread
        final Thread playerThread = new Thread(() -> playNotes(sounds, setSoundsAndIndex, reverse));
        playerThread.start();
    }
    
    /** 
     * Plays given sounds and strikes according piano keys.
     * This method runs in a background-thread (to be interruptable), 
     * so any call to Swing must happen via SwingUtilities.invokeXXX().
     * @param sounds the intervals or chords to play on piano.
     * @param setSoundsAndIndex hen false, this is a single-step play, else it plays more.
     */
    private void playNotes(Note[][] sounds, boolean setSoundsAndIndex, boolean reverse) {
        if (setSoundsAndIndex) {
            this.sounds = sounds;
            this.playingReverse = reverse;
            if (reverse && currentSoundIndex <= 0) // happens when initially pressing "Reverse"
                currentSoundIndex = sounds.length - 1;
        }
        
        final Player myPlayer = this.player; // remember which player to use
        
        boolean interrupted = false;
        RuntimeException exception = null;
        int startIndex = setSoundsAndIndex ? Math.max(currentSoundIndex, 0) : 0; // 0 = single note play
        
        for (int i = startIndex; interrupted == false && exception == null && i >= 0 && i < sounds.length; ) { // play all notes
            if (setSoundsAndIndex)
                currentSoundIndex = i;
            
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
            
            if (reverse)
                i--;
            else
                i++;
        }
        
        if (interrupted) // was stopped but already incremented/decremented, correct that
            if (reverse)
                currentSoundIndex++;
            else
                currentSoundIndex--;

        SwingUtilities.invokeLater(() -> { // causes Swing UI updates and thus must run in EDT
            synchronized(playerLock) {
                if (myPlayer == this.player) { // not stopped by user, do self-stop
                    startOrStopPlayer(false, playingReverse);
                    
                    if (setSoundsAndIndex) // rewind
                        this.currentSoundIndex = -1;
                }
            }
        });
        
        if (exception != null) // throw only after correct termination
            throw exception;
    }
    
    
    private void skip(boolean forward) {
        if (sounds == null) { // no "Play" was pressed before "Skip to Next"
            final Note[] notesArray = readNotesFromTextArea(false);
            sounds = view.convertNotesToChords(notesArray);
            if (currentSoundIndex == 0) // if initial state
                currentSoundIndex = -1; // will be changed below
        }

        int startIndex = Math.max(0, currentSoundIndex);
        do { // search for a non-rest note
            skipCurrentSoundIndex(forward);
        }
        while (startIndex != currentSoundIndex && sounds[currentSoundIndex][0].isRest());
        
        if (sounds[currentSoundIndex][0].isRest() == false) {
            synchronized(playerLock) {
                startPlayer(new Note[][] { sounds[currentSoundIndex] }, false, false);
            }
        }
    }

    private void skipCurrentSoundIndex(boolean increment) {
        if (sounds != null) {
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
}