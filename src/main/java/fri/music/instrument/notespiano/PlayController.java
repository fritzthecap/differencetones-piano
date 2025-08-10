package fri.music.instrument.notespiano;

import java.util.List;
import java.util.Objects;
import javax.swing.SwingUtilities;
import fri.music.SoundChannel;
import fri.music.instrument.PianoWithSound;
import fri.music.instrument.wave.PianoKeyConnector;
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
    
    private SoundChannel pianoKeyConnector;
    
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
        playSingleNote(false, true);
    }
    @Override
    public void backwardReleased() {
        playSingleNote(false, false);
    }
    @Override
    public void reversePressed() {
        playMelodyContinually(true);
    }
    @Override
    public void playPressed() {
        playMelodyContinually(false);
    }
    @Override
    public void forwardPressed() {
        playSingleNote(true, true);
    }
    @Override
    public void forwardReleased() {
        playSingleNote(true, false);
    }
    @Override
    public void fastForwardPressed() {
        gotoStartOrEnd(false);
    }
    
    // methods called by view
    
    Note[] readNotesFromTextArea(boolean clearCurrentSounds) throws IllegalArgumentException {
        if (clearCurrentSounds)
            sounds = null; // currentSoundIndex stays on its value, to further support single-steps
        
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
    private void playMelodyContinually(boolean reverse) {
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
    private void playSingleNote(boolean forward, boolean buttonPressed) {
        synchronized(playerLock) {
            if (player != null)
                startOrStopPlayer(false, playingReverse); // stop when currently playing
            else
                skip(forward, buttonPressed); // play single note
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
            final Note[][] sounds = view.convertNotesToChords(notesArray); // is optionally overridden!
            if (sounds != null && sounds.length > 0) // could have caused a reported exception
                startPlayer(sounds, reverse); // thread is running now
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
    private void startPlayer(Note[][] sounds, boolean reverse) {
        // disable controls that may have been enabled by readNotesFromTextArea()
        view.timeSignatureChoice.setEnabled(false);
        view.tempoSpinner.setEnabled(false);
        view.formatBars.setEnabled(false);
        
        this.player = new Player(pianoKeyConnector()); // is synchronized because called from startOrStop()

        // to allow "Stop" while playing, all playing happens in a background thread
        final Thread playerThread = new Thread(() -> playNotes(sounds, reverse));
        playerThread.start();
    }
    
    /** 
     * Plays given sounds and strikes according piano keys.
     * This method runs in a background-thread (to be interruptable), 
     * so any call to Swing must happen via SwingUtilities.invokeXXX().
     * @param sounds the intervals or chords to play on piano.
     * @param playingReverse false when playing backward, true when playing forward.
     */
    private void playNotes(Note[][] sounds, boolean playingReverse) {
        this.sounds = Objects.requireNonNull(sounds);
        this.playingReverse = playingReverse;
        ensureExistingStartingIndex();
        
        final Player myPlayer = this.player; // remember which player to use
        
        boolean interrupted = false;
        RuntimeException exception = null;
        
        for (int i = currentSoundIndex; interrupted == false && exception == null && i >= 0 && i < sounds.length; ) { // play all notes
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
            
            if (playingReverse)
                i--;
            else
                i++;
        }
        
        if (interrupted) // was stopped but already incremented/decremented, correct that
            if (playingReverse)
                currentSoundIndex++;
            else
                currentSoundIndex--;

        SwingUtilities.invokeLater(() -> { // causes Swing UI updates and thus must run in EDT
            synchronized(playerLock) {
                if (myPlayer == this.player) { // not stopped by user, do self-stop
                    startOrStopPlayer(false, playingReverse);
                    this.currentSoundIndex = -1;
                }
            }
        });
        
        if (exception != null) // throw only after correct termination
            throw exception;
    }

    
    private void ensureExistingStartingIndex() {
        final int firstIndex = 0;
        final int lastIndex = sounds.length - 1;
        
        if (playingReverse == true && 
                (currentSoundIndex <= firstIndex || // happens when initially pressing "Reverse"
                 currentSoundIndex > lastIndex))
            currentSoundIndex = lastIndex;
        
        if (playingReverse == false && 
                (currentSoundIndex >= lastIndex ||
                currentSoundIndex < firstIndex))
            currentSoundIndex = firstIndex;
    }
    
    
    private void skip(boolean forward, boolean buttonPressed) {
        if (buttonPressed == false) { // released
            if (sounds != null && sounds.length > 0) {
                for (Note note : sounds[currentSoundIndex]) // ArrayIndexOutOfBoundsException when min FORTH max FIFTH and LIMIT_5 and pressing "rewind" and then "step forward"
                    pianoKeyConnector.noteOff(note.midiNumber);
                
                view.enableUiOnPlaying(true);
            }
        }
        else {
            if (sounds == null) { // no "Play" was pressed before "Step Forward"
                final Note[] notesArray = readNotesFromTextArea(false);
                sounds = view.convertNotesToChords(notesArray);
                if (sounds == null || sounds.length <= 0)
                    return;
                
                if (currentSoundIndex == 0) // if initial state
                    currentSoundIndex = -1; // will be changed below
            }

            final int startIndex = Math.max(0, currentSoundIndex);
            do { // search for a non-rest note
                skipCurrentSoundIndex(forward);
            }
            while (startIndex != currentSoundIndex && sounds[currentSoundIndex][0].isRest());
            
            if (sounds[currentSoundIndex][0].isRest() == false) {
                view.enableUiOnPlaying(false);

                for (Note note : sounds[currentSoundIndex])
                    pianoKeyConnector().noteOn(note.midiNumber, note.volume);
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
    
    private SoundChannel pianoKeyConnector() {
        if (pianoKeyConnector == null)
            pianoKeyConnector = new PianoKeyConnector(view.piano);
        return pianoKeyConnector;
    }
}