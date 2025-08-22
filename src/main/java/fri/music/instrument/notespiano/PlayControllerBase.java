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
public class PlayControllerBase implements PlayControlButtons.Listener
{
    private final Object playerLock = new Object();
    private Player player;
    
    private final NotesPianoPlayer notesPianoPlayer;
    
    private NotesTextPanelBase view;
    
    private Note[][] sounds; // playing notes, possibly polyphonic
    private int currentSoundIndex;
    private boolean playingReverse;
    
    private SoundChannel pianoKeyConnector;
    
    public PlayControllerBase(NotesPianoPlayer notesPianoPlayer) {
        this.notesPianoPlayer = notesPianoPlayer;
    }
    
    public void setViewBase(NotesTextPanelBase view) {
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
    
    /**
     * Enables to catch a note when starting or stopping it by override.
     * @param soundChannel the pianoKeyConnector.
     * @param note the note to start or stop.
     * @param noteOn when true, start, else stop note.
     */
    protected void playOrStopNote(SoundChannel soundChannel, Note note, boolean noteOn) {
        if (noteOn)
            soundChannel.noteOn(note.midiNumber, note.volume);
        else
            soundChannel.noteOff(note.midiNumber);
    }
    
    /**
     * Called before and after this controller actually plays or stops a note.
     * When a note goes on, this is called twice, once before turning on the sound,
     * once after the sound was turned on (but is not yet off).
     * The same is when sound goes off, it is called twice.
     * @param before true when called before note action, false when called after.
     * @param isNoteOn true when note starts, else false.
     */
    protected void aroundPlayEvent(boolean before, boolean isNoteOn) {
    }
    
    // methods called by player

    Note[][] readNotesFromTextArea(boolean clearCurrentSounds) throws IllegalArgumentException {
        if (clearCurrentSounds)
            sounds = null; // currentSoundIndex stays on its value, to further support single-steps
        
        final String notesString = view.notesText.getText();
        final boolean enable;
        final Note[][] notes;
        
        if (notesString.trim().isEmpty()) { // nothing in text-area
            notes = new Note[0][];
            enable = false; // no actions can be performed when empty
            
            onEmptyNotes();
        }
        else { // parse notes
            final MelodyFactory melodyFactory = notesPianoPlayer.newMelodyFactory();
            notes = melodyFactory.translate(notesString); // throws exceptions
            checkNotesRange(notes); // throws exceptions
            
            enable = true; // no exception was thrown until here, so enable actions
            
            onNonEmptyNotes(melodyFactory.getTimeSignatureOnTop(), melodyFactory.getTempoOnTop());
        }
        
        view.playButtons.setEnabled(enable);
        view.formatBars.setEnabled(enable);
        
        return notes;
    }
    
    
    /** Does nothing, to be overridden for extended view. */
    protected void onEmptyNotes() {
    }
    
    /** Does nothing, to be overridden for extended view. */
    protected void onNonEmptyNotes(String timeSignatureOnTop, Integer tempoOnTop) {
    }

    /** Does nothing, to be overridden for extended view. */
    protected void onStartPlayer() {
    }

    /** Does nothing, to be overridden for extended view. */
    protected void onEnableUiOnPlaying(boolean isStop) {
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
    
    
    private void checkNotesRange(Note[][] notesArray) {
        if (notesArray.length <= 0)
            throw new IllegalArgumentException("No notes were given!");
        
        final List<PianoWithSound.Keyboard.Key> keys = notesPianoPlayer.piano.getKeys();
        final int lowestMidiNumber  = keys.get(0).midiNoteNumber;
        final int highestMidiNumber = keys.get(keys.size() - 1).midiNoteNumber;
        for (Note[] chords : notesArray)
            for (Note note : chords)
                if (note.isRest() == false)
                    if (lowestMidiNumber > note.midiNumber || highestMidiNumber < note.midiNumber)
                        throw new IllegalArgumentException("Note is not in keyboard range: "+note.ipnName);
    }
    
    /** Make sure to always call this synchronized(playerLock)! */
    private void startOrStopPlayer(boolean isStart, boolean reverse) {
        view.playButtons.setPlaying(isStart == true, reverse);
        
        enableUiOnPlaying(isStart == false);
    
        if (isStart) {
            final Note[][] notesArray = readNotesFromTextArea(false);
            final Note[][] sounds = notesPianoPlayer.convertNotes(notesArray); // is optionally overridden!
            if (sounds != null && sounds.length > 0) // could have caused a reported exception
                startPlayer(sounds, reverse); // thread is running now
            else
                startOrStopPlayer(false, reverse); // stop, there were errors reported
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
        onStartPlayer();
        view.formatBars.setEnabled(false);
        
        // following assignment is synchronized because called from startOrStop()
        this.player = new Player(pianoKeyConnector()) {
            /** Overridden to let catch the played note. */
            @Override
            protected void noteOnOrOff(SoundChannel soundChannel, Note note, boolean on) {
                playOrStopNote(soundChannel, note, on);
            }
        };

        // to allow "Stop" while playing, all playing happens in a background thread
        final Thread playerThread = new Thread(() -> playNotes(sounds, reverse));
        playerThread.start();
    }
    
    /** 
     * Plays given sounds and strikes according piano keys.
     * This method runs in a background-thread (to be interruptable), 
     * so any call to Swing must happen via SwingUtilities.invokeXXX()!
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
        if (buttonPressed) {
            if (sounds == null) { // no "Play" was pressed before "Step Forward"
                final Note[][] notesArray = readNotesFromTextArea(false);
                sounds = notesPianoPlayer.convertNotes(notesArray);
                if (sounds == null || sounds.length <= 0)
                    return;
                
                if (currentSoundIndex == 0) // if initial state
                    currentSoundIndex = -1; // will be incremented in skipCurrentSoundIndex()
            }

            // search for a non-rest note, but avoid endless loop when having only rests
            final int startIndex = (currentSoundIndex <= 0) ? (sounds.length - 1) : currentSoundIndex;
            do {
                skipCurrentSoundIndex(forward);
            }
            while (startIndex != currentSoundIndex && 
                    sounds[currentSoundIndex][0].isRest() || sounds[currentSoundIndex][0].durationMilliseconds <= 0);
            
            final Note firstNote = sounds[currentSoundIndex][0];
            if (firstNote.isRest() == false) {
                enableUiOnPlaying(false);
                
                for (Note note : sounds[currentSoundIndex]) // starts playing all notes
                    playOrStopNote(pianoKeyConnector(), note, true);
            }
        }
        else { // button released
            if (sounds != null && sounds.length > 0 && 
                    currentSoundIndex >= 0 && sounds[currentSoundIndex][0].isRest() == false)
            {
                for (Note note : sounds[currentSoundIndex]) // stops playing all notes
                    playOrStopNote(pianoKeyConnector(), note, false);
                
                enableUiOnPlaying(true);
            }
        }
    }


    private void enableUiOnPlaying(boolean isStop) {
        onEnableUiOnPlaying(isStop);
        
        view.notesText.setEditable(isStop);
        notesPianoPlayer.enableUiOnPlaying(isStop);
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
            pianoKeyConnector = new PianoKeyConnector(notesPianoPlayer.piano) {
                @Override
                public void noteOn(int midiNoteNumber, int velocity) {
                    aroundPlayEvent(true, true);
                    super.noteOn(midiNoteNumber, velocity);
                    aroundPlayEvent(false, true);
                }
                @Override
                public void noteOff(int midiNoteNumber) {
                    aroundPlayEvent(true, false);
                    super.noteOff(midiNoteNumber);
                    aroundPlayEvent(false, false);
                }
                @Override
                public void allNotesOff() {
                    aroundPlayEvent(true, false);
                    super.allNotesOff();
                    aroundPlayEvent(false, false);
                }
            };
        return pianoKeyConnector;
    }
}