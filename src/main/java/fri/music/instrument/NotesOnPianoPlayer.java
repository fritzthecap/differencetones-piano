package fri.music.instrument;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.WindowListener;
import java.util.List;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.SwingUtilities;
import fri.music.SoundChannel;
import fri.music.player.Note;
import fri.music.player.Player;
import fri.music.player.notelanguage.MelodyFactory;
import fri.music.swingutils.ButtonUtil;
import fri.music.swingutils.TextAreaUtil;

/**
 * Piano that can play notes.
 */
public class NotesOnPianoPlayer
{
    private final PianoWithSound.Configuration config;
    private final SoundChannel channel;
    
    private String melody;
    
    private JComponent playerPanel;
    private PianoWithSound piano;
    private JTextArea notesText;
    
    private JButton play;
    private Player player;
    
    private final Object lock = new Object();
    
    public NotesOnPianoPlayer(SoundChannel channel) {
        this(null, channel);
    }
    public NotesOnPianoPlayer(PianoWithSound.Configuration config, SoundChannel channel) {
        this(config, channel, null);
    }
    public NotesOnPianoPlayer(PianoWithSound.Configuration config, SoundChannel channel, String melody) {
        this.config = config;
        this.channel = channel;
        this.melody = melody;
    }
    
    /**
     * Call this to get the UI.
     * @return a panel containing the piano and a notes text area.
     */
    public JComponent getPlayer() {
        if (this.playerPanel != null)
            return this.playerPanel; // just one view, due to mouseHandler that stores UI-state
        
        this.piano = newPiano(config, channel);
        
        final JComponent playerPanel = piano.getKeyboard(); // using the piano's panel
        
        this.notesText = new JTextArea();
        notesText.setToolTipText("Write Notes to be Played on Keyboard");
        TextAreaUtil.addUndoManagement(notesText);
        if (melody != null && melody.length() > 0) {
            notesText.setText(melody);
            final int lines = (int) melody.chars().filter(c -> c == '\n').count();
            notesText.setRows(Math.max(2, Math.min(8, lines + 1))); // min 2, max 12 lines
        }
        else {
            notesText.setRows(2);
        }
        final JScrollPane scrollingNotesText = new JScrollPane(notesText);
        
        this.play = new JButton("Play");
        play.setToolTipText("Play Notes Written in Text Area");
        play.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                final boolean isStop = play.getText().equals("Stop");
                startOrStopPlaying(isStop);
            }
        });
        final JPanel playButtonPanel = new JPanel(new FlowLayout()); // else button is too big
        playButtonPanel.add(play);
        
        final JPanel notesPanel = new JPanel(new BorderLayout());
        notesPanel.add(scrollingNotesText, BorderLayout.CENTER);
        notesPanel.add(playButtonPanel, config.isVertical ? BorderLayout.NORTH : BorderLayout.WEST);
        
        playerPanel.add(notesPanel, BorderLayout.CENTER); // to CENTER, so that user can resize area
        
        return this.playerPanel = playerPanel;
    }
    
    /**
     * Factory method for the piano where notes are played upon.
     * This implementation returns the simplest possible piano.
     * To be overridden for other types of piano.
     * @return a piano where notes can be played upon.
     */
    protected PianoWithSound newPiano(PianoWithSound.Configuration config, SoundChannel channel) {
        return new PianoWithVolume(config, channel) {
            @Override
            protected String getPianoPanelBorderLayoutConstraint() {
                return config.isVertical ? BorderLayout.EAST : BorderLayout.SOUTH;
            }
            @Override
            protected String getControlPanelBorderLayoutConstraint() {
                return config.isVertical ? BorderLayout.WEST : BorderLayout.NORTH;
            }
        };
    }
    
    /** @return the listener of created piano. */
    public WindowListener getWindowClosingListener() {
        return piano.getWindowClosingListener();
    }


    private void startOrStopPlaying(boolean isStop) {
        play.setText(isStop ? "Play" : "Stop");
        notesText.setEnabled(isStop);
        setPianoKeysIgnoreMouse(isStop);
        
        if (isStop) {
            if (player != null) {
                synchronized(lock) {
                    player.close();
                    player = null;
                }
            }
        }
        else {
            checkAndPlayNotes(play);
        }
    }
    
    private void setPianoKeysIgnoreMouse(boolean dontIgnore) {
        for (PianoWithSound.Keyboard.Key key : piano.getKeys())
            key.setIgnoreMouse(dontIgnore == false);
    }
    
    private void checkAndPlayNotes(final JButton play) {
        final String notesString = notesText.getText();
        try {
            final MelodyFactory melodyFactory = new MelodyFactory(); // TODO: configure BPM and beat-type from UI
            final Note[] notesArray = melodyFactory.translate(notesString);
            checkNotesRange(notesArray);
            
            // to allow "Stop" while playing, all playing happens in a background thread
            final Thread playerThread = new Thread(() -> playNotes(notesArray));
            playerThread.start();
        }
        catch (Exception e) {
            JOptionPane.showMessageDialog(notesText, "Error: "+e.getMessage());
            startOrStopPlaying(true);
        }
    }
    
    private void checkNotesRange(Note[] notesArray) {
        final List<PianoWithSound.Keyboard.Key> keys = piano.getKeys();
        final int lowestMidiNumber  = keys.get(0).midiNoteNumber;
        final int highestMidiNumber = keys.get(keys.size() - 1).midiNoteNumber;
        for (Note note : notesArray)
            if (lowestMidiNumber > note.midiNumber || highestMidiNumber < note.midiNumber)
                throw new IllegalArgumentException("Note is not in range: "+note.ipnName);
    }

    /** This method runs in a background-thread, so any call to Swing must happen via SwingUtilities.invokeXXX(). */
    private void playNotes(Note[] notesArray) {
        final SoundChannel pianoKeyConnector = new PianoKeyConnector(piano);
        
        if (player != null)
            player.close();
        player = new Player(pianoKeyConnector);
        
        for (final Note note : notesArray) { // play all notes one by one
            try {
                SwingUtilities.invokeAndWait(() -> { // waits synchronously for each note
                    synchronized(lock) {
                        if (player != null) // "Stop" button could set it to null at any time
                            player.play(note); // this causes Swing UI updates and thus must run in EDT
                    }
                });
            }
            catch (Exception e) {
                throw new RuntimeException(e);
            }
        }
        
        SwingUtilities.invokeLater(() -> {
            startOrStopPlaying(true); // enables "Play" and closes player
        });
    }
    

    /** Connects Notes array to the piano's keys. */
    private static class PianoKeyConnector implements SoundChannel
    {
        private final PianoWithSound piano;
        private final PianoWithSound.MouseHandler mouseHandler;
        private final List<PianoWithSound.Keyboard.Key> keys;
        private final int lowestMidiNumber;
        
        PianoKeyConnector(PianoWithSound piano) {
            this.piano = piano;
            this.mouseHandler = piano.getMouseHandler();
            this.keys = piano.getKeys();
            this.lowestMidiNumber = keys.get(0).midiNoteNumber;
        }
        
        @Override
        public void noteOn(int midiNoteNumber, int velocity) {
            final PianoWithSound.Keyboard.Key key = findKey(midiNoteNumber);
            //velocityChange(velocity);
            pressOrReleaseKey(key, true);
        }
        @Override
        public void noteOff(int midiNoteNumber) {
            final PianoWithSound.Keyboard.Key key = findKey(midiNoteNumber);
            pressOrReleaseKey(key, false);
        }
        
        @Override
        public void volumeChange(int volume) {
        }
        @Override
        public void allNotesOff() {
        }
        
        private PianoWithSound.Keyboard.Key findKey(int midiNoteNumber) {
            return keys.get(midiNoteNumber - lowestMidiNumber);
        }
        
        private MouseEvent createMouseEvent(Component eventSource, int mouseEventId) {
            return new MouseEvent(
                    eventSource,
                    mouseEventId, // MouseEvent.MOUSE_XXX
                    System.currentTimeMillis(), // when
                    0, // modifiers
                    2, 2, /// x, y 
                    1, // click count
                    false); // popup trigger
        }
        
        private void pressOrReleaseKey(PianoWithSound.Keyboard.Key key, boolean press) {
            if (press) {
                ButtonUtil.press(key);
                mouseHandler.mousePressed(createMouseEvent(key, MouseEvent.MOUSE_PRESSED));
            }
            else { // release
                ButtonUtil.release(key);
                mouseHandler.mouseReleased(createMouseEvent(key, MouseEvent.MOUSE_RELEASED));
            }
        }
    }
}