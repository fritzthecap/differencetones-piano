package fri.music.instrument;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.WindowListener;
import java.util.List;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSpinner;
import javax.swing.JSplitPane;
import javax.swing.JTextArea;
import javax.swing.SpinnerModel;
import javax.swing.SpinnerNumberModel;
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
    
    private JButton play;
    private JTextArea notesText;
    private JTextArea errors;
    private JSpinner tempoSpinner;
    private JComboBox<String> timeSignatureChoice;
    
    private Player player;
    private final Object playerLock = new Object();
    
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
        final JPanel notesPanel = buildNotesArea();
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
        return new PianoWithVolume(config, channel);
    }
    
    /** @return the listener of created piano. */
    public WindowListener getWindowClosingListener() {
        return piano.getWindowClosingListener();
    }

    
    
    /** Factory method for MelodyFactory. */
    protected MelodyFactory newMelodyFactory() {
        final String timeSignature = (String) timeSignatureChoice.getSelectedItem();
        final int separatorIndex = timeSignature.indexOf(Note.DURATION_SEPARATOR);
        final String beatsPerBar = timeSignature.substring(0, separatorIndex);
        final String beatType = timeSignature.substring(separatorIndex + 1);
        return new MelodyFactory(
                null,
                (Integer) tempoSpinner.getValue(),
                Integer.valueOf(beatsPerBar),
                Integer.valueOf(beatType));
    }
    

    private JPanel buildNotesArea() {
        this.notesText = new JTextArea();
        notesText.setToolTipText("Write Notes to be Played on Piano");
        TextAreaUtil.addUndoManagement(notesText);
        final JScrollPane notesTextScrollPane = new JScrollPane(notesText);
        notesTextScrollPane.setBorder(BorderFactory.createTitledBorder("Notes"));
        
        this.errors = new JTextArea();
        errors.setEditable(false);
        final JScrollPane errorsScrollPane = new JScrollPane(errors);
        errorsScrollPane.setBorder(BorderFactory.createTitledBorder("Errors"));
        
        final JSplitPane textAreaSplitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, notesTextScrollPane, errorsScrollPane);
        textAreaSplitPane.setResizeWeight(0.8);
        textAreaSplitPane.setDividerLocation(0.8);
        
        this.play = new JButton("Play");
        play.setToolTipText("Play Notes Written in Text Area on Piano");
        play.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                synchronized(playerLock) {
                    final boolean isStop = (player != null);
                    startOrStopPlayer(isStop);
                }
            }
        });
        final JPanel playButtonPanel = new JPanel(); // else button is too big
        playButtonPanel.setLayout(new BoxLayout(playButtonPanel, BoxLayout.Y_AXIS));
        play.setAlignmentX(Component.CENTER_ALIGNMENT);
        playButtonPanel.add(play);
        playButtonPanel.add(Box.createRigidArea(new Dimension(1, 16))); // space to other control fields
        
        final JPanel notesPanel = new JPanel(new BorderLayout());
        notesPanel.add(textAreaSplitPane, BorderLayout.CENTER);
        notesPanel.add(playButtonPanel, config.isVertical ? BorderLayout.NORTH : BorderLayout.WEST);
        
        final String[] timeSignatures = new String[] {
            "4/4", "3/4", "2/4", "12/8", "9/8", "6/8", "5/4", "7/4",
        };
        this.timeSignatureChoice = new JComboBox<>(timeSignatures);
        timeSignatureChoice.setBorder(BorderFactory.createTitledBorder("Bar"));
        timeSignatureChoice.setToolTipText("Time Signature");
        timeSignatureChoice.setEditable(true);
        timeSignatureChoice.setPreferredSize(new Dimension(70, 50)); // else much too wide when editable
        
        timeSignatureChoice.setAlignmentX(Component.CENTER_ALIGNMENT);
        playButtonPanel.add(timeSignatureChoice);
        
        final SpinnerModel tempoModel = new SpinnerNumberModel(
                Note.DEFAULT_TEMPO_BPM, //initial
                40, //minimum
                208, //maximum
                4); //step
        this.tempoSpinner = new JSpinner(tempoModel);
        tempoSpinner.setToolTipText("Beats Per Minute");
        tempoSpinner.setBorder(BorderFactory.createTitledBorder("Tempo"));
        
        final JPanel tempoLayoutPanel = new JPanel(new BorderLayout()); // without this panel, field would be sized
        tempoLayoutPanel.add(tempoSpinner, BorderLayout.NORTH);
        
        tempoLayoutPanel.setAlignmentX(Component.CENTER_ALIGNMENT);
        playButtonPanel.add(tempoLayoutPanel);
        
        if (melody != null && melody.length() > 0) {
            final int lines = (int) melody.chars().filter(c -> c == '\n').count();
            notesText.setRows(Math.max(2, Math.min(8, lines + 1))); // min 2, max 12 lines
            notesText.setText(melody);
            
            try {
                readNotesFromTextArea(); // adjusts tempo and time-signature choosers
            }
            catch (IllegalArgumentException e) {
                errors.setText(e.getMessage());
            }
        }
        else {
            notesText.setRows(2);
        }
        
        return notesPanel;
    }
    
    
    /** Make sure to always call this synchronized(playerLock)! */
    private void startOrStopPlayer(boolean isStop) {
        play.setText(isStop ? "Play" : "Stop");
        notesText.setEnabled(isStop);
        enablePianoMouseEvents(isStop);
    
        if (isStop) {
            if (player != null) {
                player.close();
                player = null;
            }
        }
        else {
            checkAndPlayNotes();
        }
    }
    
    private void enablePianoMouseEvents(boolean enable) {
        for (PianoWithSound.Keyboard.Key key : piano.getKeys())
            key.setIgnoreMouse(enable == false);
    }
    
    private Note[] readNotesFromTextArea() throws IllegalArgumentException {
        final String notesString = notesText.getText();
        final MelodyFactory melodyFactory = newMelodyFactory();
        final Note[] notes = melodyFactory.translate(notesString); // extracts BPM and time signature
        
        if (melodyFactory.translateChangedTimeSignature()) {
            final String timeSignature = ""+melodyFactory.getBeatsPerBar()+Note.DURATION_SEPARATOR+melodyFactory.getBeatType();
            timeSignatureChoice.setSelectedItem(timeSignature);
            timeSignatureChoice.setEnabled(false);
        }
        else {
            timeSignatureChoice.setEnabled(true);
        }
        
        if (melodyFactory.translateChangedTempo()) {
            tempoSpinner.setValue(melodyFactory.getBeatsPerMinute());
            tempoSpinner.setEnabled(false);
        }
        else {
            tempoSpinner.setEnabled(true);
        }
        
        checkNotesRange(notes);
        
        return notes;
    }
    
    /** This method is synchronized(playerLock) because called from startOrStopPlayer() only. */
    private void checkAndPlayNotes() {
        errors.setText("");
        try {
            final Note[] notesArray = readNotesFromTextArea();
            
            final SoundChannel pianoKeyConnector = new PianoKeyConnector(piano);
            this.player = new Player(pianoKeyConnector); // is synchronized because called from startOrStop()
            
            // to allow "Stop" while playing, all playing happens in a background thread
            final Thread playerThread = new Thread(() -> playNotes(notesArray));
            playerThread.start();
        }
        catch (Exception e) {
            JOptionPane.showMessageDialog(notesText, "Error: "+e.getMessage());
            errors.setText(e.getMessage());
            startOrStopPlayer(true); // true = stop
        }
    }
    
    private void checkNotesRange(Note[] notesArray) {
        final List<PianoWithSound.Keyboard.Key> keys = piano.getKeys();
        final int lowestMidiNumber  = keys.get(0).midiNoteNumber;
        final int highestMidiNumber = keys.get(keys.size() - 1).midiNoteNumber;
        for (Note note : notesArray)
            if (note.isRest() == false)
                if (lowestMidiNumber > note.midiNumber || highestMidiNumber < note.midiNumber)
                    throw new IllegalArgumentException("Note is not in range: "+note.ipnName);
    }

    /** 
     * This method runs in a background-thread (to be interruptable), 
     * so any call to Swing must happen via SwingUtilities.invokeXXX().
     */
    private void playNotes(Note[] notesArray) {
        final Player myPlayer = this.player; // remember which player to use
        
        boolean interrupted = false;
        for (int i = 0; interrupted == false && i < notesArray.length; i++) { // play all notes one by one
            final Note note = notesArray[i];
            try {
                SwingUtilities.invokeAndWait(() -> { // waits synchronously for each note
                    synchronized(playerLock) {
                        if (myPlayer == this.player) // Start/Stop button could be clicked several times
                            myPlayer.play(note); // this causes Swing UI updates and thus must run in EDT
                    }
                });
            }
            catch (Exception e) {
                throw new RuntimeException(e);
            }
            
            synchronized(playerLock) {
                interrupted = (myPlayer != this.player); // "Stop" button was pressed
            }
        }
        
        SwingUtilities.invokeLater(() -> { // causes Swing UI updates and thus must run in EDT
            synchronized(playerLock) {
                if (myPlayer == this.player) // still not stopped, do self-stop
                    startOrStopPlayer(true); // enable "Play" and close player
            }
        });
    }
    

    /** Connects Notes array to the piano's keys. */
    private static class PianoKeyConnector implements SoundChannel
    {
        private final PianoWithSound.MouseHandler mouseHandler;
        private final List<PianoWithSound.Keyboard.Key> keys;
        private final int lowestMidiNumber;
        
        PianoKeyConnector(PianoWithSound piano) {
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
                    false, // popup trigger
                    MouseEvent.BUTTON1); // which button of mouse
        }
        
        private void pressOrReleaseKey(PianoWithSound.Keyboard.Key key, boolean press) {
            if (press) {
                ButtonUtil.press(key);
                mouseHandler.mousePressed(createMouseEvent(key, MouseEvent.MOUSE_PRESSED));
                
                // following does not paint the key:
                //key.dispatchEvent(createMouseEvent(key, MouseEvent.MOUSE_PRESSED));
            }
            else { // release
                ButtonUtil.release(key);
                mouseHandler.mouseReleased(createMouseEvent(key, MouseEvent.MOUSE_RELEASED));
                mouseHandler.mouseClicked(createMouseEvent(key, MouseEvent.MOUSE_CLICKED)); // just for correctness
            }
        }
    }   // end class PianoKeyConnector
}