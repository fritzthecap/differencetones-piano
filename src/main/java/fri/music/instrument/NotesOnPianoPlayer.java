package fri.music.instrument;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.WindowListener;
import java.util.List;
import java.util.Objects;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSpinner;
import javax.swing.JSplitPane;
import javax.swing.JTextArea;
import javax.swing.SpinnerModel;
import javax.swing.SpinnerNumberModel;
import javax.swing.SwingUtilities;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import fri.music.SoundChannel;
import fri.music.player.Note;
import fri.music.player.Player;
import fri.music.player.notelanguage.MelodyFactory;
import fri.music.swingutils.ButtonUtil;
import fri.music.swingutils.DialogUtil;
import fri.music.swingutils.SmartComboBox;
import fri.music.swingutils.SmartPanel;
import fri.music.swingutils.TextAreaUtil;

/**
 * A notes area that can play user-editable notes on a given piano.
 */
public class NotesOnPianoPlayer
{
    private final PianoWithSound piano;
    private String melody;
    
    private JComponent playerPanel; // the component
    
    private JButton play;
    private JButton formatBars;
    private JTextArea notesText;
    private JTextArea errors;
    private JSpinner tempoSpinner;
    private JComboBox<String> timeSignatureChoice;
    
    private Player player;
    private final Object playerLock = new Object();
    
    /** @param piano required, the piano on which to play notes. */
    public NotesOnPianoPlayer(PianoWithSound piano) {
        this(piano, null);
    }
    /**
     * @param piano required, the piano on which to play notes.
     * @param initialMelody optional, an initial tune to put into notes text-area.
     */
    public NotesOnPianoPlayer(PianoWithSound piano, String initialMelody) {
        this.piano = Objects.requireNonNull(piano);
        this.melody = initialMelody;
    }
    
    /**
     * Call this to get the UI.
     * @return a panel containing the piano and a notes text area.
     */
    public JComponent getPlayer() {
        if (this.playerPanel != null)
            return this.playerPanel; // just one view, due to mouseHandler that stores UI-state
        
        final JComponent playerPanel = piano.getKeyboard(); // using the piano's panel
        final JPanel notesPanel = buildNotesPanel();
        playerPanel.add(notesPanel, BorderLayout.CENTER); // to CENTER, so that user can resize area
        
        return this.playerPanel = playerPanel;
    }
    
    /** @return the listener of created piano. */
    public WindowListener getWindowClosingListener() {
        return piano.getWindowClosingListener();
    }
    
    
    /** Factory-method to get a MelodyFactory with current parameters from UI. */
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

    // UI build methods
    
    private JPanel buildNotesPanel() {
        final JComponent notesTextAndErrors = buildNotesTextArea();
        final JPanel notesControlPanel = buildNotesControlPanel();
        
        final JPanel notesPanel = new JPanel(new BorderLayout());
        notesPanel.add(notesTextAndErrors, BorderLayout.CENTER);
        notesPanel.add(notesControlPanel, piano.config.isVertical ? BorderLayout.NORTH : BorderLayout.WEST);
        
        // put initial melody into text-area
        if (melody != null && melody.length() > 0) {
            final int lines = (int) melody.chars().filter(c -> c == '\n').count();
            notesText.setRows(Math.max(2, Math.min(12, lines + 1))); // min 2, max 12 lines
            notesText.setText(melody);
            
            //readNotesFromTextAreaCatchExceptions(); // adjusts tempo and time-signature choosers
        }
        else {
            notesText.setRows(2);
        }
        
        return notesPanel;
    }
    
    private JComponent buildNotesTextArea() {
        this.notesText = new JTextArea();
        notesText.setToolTipText("Write Notes to be Played on Piano");
        TextAreaUtil.addUndoManagement(notesText);
        final JScrollPane notesTextScrollPane = new JScrollPane(notesText);
        notesTextScrollPane.setBorder(BorderFactory.createTitledBorder("Notes"));
        // on every text change, try to translate into notes and render errors
        notesText.getDocument().addDocumentListener(new DocumentListener() {
            @Override
            public void removeUpdate(DocumentEvent e) {
                readNotesFromTextAreaCatchExceptions();
            }
            @Override
            public void insertUpdate(DocumentEvent e) {
                readNotesFromTextAreaCatchExceptions();
            }
            @Override
            public void changedUpdate(DocumentEvent e) {
                readNotesFromTextAreaCatchExceptions();
            }
        });
        
        this.errors = new JTextArea();
        errors.setEditable(false);
        errors.setForeground(Color.RED);
        final JScrollPane errorsScrollPane = new JScrollPane(errors);
        errorsScrollPane.setBorder(BorderFactory.createTitledBorder("Errors"));
        
        final JButton help = new JButton("Help");
        help.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                DialogUtil.showModelessHtmlDialog(
                        "Notes Edit Help", 
                        playerPanel, 
                        SYNTAX_HELP, 
                        new Dimension(660, 460));
            }
        });
        
        final JPanel errorsAndHelpPanel = new JPanel(new BorderLayout());
        errorsAndHelpPanel.add(help, BorderLayout.NORTH);
        errorsAndHelpPanel.add(errorsScrollPane, BorderLayout.CENTER);
        
        final JSplitPane textAreaSplitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, notesTextScrollPane, errorsAndHelpPanel);
        textAreaSplitPane.setResizeWeight(0.86);
        textAreaSplitPane.setDividerLocation(0.86);
        return textAreaSplitPane;
    }
    
    private JPanel buildNotesControlPanel() {
        this.play = new JButton("Play");
        play.setEnabled(false); // assume no notes present
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
        
        final String[] timeSignatures = new String[] {
            "4/4", "3/4", "12/8", "6/8", "2/4", "9/8", "5/4", "7/4",
        };
        this.timeSignatureChoice = new SmartComboBox(timeSignatures);
        timeSignatureChoice.setToolTipText("Time Signature");
        timeSignatureChoice.setEditable(true);
        timeSignatureChoice.setPreferredSize(new Dimension(60, 24)); // else much too wide when editable
        final JPanel timeLayoutPanel = new SmartPanel(new BorderLayout());
        timeLayoutPanel.add(timeSignatureChoice, piano.config.isVertical ? BorderLayout.WEST : BorderLayout.NORTH);
        timeLayoutPanel.setBorder(BorderFactory.createTitledBorder("Bar"));
        
        final SpinnerModel tempoModel = new SpinnerNumberModel(
                Note.DEFAULT_TEMPO_BPM, // initial value
                MelodyFactory.TEMPO_MINIMUM_BPM, // minimum
                MelodyFactory.TEMPO_MAXIMUM_BPM, // maximum
                4); // step width
        this.tempoSpinner = new JSpinner(tempoModel);
        tempoSpinner.setToolTipText("Beats Per Minute");
        final JPanel tempoLayoutPanel = new SmartPanel(new BorderLayout()); // without this panel, field would be sized vertically
        tempoLayoutPanel.add(tempoSpinner, piano.config.isVertical ? BorderLayout.WEST : BorderLayout.NORTH);
        tempoLayoutPanel.setBorder(BorderFactory.createTitledBorder("Tempo"));
        
        this.formatBars = new JButton("Format");
        formatBars.setEnabled(false); // assume no notes present
        formatBars.setToolTipText("Put each Bar into a Separate Line");
        formatBars.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                final Note[] notes = readNotesFromTextAreaCatchExceptions();
                if (notes != null)
                    notesText.setText(newMelodyFactory().toString(notes));
            }
        });
        
        final JPanel notesControlPanel = new JPanel(); // else button is too big
        notesControlPanel.setLayout(new BoxLayout(notesControlPanel, piano.config.isVertical ? BoxLayout.X_AXIS : BoxLayout.Y_AXIS));
        play.setAlignmentX(Component.CENTER_ALIGNMENT);
        notesControlPanel.add(play);
        notesControlPanel.add(Box.createRigidArea(new Dimension(1, 10))); // space to other control fields
        timeLayoutPanel.setAlignmentX(Component.CENTER_ALIGNMENT);
        notesControlPanel.add(timeLayoutPanel);
        tempoLayoutPanel.setAlignmentX(Component.CENTER_ALIGNMENT);
        notesControlPanel.add(tempoLayoutPanel);
        formatBars.setAlignmentX(Component.CENTER_ALIGNMENT);
        notesControlPanel.add(formatBars);
        return notesControlPanel;
    }
    
    // callback methods
    
    /** Make sure to always call this synchronized(playerLock)! */
    private void startOrStopPlayer(boolean isStop) {
        play.setText(isStop ? "Play" : "Stop");
        
        // enable or disable UI
        formatBars.setEnabled(isStop);
        notesText.setEnabled(isStop);
        
        for (PianoWithSound.Keyboard.Key key : piano.getKeys())
            key.setIgnoreMouse(isStop == false);
            // setEnabled() did not reject mouse-events, and prevented key-down rendering
    
        if (isStop) {
            if (player != null) {
                player.close();
                player = null;
            }
            readNotesFromTextAreaCatchExceptions(); // adjust time-signature and tempo choosers
        }
        else {
            checkAndPlayNotes();
            
            // thread is running, disable time-signature and tempo choosers
            timeSignatureChoice.setEnabled(isStop);
            tempoSpinner.setEnabled(isStop);
        }
    }
    
    /** This method is synchronized(playerLock) because called from startOrStopPlayer() only. */
    private void checkAndPlayNotes() {
        final Note[] notesArray = readNotesFromTextArea(); // won't throw exceptions
        // this was called from "Play" that is enabled only when no errors exist
        
        final SoundChannel pianoKeyConnector = new PianoKeyConnector(piano);
        this.player = new Player(pianoKeyConnector); // is synchronized because called from startOrStop()
        
        // to allow "Stop" while playing, all playing happens in a background thread
        final Thread playerThread = new Thread(() -> playNotes(notesArray));
        playerThread.start();
    }
    
    private Note[] readNotesFromTextArea() throws IllegalArgumentException {
        final String notesString = notesText.getText();
        
        final boolean enable;
        final Note[] notes;
        if (notesString.trim().isEmpty() == false) {
            final MelodyFactory melodyFactory = newMelodyFactory();
            notes = melodyFactory.translate(notesString); // throws exceptions!
            checkNotesRange(notes); // throws exceptions!
            
            enable = true; // no exception was thrown until here, so enable actions
            
            // optional BPM and first time-signature were extracted
            final String firstFoundTimeSignature = melodyFactory.getFirstFoundTimeSignature();
            if (firstFoundTimeSignature != null) {
                timeSignatureChoice.setSelectedItem(firstFoundTimeSignature);
                timeSignatureChoice.setEnabled(false); // will be managed in text-area now
            }
            else {
                timeSignatureChoice.setEnabled(true);
            }
            
            final Integer firstFoundBeatsPerMinute = melodyFactory.getFirstFoundTempo();
            if (firstFoundBeatsPerMinute != null) {
                tempoSpinner.setValue(firstFoundBeatsPerMinute);
                tempoSpinner.setEnabled(false); // will be managed in text-area now
            }
            else {
                tempoSpinner.setEnabled(true);
            }
        }
        else {
            notes = new Note[0];
            enable = false; // no actions can be performed when empty
            timeSignatureChoice.setEnabled(true); // let user choose tempo and bar-type
            tempoSpinner.setEnabled(true);
        }
        
        errors.setText(""); // no exception was thrown, so clear errors
        play.setEnabled(enable);
        formatBars.setEnabled(enable);
        
        return notes;
    }
    
    private Note[] readNotesFromTextAreaCatchExceptions() {
        try {
            return readNotesFromTextArea();
        }
        catch (Exception e) {
            errors.setText(e.getMessage());
            play.setEnabled(false);
            formatBars.setEnabled(false);
        }
        return null;
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
     * Converts given 1-dimensional notes from text-area to 2-dimensional chords-array.
     * To be overridden for other conversions than just one note per chord.
     * @param notesArray the notes from text-area to convert to 2-dimensional array.
     */
    protected Note[][] convertNotesToChords(Note[] notesArray) {
        final Note[][] chordsArray = new Note[notesArray.length][];
        for (int i = 0; i < notesArray.length; i++) {
            chordsArray[i] = new Note[1];
            chordsArray[i][0] = notesArray[i];
        }
        return chordsArray;
    }
    
    private void playNotes(Note[] notesArray) {
        playNotes(convertNotesToChords(notesArray));
    }
    
    /** 
     * Plays given chords as sound and strikes according piano keys.
     * This method runs in a background-thread (to be interruptable), 
     * so any call to Swing must happen via SwingUtilities.invokeXXX().
     * @param chordsArray the intervals or chords to play on piano.
     */
    private final void playNotes(Note[][] chordsArray) {
        final Player myPlayer = this.player; // remember which player to use
        
        boolean interrupted = false;
        for (int i = 0; interrupted == false && i < chordsArray.length; i++) { // play all notes one by one
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
    

    /**
     * Used as <code>SoundChannel</code> for <code>Player</code>, 
     * connects notes to the piano's keys.
     */
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
        
        private void pressOrReleaseKey(PianoWithSound.Keyboard.Key key, boolean press) {
            if (press) {
                ButtonUtil.press(key);
                mouseHandler.mousePressed(createMouseEvent(key, MouseEvent.MOUSE_PRESSED));
            }
            else { // release
                ButtonUtil.release(key);
                mouseHandler.mouseReleased(createMouseEvent(key, MouseEvent.MOUSE_RELEASED));
                //mouseHandler.mouseClicked(createMouseEvent(key, MouseEvent.MOUSE_CLICKED));
            }
        }
        
        private MouseEvent createMouseEvent(Component eventSource, int mouseEventId) {
            return new MouseEvent(
                    eventSource, // where the event occurred
                    mouseEventId, // MouseEvent.MOUSE_XXX
                    System.currentTimeMillis(), // when
                    0, // modifiers
                    2, 2, /// x, y coordinates
                    1, // click count
                    false, // popup trigger
                    MouseEvent.BUTTON1); // which button of mouse: left
        }
    }   // end class PianoKeyConnector
    
    

    /** Help taken from JavaDoc of MelodyFactory class. */
    private static final String SYNTAX_HELP = """
<html>
<head></head>
<body>
<h2>Editor Keys</h2>
<ul>
<li><b>Ctrl-X</b> for "Cut" selection</li>
<li><b>Ctrl-C</b> for "Copy" selection</li>
<li><b>Ctrl-V</b> for "Paste" at caret position</li>
<li><b>Ctrl-Z</b> for "Undo" last action</li>
<li><b>Ctrl-Y</b> for "Redo" last "Undo"</li>
</ul>

<h2>Notes Syntax</h2>
<p>
Every note is given as an IPN-name (international pitch notation)
and its length behind a slash, for example: 
</p>
<ul>
<li>"A4/4" for a quarter note on pitch of A4 (440 Hz)</li>
<li>"C#4/2." for a dotted C#4 half note (spans three quarter notes)</li>
<li>"E5/8,3" for a E5 triplet eighth note 
    (each of the triplets must have the ",3" postfix!)</li>
<li>"(G5/1 (G5/1) G5/1)" for a G5 whole note that spans three 4/4 bars</li>
<li>"-/2" for a half rest note.</li>
</ul>
<p>
No space must appear inside notes and their length specification,
but at least one whitespace MUST be between different notes.
In IPN there is no "Eb" or "Bb", you must give "D#" or "A#",
and there is no German "H", such is written as "B".
But you can use both lower or upper case letters in IPN-names.
</p><p>
The time signature can appear on top of the notes, or everywhere in-between,
written as "4/4" or "3/4" or similar.
The tempo can appear as simple BPM number (beats per minute)
on top of the notes only, it can not change in-between.
</p><p>
Do not care about bars, the player will automatically calculate bar bounds
using the given time signature(s).
You can use the "Format" button to put every bar into a separate text line.
</p><p>
Notes connected by a "tie" are notes of same pitch that are played as single note, 
even across several bars.
Ties are started by an parenthesis "(" and ended by ")",
notes in between the start and end note SHOULD be enclosed in
parentheses, because this is how it would look like in written notes.
Space between parentheses and note is allowed.
</p><p>
Notes connected by a "slur" are notes of different pitch that are phrased together, 
even across several bars.
Slurs are started by a brace "{" and ended by "}",
notes in between MUST NOT be enclosed in "{...}",
because it is not clear how to phrase several notes that are all slurred together.
Space between braces and note is allowed.
</p><p>
</p>
</body>
</html>
""";
}