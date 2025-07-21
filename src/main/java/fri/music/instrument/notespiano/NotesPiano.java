package fri.music.instrument.notespiano;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowListener;
import java.util.List;
import java.util.Objects;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSpinner;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.SpinnerModel;
import javax.swing.SpinnerNumberModel;
import javax.swing.SwingUtilities;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.BadLocationException;
import javax.swing.text.JTextComponent;
import fri.music.SoundChannel;
import fri.music.instrument.PianoWithSound;
import fri.music.player.Note;
import fri.music.player.Player;
import fri.music.player.notelanguage.MelodyFactory;
import fri.music.swingutils.DialogUtil;
import fri.music.swingutils.SmartComboBox;
import fri.music.swingutils.SmartPanel;
import fri.music.swingutils.TextAreaActions;

/**
 * A notes area that can play user-editable notes on a given piano.
 */
public class NotesPiano
{
    /** The piano obtained from construction, immutable. */
    protected final PianoWithSound piano;
    
    private JComponent playerPanel; // the component
    
    private JPanel notesControlPanel;
    
    private JButton play;
    private JButton formatBars;
    private JTextArea notesText;
    private JTextComponent error;
    private JSpinner tempoSpinner;
    private JComboBox<String> timeSignatureChoice;
    private JCheckBox writeToNotesCheckbox;
    
    private Player player;
    private final Object playerLock = new Object();
    
    private final NotesWritingMouseListener notesWritingListener = new NotesWritingMouseListener(this);
    
    /** @param piano required, the piano on which to play notes. */
    public NotesPiano(PianoWithSound piano) {
        this.piano = Objects.requireNonNull(piano);
    }
    
    /**
     * Call this to get the UI.
     * @param melody optional, an initial tune to put into notes text-area.
     * @return a panel containing the piano and a notes text area.
     */
    public JComponent getPlayer(String melody) {
        if (this.playerPanel != null)
            return this.playerPanel; // just one view, due to mouseHandler that stores UI-state
        
        final JComponent playerPanel = piano.getKeyboard(); // using the piano's panel
        playerPanel.add(buildNotesPanel(), BorderLayout.CENTER); // to CENTER, so that user can resize area
        
        if (melody != null && melody.length() > 0) { // put initial melody into text-area
            notesText.setText(melody);
            notesText.setCaretPosition(melody.length());
        }
        else { // enable or disable all buttons
            readNotesFromTextAreaCatchExceptions(true);
        }
        
        addPianoMouseListener();
        
        return this.playerPanel = playerPanel;
    }
    
    /** @return the left-side panel with note controls. */
    public JPanel getNotesControlPanel() {
        return notesControlPanel;
    }
    
    /** @return the error text field on bottom of notes area. */
    public JTextComponent getErrorArea() {
        return error;
    }
    
    /** @return the listener of created piano. */
    public WindowListener getWindowClosingListener() {
        return piano.getWindowClosingListener();
    }
    
    
    // methods for NotesWritingMouseListener
    
    String noteLengthForMillis(int durationMillis) {
        Integer beatsPerMinute = (Integer) tempoSpinner.getValue();
        final int beatDurationMillis = MelodyFactory.beatDurationMillis(beatsPerMinute);
        
        final Integer[] timeSignature = timeSignatureParts((String) timeSignatureChoice.getSelectedItem());
        final Integer beatType = timeSignature[1];
        
        return MelodyFactory.noteLengthDivisor(durationMillis, beatType, beatDurationMillis);
    }
    
    void writeSingleNote(String noteWithLength) {
        removeSelectedText(); // overwrite selected text if any
        insertTextAtNearestSpace(noteWithLength); // intelligent insertion
        notesText.requestFocus(); // jump from keyboard to text-area to edit immediately
    }
    
    void playSingleNote(String noteWithLength) {
        final Note[] note = newMelodyFactory().translate(new String[] { noteWithLength });
        new Player(new PianoKeyConnector(piano)).play(note[0]);
    }
    
    
    // UI build methods
    
    private JPanel buildNotesPanel() {
        final JComponent notesTextAndErrors = buildNotesTextArea();
        this.notesControlPanel = buildNotesControlPanel();
        
        final JPanel notesPanel = new JPanel(new BorderLayout());
        notesPanel.add(notesTextAndErrors, BorderLayout.CENTER);
        notesPanel.add(notesControlPanel, piano.config.isVertical ? BorderLayout.NORTH : BorderLayout.WEST);

        return notesPanel;
    }
    
    private JComponent buildNotesTextArea() {
        this.notesText = new JTextArea();
        notesText.addMouseListener(new TextAreaActions(notesText));
        notesText.setToolTipText("Write Notes to be Played on Piano");
        // on every text change, try to translate into notes and render errors
        notesText.getDocument().addDocumentListener(new DocumentListener() {
            @Override
            public void removeUpdate(DocumentEvent e) {
                readNotesFromTextAreaCatchExceptions(true);
            }
            @Override
            public void insertUpdate(DocumentEvent e) {
                readNotesFromTextAreaCatchExceptions(true);
            }
            @Override
            public void changedUpdate(DocumentEvent e) {
                readNotesFromTextAreaCatchExceptions(true);
            }
        });
        final JScrollPane notesTextScrollPane = new JScrollPane(notesText);
        notesTextScrollPane.setBorder(BorderFactory.createTitledBorder("Notes"));
        
        this.error = new JTextField();
        error.setBorder(BorderFactory.createTitledBorder("Error"));
        error.setEditable(false);
        error.setForeground(Color.RED);
        
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
        final JPanel helpLayoutPanel = new JPanel();
        helpLayoutPanel.add(help);
        
        final JPanel errorsAndHelpPanel = new JPanel(new BorderLayout());
        errorsAndHelpPanel.add(helpLayoutPanel, BorderLayout.WEST);
        errorsAndHelpPanel.add(error, BorderLayout.CENTER);
        
        final JPanel textAreaAndError = new JPanel(new BorderLayout());
        textAreaAndError.add(notesTextScrollPane, BorderLayout.CENTER);
        textAreaAndError.add(errorsAndHelpPanel, BorderLayout.SOUTH);
        
        return textAreaAndError;
    }
    
    private JPanel buildNotesControlPanel() {
        this.play = new JButton("Play");
        play.setBackground(Color.GREEN);
        play.setToolTipText("Play Notes in Textarea on Piano");
        play.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                playButtonPressed();
            }
        });
        
        this.formatBars = new JButton("Format");
        formatBars.setToolTipText("Put each Bar into a Separate Line");
        formatBars.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                formatNotes();
            }
        });
        
        final String[] timeSignatures = new String[] {
            "4/4", "3/4", "12/8", "6/8", "2/4", "9/8", "5/4", "7/4",
        };
        this.timeSignatureChoice = new SmartComboBox(timeSignatures);
        timeSignatureChoice.setToolTipText("Time Signature, or Bar Type");
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
        
        this.writeToNotesCheckbox = new JCheckBox("Write Notes", false);
        writeToNotesCheckbox.setToolTipText("Write Notes from Piano to Textarea");
        writeToNotesCheckbox.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                notesWritingListener.setActive(((JCheckBox) e.getSource()).isSelected());
            }
        });
        
        final JPanel notesControlPanel = new JPanel(); // else button is too big
        notesControlPanel.setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createLineBorder(Color.GRAY),
                BorderFactory.createEmptyBorder(6, 6, 6, 6)));
        notesControlPanel.setLayout(new BoxLayout(notesControlPanel, piano.config.isVertical ? BoxLayout.X_AXIS : BoxLayout.Y_AXIS));
        
        play.setAlignmentX(Component.CENTER_ALIGNMENT);
        notesControlPanel.add(play);
        notesControlPanel.add(Box.createRigidArea(new Dimension(1, 8))); // space to next button
        
        formatBars.setAlignmentX(Component.CENTER_ALIGNMENT);
        notesControlPanel.add(formatBars);
        notesControlPanel.add(Box.createRigidArea(new Dimension(1, 6))); // space to other control fields
        
        timeLayoutPanel.setAlignmentX(Component.CENTER_ALIGNMENT);
        notesControlPanel.add(timeLayoutPanel);
        
        tempoLayoutPanel.setAlignmentX(Component.CENTER_ALIGNMENT);
        notesControlPanel.add(tempoLayoutPanel);
        notesControlPanel.add(piano.config.isVertical ? Box.createHorizontalGlue() : Box.createVerticalGlue());
        
        writeToNotesCheckbox.setAlignmentX(Component.CENTER_ALIGNMENT);
        notesControlPanel.add(writeToNotesCheckbox);
        
        return notesControlPanel;
    }
    
    /** Listen to piano mouse clicks and write notes into text-area. */
    private void addPianoMouseListener() {
        for (PianoWithSound.Keyboard.Key key : piano.getKeys())
            key.addMouseListener(notesWritingListener);
    }
    
    
    
    // callback methods
    
    private void insertTextAtNearestSpace(String noteWithLength) {
        final int caretPosition = notesText.getCaretPosition();
        int insertPosition = caretPosition;
        
        // don't write into existing notes
        final int textLenght = notesText.getDocument().getLength();
        if (textLenght > 0) {
            int before = caretPosition, after = caretPosition;
            boolean leftSideBlank  = false;
            boolean rightSideBlank = false;
            do {
                final String textAroundCaret = getText(
                        before, // offset
                        after - before + 1); // length
                leftSideBlank  = Character.isWhitespace(textAroundCaret.charAt(0));
                rightSideBlank = Character.isWhitespace(textAroundCaret.charAt(textAroundCaret.length() - 1));
                before--;
                after++;
            }
            while (leftSideBlank == false && rightSideBlank == false && 
                    before >= 0 && after <= textLenght);
            
            before++;
            after--;
            
            if (leftSideBlank && rightSideBlank) { // space on both sides equally near, or directly on space
                insertPosition = after; // default to appending
                if (before != after || // when not directly on space
                        after > 0 && getText(after - 1, 1).equals("\n") == false) // or not at line start
                    noteWithLength = " "+noteWithLength; // prepend space
            }
            else if (leftSideBlank) {
                if (getText(before, 1).equals("\n") == true) { // would go to previous line
                    insertPosition = before + 1;
                    noteWithLength = noteWithLength+" ";
                }
                else {
                    insertPosition = before;
                    noteWithLength = " "+noteWithLength;
                }
            }
            else if (rightSideBlank) {
                insertPosition = after;
                noteWithLength = " "+noteWithLength;
            }
            else {
                insertPosition = before;
                noteWithLength = noteWithLength+" ";
            }
        }
        
        notesText.insert(noteWithLength, insertPosition);
    }

    private String getText(int offset, int length) { // just to catch Exception
        try {
            return notesText.getText(offset, length);
        }
        catch (BadLocationException e) {
            throw new RuntimeException(e);
        }
    }

    private void removeSelectedText() {
        final int selectionStart = notesText.getSelectionStart();
        final int selectionEnd = notesText.getSelectionEnd();
        if (selectionStart != selectionEnd) {
            try {
                notesText.getDocument().remove(selectionStart, selectionEnd - selectionStart);
            }
            catch (BadLocationException e) {
                throw new RuntimeException(e);
            }
        }
    }

    private void formatNotes() {
        final Note[] notes = readNotesFromTextAreaCatchExceptions(true);
        if (notes != null) {
            final String formatted = newMelodyFactory().toString(
                    notes, 
                    tempoSpinner.isEnabled() == false, // write tempo and bar only when it was written in text
                    timeSignatureChoice.isEnabled() == false);
            notesText.setText(formatted);
            notesText.requestFocus();
        }
    }

    
    // player methods
    
    private void playButtonPressed() {
        synchronized(playerLock) {
            final boolean isStop = (player != null);
            startOrStopPlayer(isStop);
        }
    }

    /** Make sure to always call this synchronized(playerLock)! */
    private void startOrStopPlayer(boolean isStop) {
        play.setText(isStop ? "Play" : "Stop");
        play.setBackground(isStop ? Color.GREEN : Color.decode("0xff9999")); // bright red
        
        enableUiOnPlaying(isStop);
    
        if (isStop) {
            if (player != null) {
                player.close();
                player = null;
            }
            readNotesFromTextAreaCatchExceptions(false); // enables time-signature and tempo choosers, do not clear errors
            notesText.requestFocus();
        }
        else { // is start
            checkAndPlayNotes();
            
            // thread is running, disable controls that may have been enabled by readNotesFromTextArea()
            timeSignatureChoice.setEnabled(false);
            tempoSpinner.setEnabled(false);
            formatBars.setEnabled(false);
        }
    }
    
    /** Called when starting or stopping the player thread. */
    protected void enableUiOnPlaying(boolean isStop) {
        notesText.setEnabled(isStop);
        writeToNotesCheckbox.setEnabled(isStop);
        
        // block mouse events for any listener
        for (PianoWithSound.Keyboard.Key key : piano.getKeys())
            key.setIgnoreMouse(isStop == false);
            // setEnabled() did not reject mouse-events, and prevented key-down rendering
    }
    
    /** This method is synchronized(playerLock) because called from startOrStopPlayer() only. */
    private void checkAndPlayNotes() {
        final Note[] notesArray = readNotesFromTextAreaCatchExceptions(true);
        
        final SoundChannel pianoKeyConnector = new PianoKeyConnector(piano);
        this.player = new Player(pianoKeyConnector); // is synchronized because called from startOrStop()
        
        // to allow "Stop" while playing, all playing happens in a background thread
        final Thread playerThread = new Thread(() -> playNotes(notesArray));
        playerThread.start();
    }
    
    /** This is called on any text input. */
    private Note[] readNotesFromTextAreaCatchExceptions(boolean clearErrors) {
        try {
            final Note[] notes = readNotesFromTextArea();
            if (clearErrors)
                getErrorArea().setText(""); // no exception was thrown, so clear errors
            return notes;
        }
        catch (Exception e) {
            getErrorArea().setText(e.getMessage());
            play.setEnabled(false);
            formatBars.setEnabled(false);
            
            if (e instanceof IllegalArgumentException == false)
                e.printStackTrace();
        }
        return null;
    }
    
    private Note[] readNotesFromTextArea() throws IllegalArgumentException { // exceptions coming from MelodyFactory
        final String notesString = notesText.getText();
        final boolean enable;
        final Note[] notes;
        
        if (notesString.trim().isEmpty() == false) {
            final MelodyFactory melodyFactory = newMelodyFactory();
            notes = melodyFactory.translate(notesString); // throws exceptions!
            checkNotesRange(notes); // throws exceptions!
            
            enable = true; // no exception was thrown until here, so enable actions
            
            // optional BPM and time-signature were extracted from top
            final String timeSignatureOnTop = melodyFactory.getTimeSignatureOnTop();
            if (timeSignatureOnTop != null) {
                timeSignatureChoice.setSelectedItem(timeSignatureOnTop);
                timeSignatureChoice.setEnabled(false); // will be managed in text-area now
            }
            else {
                timeSignatureChoice.setEnabled(true);
            }
            
            final Integer tempoOnTop = melodyFactory.getTempoOnTop();
            if (tempoOnTop != null) {
                tempoSpinner.setValue(tempoOnTop);
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
        
        play.setEnabled(enable);
        formatBars.setEnabled(enable);
        
        return notes;
    }
    
    /** Get a new MelodyFactory with current parameters from UI. */
    private MelodyFactory newMelodyFactory() {
        final Integer[] timeSignature = timeSignatureParts((String) timeSignatureChoice.getSelectedItem());
        return new MelodyFactory(
                null,
                (Integer) tempoSpinner.getValue(),
                timeSignature[0],
                timeSignature[1]);
    }

    private void checkNotesRange(Note[] notesArray) {
        if (notesArray.length <= 0)
            throw new IllegalArgumentException("No notes were given!");
        
        final List<PianoWithSound.Keyboard.Key> keys = piano.getKeys();
        final int lowestMidiNumber  = keys.get(0).midiNoteNumber;
        final int highestMidiNumber = keys.get(keys.size() - 1).midiNoteNumber;
        for (Note note : notesArray)
            if (note.isRest() == false)
                if (lowestMidiNumber > note.midiNumber || highestMidiNumber < note.midiNumber)
                    throw new IllegalArgumentException("Note is not in range: "+note.ipnName);
    }

    private Integer[] timeSignatureParts(String timeSignature) {
        final int separatorIndex = timeSignature.indexOf(Note.DURATION_SEPARATOR);
        final String beatsPerBar = timeSignature.substring(0, separatorIndex);
        final String beatType = timeSignature.substring(separatorIndex + 1);
        return new Integer[] {
                Integer.valueOf(beatsPerBar),
                Integer.valueOf(beatType) };
    }
    
    private void playNotes(Note[] notesArray) {
        playNotes(convertNotesToChords(notesArray));
    }
    
    /**
     * Called when playing notes on piano.
     * Converts given 1-dimensional notes from text-area to 2-dimensional chords-array.
     * To be overridden for other conversions than just one note per chord.
     * @param notesArray the notes from text-area to convert to a 2-dimensional chords array.
     */
    protected Note[][] convertNotesToChords(Note[] notesArray) {
        final Note[][] chordsArray = new Note[notesArray.length][];
        for (int i = 0; i < notesArray.length; i++) {
            chordsArray[i] = new Note[1];
            chordsArray[i][0] = notesArray[i];
        }
        return chordsArray;
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
        RuntimeException exception = null;
        
        for (int i = 0; interrupted == false && exception == null && i < chordsArray.length; i++) { // play all notes one by one
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
    

    /** Help taken from JavaDoc of MelodyFactory class. */
    private static final String SYNTAX_HELP = """
<html>
<head></head>
<body>
<h2>Write by Piano</h2>
<p>
When you turn on the "Write Notes" checkbox, 
you can use the piano to write notes.
Left click on any key writes to the text-area at cursor position 
with a note length that is calculated
from the time the mouse button was down.
Right mouse click opens a context-menu that lets choose
the length of the clicked note.
</p>
<h2>Notes Syntax</h2>
<p>
Every note is given as an IPN-name (international pitch notation)
and its length behind a slash, for example: 
</p>
<ul>
<li>"A4/8" for a eighth note on A4 (4th octave) with pitch 440 Hz</li>
<li>"C#4/2." for a dotted C#4 half note (spans three quarter notes)</li>
<li>"E5/16,3" for a E5 triplet sixteenth note 
    (each of the triplets must have the ",3" postfix!)</li>
<li>"(G5/1 (G5/1) G5/1)" for a G5 whole note that spans three bars</li>
<li>"-/4" for a quarter rest note.</li>
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
You can use the "Format" button to put every bar into a separate line.
</p><p>
Notes connected by a "tie" are notes of same pitch that are played as single note, 
even across several bars.
Ties are started by an parenthesis "(" and ended by ")",
notes in between the start and end note SHOULD be enclosed in
parentheses, because this is how it would look like in written notes.
Space between parentheses and note is allowed.
You can not tie rests.
</p><p>
Notes connected by a "slur" are notes of different pitch that are phrased together, 
even across several bars.
Slurs are started by a brace "{" and ended by "}",
notes in between MUST NOT be enclosed in "{...}",
because it is not clear how to phrase several notes that are all slurred together.
Space between braces and note is allowed.
You can not slur rests.
</p>
<h2>Editor Actions</h2>
<p>
With right mouse click you can open a context-menu that provides
visible representations for following actions on text-area:
</p>
<ul>
<li><b>Ctrl-x</b> for "Cut" selection</li>
<li><b>Ctrl-c</b> for "Copy" selection</li>
<li><b>Ctrl-v</b> for "Paste" at caret position</li>
<li><b>Ctrl-z</b> for "Undo" last action</li>
<li><b>Ctrl-y</b> for "Redo" last "Undo"</li>
</ul>
</body>
</html>
""";
}