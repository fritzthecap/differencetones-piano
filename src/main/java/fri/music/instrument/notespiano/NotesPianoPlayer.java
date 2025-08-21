package fri.music.instrument.notespiano;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowListener;
import java.util.Objects;
import javax.swing.JComponent;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.BadLocationException;
import fri.music.instrument.PianoWithSound;
import fri.music.instrument.wave.PianoKeyConnector;
import fri.music.player.Note;
import fri.music.player.Player;
import fri.music.player.notelanguage.MelodyFactory;

/**
 * A notes area that can play user-editable notes on a given piano.
 * It uses a constructor-given piano to play the entered notes, thus
 * it is called "Player" and not "Piano", although it displays a piano.
 * <p/>
 * It installs an additional mouse-listener to the keyboard that writes
 * clicked notes from keyboard to the "Notes" text-area.
 */
public class NotesPianoPlayer
{
    /** The piano obtained from constructor. */
    protected final PianoWithSound piano;

    private PlayController playController;
    
    private JComponent playerPanel; // the object-singleton component
    
    private NotesTextPanel view;
    private NotesWritingMouseListener notesWritingPianoListener;
    
    /** @param piano required, the piano on which to play notes. */
    public NotesPianoPlayer(PianoWithSound piano) {
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
        
        this.playController = newPlayController();
        this.notesWritingPianoListener = newNotesWritingMouseListener();

        final JComponent playerPanel = piano.getKeyboard(); // build up the piano
        
        piano.getPanelWithFreeCenter().add( // now we can add UI
                buildCenterPanel(), // allocates view
                BorderLayout.CENTER); // to CENTER, so that user can resize text-area
        
        if (melody != null && melody.length() > 0) { // put initial melody into text-area
            view().notesText.setText(melody); // triggers check via DocumentListener
            view().notesText.setCaretPosition(melody.length());
        }
        else {
            view().writeToNotesCheckbox.setSelected(true); // enable immediate notes writing
            notesWritingPianoListener.setActive(true);
        }
        
        readNotesFromTextAreaCatchExceptions(playController, view()); // enable or disable player buttons
        
        // listen to piano mouse clicks and write notes into text-area
        for (PianoWithSound.Keyboard.Key key : piano.getKeys()) {
            key.addMouseListener(notesWritingPianoListener);
            key.addKeyListener(notesWritingPianoListener);
        }
        
        return this.playerPanel = playerPanel;
    }
    
    /** @return the close-listener of created piano, required when using WaveGenerator! */
    public WindowListener getWindowClosingListener() {
        return piano.getWindowClosingListener();
    }
    
    
    /** Factory method for customized PlayController classes. */
    protected PlayController newPlayController() {
        return new PlayController(this);
    }
    
    /** @return the PlayController created by getPlayer(). */
    protected final PlayController getPlayController() {
        return playController;
    }

    /** Factory method for customized PlayController classes. */
    protected NotesWritingMouseListener newNotesWritingMouseListener() {
        return new NotesWritingMouseListener(this);
    }

    /** @return a new MelodyFactory with current parameters from UI. */
    protected final MelodyFactory newMelodyFactory() {
        final Integer[] timeSignature = timeSignatureParts();
        return new MelodyFactory(
                null,
                (Integer) view().tempoSpinner.getValue(),
                timeSignature[0],
                timeSignature[1]);
    }
    
    // override-able UI builder methods
    
    /** @return the notes panel in CENTER, to be overridden for other components. */
    protected JComponent buildCenterPanel() {
        this.view = new NotesTextPanel(playController, piano.config.isVertical);
        playController.setView(view);
        
        buildNotesPanel(playController, view);
        
        view.tempoSpinner.addChangeListener(new ChangeListener() {
            /** Tempo change should reload PlayController (like editing notes text does). */
            @Override
            public void stateChanged(ChangeEvent e) {
                readNotesFromTextAreaCatchExceptions(playController, view);
            }
        });
        
        view.writeToNotesCheckbox.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                notesWritingPianoListener.setActive(view.writeToNotesCheckbox.isSelected());
            }
        });
        
        return this.view;
    }
    
    // methods needed in sub-classes
    
    /** @return the view from getPlayer() call. */
    protected final NotesTextPanel view() {
        return view;
    }
    
    /** Adds notesText and formatBars listeners to given view. */
    protected final NotesTextPanelBase buildNotesPanel(final PlayControllerBase playController, final NotesTextPanelBase view) {
        view.notesText.getDocument().addDocumentListener(new DocumentListener() {
            @Override
            public void removeUpdate(DocumentEvent e) {
                readNotesFromTextAreaCatchExceptions(playController, view);
            }
            @Override
            public void insertUpdate(DocumentEvent e) {
                readNotesFromTextAreaCatchExceptions(playController, view);
            }
            @Override
            public void changedUpdate(DocumentEvent e) {
                readNotesFromTextAreaCatchExceptions(playController, view);
            }
        });
        
        view.formatBars.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                formatNotes(playController, view);
            }
        });
        
        return view;
    }
    
    // override-able methods called by PlayController and maybe needed in sub-classes
    
    /**
     * Called before playing notes on piano. Simply returns given array.
     * To be overridden for conversions.
     * @param notesArray the notes from text-area.
     * @return the notes to be played on piano.
     */
    protected Note[][] convertNotes(Note[][] notesArray) {
        return notesArray;
    }
    
    /** Called when starting or stopping the player thread. */
    protected void enableUiOnPlaying(boolean isStop) {
        // block mouse events for any listener
        for (PianoWithSound.Keyboard.Key key : piano.getKeys())
            key.setIgnoreMouse(isStop == false);
            // setEnabled(false) does not reject mouse-events, and it prevents key-down rendering
    }
    
    /**
     * Enable time-signature and tempo-chooser, optionally clear errors.
     * This is called on any text input, and also when starting or stopping melody.
     * @return null when error, else the notes-array from text area.
     */
    protected final Note[][] readNotesFromTextAreaCatchExceptions(PlayControllerBase playController, NotesTextPanelBase view) {
        if (view.isPermanentNotesCheck() == true) {
            try {
                final Note[][] notes = playController.readNotesFromTextArea(true);
                view.error.setText(""); // no exception was thrown, so clear errors
                return notes;
            }
            catch (Exception e) {
                view.error.setText(e.getMessage());
                view.playButtons.setEnabled(false);
                view.formatBars.setEnabled(false);
                
                if (e instanceof IllegalArgumentException == false)
                    e.printStackTrace();
            }
        }
        return null;
    }
    
    
    /** Method called by NotesWritingMouseListener. */
    String noteLengthForMillis(int durationMillis) {
        Integer beatsPerMinute = (Integer) view().tempoSpinner.getValue();
        final int beatDurationMillis = Note.beatDurationMillis(beatsPerMinute);
        
        final Integer[] timeSignature = timeSignatureParts();
        final Integer beatType = timeSignature[1];
        
        return MelodyFactory.noteLengthDivisor(durationMillis, beatType, beatDurationMillis);
    }
    
    /** Method called by NotesWritingMouseListener. */
    public void writeSingleNote(NotesTextPanelBase view, String noteWithLength) {
        removeSelectedText(view); // overwrite selected text if any
        insertTextAtNearestSpace(view, noteWithLength); // intelligent insertion
    }
    
    /** Method called by NotesWritingMouseListener. */
    void playSingleNote(String noteWithLength) {
        final Note[][] note = newMelodyFactory().translate(new String[] { noteWithLength });
        new Player(new PianoKeyConnector(piano)).playSimultaneously(note[0]);
    }
    
    // callback and helper methods
    
    private void formatNotes(PlayControllerBase playController, NotesTextPanelBase view) {
        final Note[][] notes = readNotesFromTextAreaCatchExceptions(playController, view);
        if (notes != null) {
            final String formatted = newMelodyFactory().formatBarLines(
                    notes, 
                    view().tempoSpinner.isEnabled() == false, // write tempo and bar only when it was written in text
                    view().timeSignatureChoice.isEnabled() == false);
            view.notesText.setText(formatted);
            view.notesText.requestFocus();
        }
    }
    
    private Integer[] timeSignatureParts() {
        final String timeSignature = (String) view().timeSignatureChoice.getSelectedItem();
        final int separatorIndex = timeSignature.indexOf(Note.DURATION_SEPARATOR);
        final String beatsPerBar = timeSignature.substring(0, separatorIndex);
        final String beatType = timeSignature.substring(separatorIndex + 1);
        return new Integer[] {
                Integer.valueOf(beatsPerBar),
                Integer.valueOf(beatType) };
    }
    
    private void insertTextAtNearestSpace(NotesTextPanelBase view, String noteWithLength) {
        final int caretPosition = view.notesText.getCaretPosition();
        int insertPosition = caretPosition;
        
        // don't write into existing notes
        final int textLenght = view.notesText.getDocument().getLength();
        if (textLenght > 0) {
            int before = caretPosition, after = caretPosition;
            boolean leftSideBlank  = false;
            boolean rightSideBlank = false;
            do {
                final String textAroundCaret = getText(
                        view,
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
                        after > 0 && getText(view, after - 1, 1).equals("\n") == false) // or not at line start
                    noteWithLength = " "+noteWithLength; // prepend space
            }
            else if (leftSideBlank) {
                if (getText(view, before, 1).equals("\n") == true) { // would go to previous line
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
        
        view.notesText.insert(noteWithLength, insertPosition);
    }

    private String getText(NotesTextPanelBase view, int offset, int length) { // just to catch Exception
        try {
            return view.notesText.getText(offset, length);
        }
        catch (BadLocationException e) {
            throw new RuntimeException(e);
        }
    }

    private void removeSelectedText(NotesTextPanelBase view) {
        final int selectionStart = view.notesText.getSelectionStart();
        final int selectionEnd = view.notesText.getSelectionEnd();
        if (selectionStart != selectionEnd) {
            try {
                view.notesText.getDocument().remove(selectionStart, selectionEnd - selectionStart);
            }
            catch (BadLocationException e) {
                throw new RuntimeException(e);
            }
        }
    }
}