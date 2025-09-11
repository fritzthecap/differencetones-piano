package fri.music.instrument.notespiano;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowListener;
import java.util.Objects;
import javax.swing.Box;
import javax.swing.JComponent;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.BadLocationException;
import javax.swing.text.JTextComponent;
import fri.music.EqualTemperament;
import fri.music.SoundChannel;
import fri.music.Tone;
import fri.music.ToneSystem;
import fri.music.Tones;
import fri.music.instrument.PianoWithSound;
import fri.music.instrument.notespiano.abc.AbcExportComponent;
import fri.music.instrument.wave.PianoKeyConnector;
import fri.music.player.Note;
import fri.music.player.Player;
import fri.music.player.notelanguage.MelodyFactory;
import fri.music.swingutils.DialogUtil;
import fri.music.wavegenerator.WaveSoundChannel;

/**
 * A notes area that can play user-edited notes on a given piano.
 * It uses a constructor-given piano to play the entered notes, thus
 * it is called "Player" and not "Piano", although it displays a piano.
 * <p/>
 * It installs an additional mouse-listener to the keyboard that writes
 * clicked notes from keyboard to the "Notes" text-area.
 */
public class NotesPianoPlayer implements NotesTextPanel.TransposeListener
{
    /** The piano obtained from constructor. */
    protected final PianoWithSound piano;

    private PlayController playController;
    
    private JComponent playerPanel; // the object-singleton component
    
    private NotesTextPanel view;
    private NotesWritingMouseListener notesWritingPianoListener;
    
    private boolean permanentNotesCheck = true;
    
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
        
        melodyView().textAreaActions.contextMenu.addSeparator();
        melodyView().textAreaActions.contextMenu.add(buildLoadMenuItems());

        if (melody != null && melody.length() > 0) { // put initial melody into text-area
            melodyView().notesText.setText(melody); // triggers check via DocumentListener
            melodyView().notesText.setCaretPosition(melody.length()); // ready to append notes
        }
        else {
            if (melodyView().writeToNotesCheckbox.isSelected() == false) // enable immediate notes writing
                melodyView().writeToNotesCheckbox.doClick(0); // triggers actionPerformed() to activate mouse listener
        }
        
        readNotesFromTextAreaCatchExceptions(playController, melodyView()); // enable or disable player buttons
        
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
    
    /** Method called by NotesWritingMouseListener. */
    public void writeSingleNote(NotesTextPanelBase view, String noteWithLength) {
        removeSelectedText(view); // overwrite selected text if any
        insertTextAtNearestSpace(view, noteWithLength); // intelligent insertion
    }
    
    /**
     * Transposes given textArea by implementing TransposeListener.
     * Writes any exception into error field of left-side view.
     * @param intervalName one of ToneSystem.INTERVAL_NAMES.
     * @param upwards true for transposing higher, false for lower.
     * @param textArea the holder of the text to transpose.
     */
    @Override
    public boolean transpose(String intervalName, boolean upwards, JTextComponent textArea) {
        try {
            transposeThrowing(intervalName, upwards, textArea);
            return true;
        }
        catch (Exception e) {
            melodyView().error.setText(e.getMessage());
            return false;
        }
    }
    
    /**
     * Called from transpose(), same parameters, but does not do any error management.
     * @throws IllegalArgumentException when transpose gets out of range of available notes.
     */
    protected final void transposeThrowing(String intervalName, boolean upwards, JTextComponent textArea) {
        final String notesText = textArea.getText();
        final MelodyFactory melodyFactory = newMelodyFactory();
        final Note[][] notes = melodyFactory.translate(notesText);
        
        final SoundChannel soundChannel = piano.getSoundChannel();
        final Tone[] tonesArray = (soundChannel instanceof WaveSoundChannel)
                ? ((WaveSoundChannel) soundChannel).getTones()
                : new EqualTemperament().tones();
        final Tones tones = new Tones(tonesArray);

        final Note[][] transposedNotes = new Note[notes.length][];
        
        for (int i = 0; i < notes.length; i++) {
            Note[] chord = notes[i];
            final Note[] transposedChord = new Note[chord.length];
            
            for (int j = 0; j < chord.length; j++) {
                final Note note = chord[j];
                final Note transposedNote;
                
                if (note.isRest() == false) {
                    final int semitoneSteps = ToneSystem.semitoneSteps(intervalName);
                    final int newMidiNumber = note.midiNumber + (upwards ? +semitoneSteps : -semitoneSteps);
                    final Tone newTone = tones.forMidiNoteNumber(newMidiNumber);
                    if (newTone == null)
                        throw new IllegalArgumentException("Transposed note would be out of range: "+note);
                    
                    transposedNote = new Note(note, newTone.ipnName, newTone.frequency, newTone.midiNumber, newTone.cent);
                }
                else {
                    transposedNote = note;
                }
                transposedChord[j] = transposedNote;
            }
            transposedNotes[i] = transposedChord;
        }
        
        final String newText = formatNotes(transposedNotes, melodyFactory);
        textArea.setText(newText);
    }
    
    
    /** Factory method for customized PlayController classes. */
    protected PlayController newPlayController() {
        return new PlayController(this);
    }
    
    /** @return the PlayController created by <code>getPlayer()</code>. */
    protected final PlayController getPlayController() {
        return playController;
    }

    /** Factory method for customized PlayController classes. */
    protected NotesWritingMouseListener newNotesWritingMouseListener() {
        return new NotesWritingMouseListener(this);
    }

    /** @return a new MelodyFactory with current parameters from UI. */
    protected MelodyFactory newMelodyFactory() {
        final Integer[] timeSignature = timeSignatureParts();
        return new MelodyFactory(
                getToneSystem(),
                (Integer) melodyView().tempoSpinner.getValue(),
                timeSignature[0],
                timeSignature[1]);
    }
    
    /** Returns null. To be overridden by classes that can choose tunings. */
    protected ToneSystem getToneSystem() {
        return null;
    }
    
    /** Factory method for AbcExportComponent, to be overridden. */
    protected AbcExportComponent newAbcExportComponent(NotesTextPanelBase view) {
        return new AbcExportComponent(view.notesText.getText(), newMelodyFactory(), false);
    }
    
    /** @return whether permanentNotesCheck is ON. */
    protected final boolean isPermanentNotesCheck() {
        return permanentNotesCheck;
    }
    /** When writing notes programmatically (e.g. auto-compose) it is useful to temporarily disable the syntax check. */
    protected final void setPermanentNotesCheck(boolean active) {
        this.permanentNotesCheck = active;
    }
    
    // override-able UI builder methods
    
    /** @return the notes panel in CENTER, to be overridden for other components. */
    protected JComponent buildCenterPanel() {
        this.view = new NotesTextPanel(playController, piano.config.isVertical, this);
        playController.setView(view);
        
        buildNotesPanel(playController, view);
        
        view.tempoSpinner.addChangeListener(new ChangeListener() {
            /** Tempo change should reload PlayController (like editing notes text does). */
            @Override
            public void stateChanged(ChangeEvent e) {
                readNotesFromTextAreaCatchExceptions(playController, view);
            }
        });
        
        view.timeSignatureChoice.addActionListener(new ActionListener() {
            /** Time-signature change should uncover bar excess errors. */
            @Override
            public void actionPerformed(ActionEvent e) {
                readNotesFromTextAreaCatchExceptions(playController, view);
            }
        });
        
        view.writeToNotesCheckbox.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                notesWritingPianoListener.setActive(view.writeToNotesCheckbox.isSelected());
            }
        });
        
        addRestButton(view);
        
        addExportActionListener(view);
        
        return this.view;
    }
    
    // methods needed in sub-classes
    
    /** @return the view from <code>getPlayer()</code> call. */
    protected final NotesTextPanel melodyView() {
        return view;
    }
    
    /** Adds the "Rest" button to textAreaToolbar. To be overridden. */
    protected void addRestButton(final NotesTextPanelBase view) {
        view.textareaToolbar.add(buildRestButton(), 0);
        view.textareaToolbar.add(Box.createRigidArea(new Dimension(16, 1)), 1);
    }
    
    /** @return a new "Rest" button that writes to notes text-area. */
    protected JComponent buildRestButton() {
        return new RestButton(new RestButton.Callback() {
            @Override
            public void writeRest(String note) {
                writeSingleNote(melodyView(), note);
            }
        });
    }

    /** Installs an ActionListener to "ABC Export" button. To be overridden. */
    protected final void addExportActionListener(final NotesTextPanelBase view) {
        view.abcExport.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                DialogUtil.showModelessDialog(
                        "Export to ABC",
                        view.notesText, // parent to show above
                        newAbcExportComponent(view), // fetches the view-owned text, with left-side tempo and time
                        new Dimension(720, 530),
                        null);
            }
        });
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
    protected void enableUiOnPlaying(boolean isStop, Note[][] sounds, int currentSoundIndex, NotesTextPanelBase view) {
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
        if (isPermanentNotesCheck() == true) {
            try {
                final Note[][] notes = playController.readNotesFromTextArea(true);
                enableUiOnReadNotes(null, notes, view);
                return notes;
            }
            catch (Exception e) {
                enableUiOnReadNotes(e, null, view);
            }
        }
        return null;
    }
    
    /** Called when notes text, tempo or time signature changes. */
    protected void enableUiOnReadNotes(Exception e, Note[][] notes, NotesTextPanelBase view) {
        if (e == null) {
            view.error.setText(""); // no exception was thrown, so clear errors
        }
        else {
            view.error.setText(e.getMessage());
            view.error.setCaretPosition(0); // scroll back to start of possibly long message
            view.playButtons.setEnabled(false);
            view.formatBars.setEnabled(false);
            view.abcExport.setEnabled(false);
            
            if (e instanceof IllegalArgumentException == false)
                e.printStackTrace();
        }
    }
    
    /** Method called by NotesWritingMouseListener. */
    String noteLengthForMillis(int durationMillis) {
        Integer beatsPerMinute = (Integer) melodyView().tempoSpinner.getValue();
        final int beatDurationMillis = Note.beatDurationMillis(beatsPerMinute);
        
        final Integer[] timeSignature = timeSignatureParts();
        final Integer beatType = timeSignature[1];
        
        return MelodyFactory.noteLengthDivisor(durationMillis, beatType, beatDurationMillis);
    }
    
    /** Method called by NotesWritingMouseListener. */
    void playSingleNote(String noteWithLength) {
        final Note[][] note = newMelodyFactory().translate(new String[] { noteWithLength });
        new Player(new PianoKeyConnector(piano)).playSimultaneously(note[0]);
    }
    
    // UI builder methods
    
    private JMenu buildLoadMenuItems() {
        final JMenu menu = new JMenu("Load Examples");
        boolean songSeparator = false;
        for (final NoteExamples.Melody melody : NoteExamples.MELODIES) {
            if (melody.isSong() && songSeparator == false) {
                songSeparator = true;
                menu.addSeparator();
            }
            final JMenuItem item = new JMenuItem(melody.title());
            item.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    melodyView().notesText.setText(melody.notes());
                }
            });
            menu.add(item);
        }
        return menu;
    }
    
    // callback and helper methods
    
    private void formatNotes(PlayControllerBase playController, NotesTextPanelBase view) {
        final Note[][] notes = readNotesFromTextAreaCatchExceptions(playController, view);
        if (notes != null) {
            final String formatted = formatNotes(notes, newMelodyFactory());
            view.notesText.setText(formatted);
            view.notesText.requestFocus();
        }
    }
    
    private String formatNotes(Note[][] notes, MelodyFactory melodyFactory) {
        return melodyFactory.formatBarLines(
                notes, 
                // write tempo and bar only when it was written in text, or when forceWriteTempoAndBar
                melodyView().tempoSpinner.isEnabled() == false,
                melodyView().timeSignatureChoice.isEnabled() == false);
    }
    
    private Integer[] timeSignatureParts() {
        final String timeSignature = (String) melodyView().timeSignatureChoice.getSelectedItem();
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