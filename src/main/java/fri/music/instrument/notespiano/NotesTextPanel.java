package fri.music.instrument.notespiano;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JSpinner;
import javax.swing.SpinnerModel;
import javax.swing.SpinnerNumberModel;
import javax.swing.text.JTextComponent;
import fri.music.ToneSystem;
import fri.music.player.Note;
import fri.music.swingutils.DialogUtil;
import fri.music.swingutils.SmartComboBox;
import fri.music.swingutils.SmartPanel;

/** Full view of NotesPianoPlayer with time-signature and tempo controls. */
public class NotesTextPanel extends NotesTextPanelBase
{
    /** Transposers listen via this interface for user commands. */
    public interface TransposeListener
    {
        /** User triggered the transpose-command described by parameters. */
        void transpose(String intervalName, boolean upwards, JTextComponent textArea);
    }
    
    public final JSpinner tempoSpinner;
    public final JComboBox<String> timeSignatureChoice;
    public final JCheckBox writeToNotesCheckbox;
    public final JButton transposeMenu;
    
    private final TransposeListener transposeListener;
    
    public NotesTextPanel(PlayController playController, boolean pianoIsVertical, TransposeListener transposeListener) {
        super(playController, pianoIsVertical, true);
        
        this.transposeListener = transposeListener;
        
        // START build public fields
        final String[] timeSignatures = new String[] {
                "4/4", "3/4", "12/8", "6/8", "2/4", "9/8", "5/4", "7/4",
            };
        this.timeSignatureChoice = new SmartComboBox(timeSignatures);
        
        final SpinnerModel tempoModel = new SpinnerNumberModel(
                Note.DEFAULT_TEMPO_BPM, // initial value
                Note.TEMPO_MINIMUM_BPM, // minimum
                Note.TEMPO_MAXIMUM_BPM, // maximum
                4); // step width
        this.tempoSpinner = new JSpinner(tempoModel);
        
        this.writeToNotesCheckbox = new JCheckBox("Write Notes", false);
        
        if (transposeListener != null)
            this.transposeMenu = buildTransposeMenu();
        else
            this.transposeMenu = null;

        // END build public fields
        
        buildNotesControlPanel(pianoIsVertical);
        
        final JButton help = new JButton("Help");
        help.setToolTipText("Notes Syntax Documentation");
        help.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                DialogUtil.showModelessHtmlDialog(
                        "Notes Edit Help", 
                        notesText, // parent
                        NOTES_EDIT_HELP, 
                        null);
            }
        });
        textAreaButtonsPanel.add(help, 0); // "Help" button on the very left
    }
    
    private void buildNotesControlPanel(boolean pianoIsVertical) {
        tempoSpinner.setToolTipText("Beats Per Minute, a Beat being the Divisor of Bar");
        final JPanel tempoLayoutPanel = new SmartPanel(new BorderLayout()); // without this panel, field would be sized vertically
        tempoLayoutPanel.add(tempoSpinner, pianoIsVertical ? BorderLayout.WEST : BorderLayout.NORTH);
        tempoLayoutPanel.setBorder(BorderFactory.createTitledBorder("Tempo"));
        
        timeSignatureChoice.setToolTipText("Time Signature, or Bar Type");
        timeSignatureChoice.setEditable(true);
        timeSignatureChoice.setPreferredSize(new Dimension(60, 24)); // else much too wide when editable
        final JPanel timeLayoutPanel = new SmartPanel(new BorderLayout());
        timeLayoutPanel.add(timeSignatureChoice, pianoIsVertical ? BorderLayout.WEST : BorderLayout.NORTH);
        timeLayoutPanel.setBorder(BorderFactory.createTitledBorder("Bar"));
        
        writeToNotesCheckbox.setToolTipText("Write Notes from Piano to Textarea at Cursor Position");
        
        tempoLayoutPanel.setAlignmentX(JComponent.CENTER_ALIGNMENT);
        notesControlPanel.add(tempoLayoutPanel);
        
        timeLayoutPanel.setAlignmentX(JComponent.CENTER_ALIGNMENT);
        notesControlPanel.add(timeLayoutPanel);
        
        if (transposeMenu != null) {
            transposeMenu.setAlignmentX(JComponent.CENTER_ALIGNMENT);
            notesControlPanel.add(Box.createRigidArea(new Dimension(1, 4))); // space to other control fields
            notesControlPanel.add(transposeMenu);
        }
        
        notesControlPanel.add(pianoIsVertical ? Box.createHorizontalGlue() : Box.createVerticalGlue());
        
        writeToNotesCheckbox.setAlignmentX(JComponent.CENTER_ALIGNMENT);
        notesControlPanel.add(writeToNotesCheckbox);
    }
    
    private JButton buildTransposeMenu() {
        final JPopupMenu popupMenu = new JPopupMenu();
        
        final JMenu upMenu = new JMenu("Up");
        popupMenu.add(upMenu);
        final JMenu downMenu = new JMenu("Down");
        popupMenu.add(downMenu);
        
        for (int i = 1; i < ToneSystem.INTERVAL_NAMES.length; i++) { // 1: spare UNISON
            upMenu.add(buildTransposeMenuItem(ToneSystem.INTERVAL_NAMES[i], true));
            downMenu.add(buildTransposeMenuItem(ToneSystem.INTERVAL_NAMES[i], false));
        }
        
        final JButton popupButton = new JButton("Transpose  \u25B8");
        popupButton.setToolTipText("Shift All Notes to Another Pitch");
        popupButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                popupMenu.show(popupButton, popupButton.getWidth() / 2, popupButton.getHeight() / 2);
            }
        });
        
        return popupButton;
    }

    private JMenuItem buildTransposeMenuItem(final String intervalName, final boolean upwards) {
        final JMenuItem item = new JMenuItem(intervalName);
        item.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                transposeListener.transpose(intervalName, upwards, notesText);
            }
        });
        return item;
    }
    
    
    /** Help taken from JavaDoc of MelodyFactory class. */
    private static final String NOTES_EDIT_HELP = """
<html>
<head></head>
<body>
<h2>Write by Piano</h2>
<p>
When you turn on the "Write Notes" checkbox, 
you can use the piano to write notes.
Left click on any key writes to the text-area at cursor position 
with a note duration that is calculated
from the time the mouse button was down.
Right mouse click opens a context-menu that lets choose
the duration of the clicked note.
</p>
<h2>Notes Syntax</h2>
<p>
Every note is given as an IPN-name (international pitch notation)
and its duration behind a slash, for example: 
</p>
<ul>
<li>"A4/8" for a eighth note on A4 (4th octave) with pitch 440 Hz</li>
<li>"C#4/2." for a dotted C#4 half note (spans three quarter notes)</li>
<li>"-/4" for a quarter rest note</li>
<li>"E5/16~3" for a E5 triplet sixteenth note 
    (each of the triplets must have the "~3" tilde-postfix!);
    a quarter note triplet MUST start with a quarter note 
    (that may be tied to a subsequent one),
    but it MUST NOT start with an eighth note or a half note, 
    else it will be regarded as eighth or half triplet</li>
<li>"[C4/4 E4 G4]" for a C-major chord, only the first note needs to have a length</li>
<li>"(G5/1 G5/1 G5/1)" for a G5 whole note that spans three 4/4 bars</li>
<li>"{A5/4 B5/4 C5/4}" for slurred notes</li>
</ul>
<p>
No space must appear between a note and its duration specification,
but at least one whitespace MUST be between different notes.
Spaces are allowed between notes and tie-, slur- and chord-symbols.
</p><p>
In IPN there is no "Eb" or "Bb", you must give "D#" or "A#",
and there is no German "H", such is written as "B".
But you can use both lower or upper case letters in IPN-names.
</p><p>
The <b>tempo</b> can appear as simple BPM-number (beats per minute)
on top of the notes only, it can not change in-between.
It always refers to a quarter note.
</p><p>
The <b>time signature</b> can appear on top of the notes, or everywhere in-between,
written as "4/4" or "3/4" or similar.
</p><p>
Do not care about bars, the player will automatically calculate bar bounds
using the given time signature(s).
You can use the "Format" button to put every bar into a separate line.
</p>
<h3>Ties</h3>
<p>
Notes connected by a "tie" are notes of same pitch that are played as single note, 
even across several bars.
Ties are started by an opened parenthesis "(" and ended by a closed ")",
notes in between MUST NOT be enclosed in parentheses (no nested parentheses).
</p>
<h3>Slurs</h3>
<p>
Notes connected by a "slur" are notes of different pitch that are phrased together, 
even across several bars.
Slurs are started by an opened brace "{" and ended by a closed "}",
notes in between MUST NOT be enclosed in "{...}" (no nested braces),
because it is not clear how to phrase several notes that are all slurred together.
</p>
<h3>Chords</h3>
<p>
Chord notes are two or more notes of different pitch that are played simultaneously.
Chords are started by an opened bracket "[" and ended by closed "]", 
just the first note needs to have a duration, it is assumed that all have the same length.
Ties and slurs from inside to outside of a chord are not allowed, but you can tie chords,
even across bars.
</p>

</p>
<h2>Editor Actions</h2>
<p>
With right mouse click you can open a context-menu that provides
some useful text-area actions:
</p>
<ul>
<li><b>Ctrl-x</b> for "Cut" selection</li>
<li><b>Ctrl-c</b> for "Copy" selection</li>
<li><b>Ctrl-v</b> for "Paste" at caret position</li>
<li><b>Ctrl-z</b> for "Undo" last action</li>
<li><b>Ctrl-y</b> for "Redo" last "Undo"</li>
<li><b>Ctrl-a</b> for "Select All" text</li>
</ul>
</body>
</html>
""";
}