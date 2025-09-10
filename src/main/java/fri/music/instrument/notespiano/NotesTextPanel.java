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
        /** 
         * User triggered the transpose-command described by parameters.
         * @param intervalName the distance to transpose.
         * @param upwards the direction to transpose.
         * @param textArea the target area for transposed notes.
         * @return true when transpose succeeded.
         */
        boolean transpose(String intervalName, boolean upwards, JTextComponent textArea);
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
                        HelpForNotes.HTML, 
                        null);
            }
        });
        textareaToolbar.add(help);
    }
    
    private void buildNotesControlPanel(boolean pianoIsVertical) {
        tempoSpinner.setToolTipText("Beats Per Minute, a Beat being the Divisor of Bar");
        final JPanel tempoLayoutPanel = new SmartPanel(new BorderLayout()); // without this panel, field would be sized vertically
        tempoLayoutPanel.add(tempoSpinner, pianoIsVertical ? BorderLayout.WEST : BorderLayout.NORTH);
        tempoLayoutPanel.setBorder(BorderFactory.createTitledBorder("Tempo"));
        
        timeSignatureChoice.setToolTipText("Time Signature, or Meter, or Bar Type");
        timeSignatureChoice.setEditable(true);
        timeSignatureChoice.setPreferredSize(new Dimension(60, 24)); // else much too wide when editable
        final JPanel timeLayoutPanel = new SmartPanel(new BorderLayout());
        timeLayoutPanel.add(timeSignatureChoice, pianoIsVertical ? BorderLayout.WEST : BorderLayout.NORTH);
        timeLayoutPanel.setBorder(BorderFactory.createTitledBorder("Time"));
        
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
        
        for (int i = 1; i < ToneSystem.INTERVAL_NAMES.length; i++) { // 1: not UNISON
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
}