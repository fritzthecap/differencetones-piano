package fri.music.instrument.notespiano;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.text.JTextComponent;
import fri.music.instrument.wave.DifferenceToneInversionsPiano;
import fri.music.swingutils.BorderUtil;
import fri.music.swingutils.DialogUtil;
import fri.music.swingutils.TextAreaActions;

/** Basic view of NotesPianoPlayer with play- and format-buttons. */
public class NotesTextPanelBase extends JPanel
{
    public final JTextArea notesText;
    public final TextAreaActions textAreaActions;
    public final JTextComponent error;
    public final JPanel notesControlPanel;
    public final PlayControlButtons playButtons;
    public final JButton formatBars;
    public final JButton abcExport;
    
    protected final JPanel textAreaButtonsPanel;
    
    public NotesTextPanelBase(PlayControllerBase playController, boolean pianoIsVertical, boolean addControlPanelToWest) {
        super(new BorderLayout());
        
        // START build public fields
        final int ROWS = 9, COLUMNS = 20;
        this.notesText = new JTextArea(ROWS, COLUMNS);
        this.textAreaActions = new TextAreaActions(notesText);
        
        this.error = new JTextField();
        this.playButtons = new PlayControlButtons(playController);
        this.formatBars = new JButton("Format");
        
        this.abcExport = new JButton("ABC Export");
        this.textAreaButtonsPanel = new JPanel();
        
        this.notesControlPanel = buildNotesControlPanel(pianoIsVertical);
        // END build public fields
        
        final JComponent notesTextAndErrors = buildNotesTextArea(addControlPanelToWest);
        
        add(notesTextAndErrors, BorderLayout.CENTER);
        add(notesControlPanel, pianoIsVertical ? BorderLayout.NORTH : addControlPanelToWest ? BorderLayout.WEST : BorderLayout.EAST);
    }
    
    
    /** Factory method for AbcExportComponent, to be overridden. */
    protected AbcExportComponent newAbcExportComponent(String notesText) {
        return new AbcExportComponent(notesText);
    }
    
    
    private JComponent buildNotesTextArea(boolean addControlPanelToWest) {
        notesText.setToolTipText("Write Notes to be Played on Piano, Context Actions via Right Mouse Click");
        final JScrollPane notesTextScrollPane = new JScrollPane(notesText);
        notesTextScrollPane.setBorder(BorderUtil.titledBorder("Notes", DifferenceToneInversionsPiano.TITLE_FONTSIZE_INCREMENT));
        
        error.setBorder(BorderFactory.createTitledBorder("Error"));
        error.setToolTipText("First Found Syntax Error in Notes Text");
        error.setEditable(false);
        error.setForeground(Color.RED);
        
        abcExport.setToolTipText("Convert Notes Text to ABC Notation");
        abcExport.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                DialogUtil.showModelessDialog(
                        "Export to ABC",
                        notesText, // parent
                        newAbcExportComponent(notesText.getText()),
                        new Dimension(720, 530),
                        null);
            }
        });
        abcExport.setEnabled(false);
        
        textAreaButtonsPanel.add(abcExport);
        
        final JPanel errorsAndHelpPanel = new JPanel(new BorderLayout());
        errorsAndHelpPanel.add(textAreaButtonsPanel, addControlPanelToWest ? BorderLayout.WEST : BorderLayout.EAST);
        errorsAndHelpPanel.add(error, BorderLayout.CENTER);
        
        final JPanel textAreaAndError = new JPanel(new BorderLayout());
        textAreaAndError.add(notesTextScrollPane, BorderLayout.CENTER);
        textAreaAndError.add(errorsAndHelpPanel, BorderLayout.SOUTH);
        
        return textAreaAndError;
    }
    
    private JPanel buildNotesControlPanel(boolean pianoIsVertical) {
        formatBars.setToolTipText("Put each Bar into a Separate Line");
        
        final JPanel notesControlPanel = new JPanel(); // else button is too big
        notesControlPanel.setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createLineBorder(Color.GRAY),
                BorderFactory.createEmptyBorder(6, 6, 6, 6)));
        notesControlPanel.setLayout(new BoxLayout(notesControlPanel, pianoIsVertical ? BoxLayout.X_AXIS : BoxLayout.Y_AXIS));
        
        playButtons.setAlignmentX(JComponent.CENTER_ALIGNMENT);
        notesControlPanel.add(playButtons);
        notesControlPanel.add(Box.createRigidArea(new Dimension(1, 6))); // space to next button
        
        formatBars.setAlignmentX(JComponent.CENTER_ALIGNMENT);
        notesControlPanel.add(formatBars);
        notesControlPanel.add(Box.createRigidArea(new Dimension(1, 2))); // space to other control fields
        
        playButtons.setEnabled(false);
        formatBars.setEnabled(false);

        return notesControlPanel;
    }
}