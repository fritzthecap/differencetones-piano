package fri.music.instrument.notespiano;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.JToolBar;
import javax.swing.text.JTextComponent;
import fri.music.instrument.wave.DifferenceToneInversionsPiano;
import fri.music.utils.swing.BorderUtil;
import fri.music.utils.swing.text.TextAreaActions;

/**
 * Basic view of NotesPianoPlayer, with play- and format-buttons.
 * Contains a control-panel and a text-area with buttons and error field on bottom.
 */
public class NotesTextPanelBase extends JPanel
{
    public final JTextArea notesText;
    public final TextAreaActions textAreaActions;
    public final JTextComponent error;
    public final JPanel notesControlPanel;
    public final PlayControlButtons playButtons;
    public final JButton formatBars;
    public final JButton abcExport;
    
    protected final JToolBar textareaToolbar;
    
    public NotesTextPanelBase(PlayControllerBase playController, boolean pianoIsVertical, boolean addControlPanelToWest) {
        super(new BorderLayout());
        
        // START build public fields
        final int ROWS = 9, COLUMNS = 20;
        this.notesText = new JTextArea(ROWS, COLUMNS);
        this.textAreaActions = new TextAreaActions(notesText);
        // make font bigger
        for (int i = 0; i < 2; i++)
            textAreaActions.magnifyFont(true, notesText);
        
        this.error = new JTextField() {
            /** Always scroll to start when text is set. */
            @Override
            public void setText(String text) {
                super.setText(text);
                setCaretPosition(0);
            }
        };
        this.playButtons = new PlayControlButtons(playController);
        this.formatBars = new JButton("Format");
        
        this.abcExport = new JButton("ABC Export");
        this.textareaToolbar = new JToolBar();
        
        this.notesControlPanel = buildNotesControlPanel(pianoIsVertical);
        // END build public fields
        
        final JComponent notesTextAndErrors = buildNotesTextArea(addControlPanelToWest);
        
        add(notesTextAndErrors, BorderLayout.CENTER);
        add(notesControlPanel, pianoIsVertical ? BorderLayout.NORTH : addControlPanelToWest ? BorderLayout.WEST : BorderLayout.EAST);
    }
    
    private JComponent buildNotesTextArea(boolean addControlPanelToWest) {
        notesText.setToolTipText("Write Notes, or click Piano Keys; Use Context Actions via Right Mouse Click");
        final JScrollPane notesTextScrollPane = new JScrollPane(notesText);
        notesTextScrollPane.setBorder(BorderUtil.titledBorder("Melody Notes", DifferenceToneInversionsPiano.TITLE_FONTSIZE_INCREMENT));
        
        error.setBorder(BorderFactory.createTitledBorder("Error"));
        error.setToolTipText("First Found Syntax Error in Notes Text");
        error.setEditable(false);
        error.setForeground(Color.RED);
        
        abcExport.setToolTipText("Convert Notes Text to ABC Notation");
        abcExport.setEnabled(false);
        
        textareaToolbar.add(addControlPanelToWest ? abcExport : error);
        textareaToolbar.add(addControlPanelToWest ? error : abcExport);
        
        final JPanel textAreaAndError = new JPanel(new BorderLayout());
        textAreaAndError.add(notesTextScrollPane, BorderLayout.CENTER);
        textAreaAndError.add(textareaToolbar, BorderLayout.SOUTH);
        
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