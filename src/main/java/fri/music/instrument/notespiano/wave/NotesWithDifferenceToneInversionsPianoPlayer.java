package fri.music.instrument.notespiano.wave;

import javax.swing.BorderFactory;
import javax.swing.JComponent;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import fri.music.differencetones.DifferenceToneInversions;
import fri.music.instrument.notespiano.NotesPianoPlayer;
import fri.music.instrument.notespiano.NotesTextPanelBase;
import fri.music.instrument.notespiano.PlayControllerBase;
import fri.music.instrument.wave.DifferenceToneInversionsPiano;
import fri.music.player.Note;
import fri.music.player.notelanguage.NoteConnections;

public class NotesWithDifferenceToneInversionsPianoPlayer extends NotesPianoPlayer
{
    private NotesTextPanelBase intervalNotes;
    
    public NotesWithDifferenceToneInversionsPianoPlayer(DifferenceToneInversionsPiano piano) {
        super(piano);
    }

    @Override
    protected JComponent buildCenterPanel() {
        final JComponent superCenterPanel = super.buildCenterPanel();
        
        this.intervalNotes = buildIntervalNotesView();
        
        final JSplitPane centerPanel = new JSplitPane();
        centerPanel.setLeftComponent(superCenterPanel);
        centerPanel.setRightComponent(intervalNotes);
        centerPanel.setResizeWeight(0.5); // divider location in middle
        
        return centerPanel;
    }
    
    private NotesTextPanelBase buildIntervalNotesView() {
        final PlayControllerBase playController = new PlayControllerBase(this) {
            /** Avoid opening interval frames for played notes. */
            @Override
            protected void aroundPlayEvent(boolean before, boolean isNoteOn) { // no Swing calls allowed here!
                if (before && isNoteOn) // before turning on
                    getIntervalFrameOpeningMouseHandler().setActive(false); // de-activate frame opener
                else if (before == false && isNoteOn == false) // after turning off
                    getIntervalFrameOpeningMouseHandler().setActive(true); // re-activate frame opener
            }
        };
        
        final NotesTextPanelBase intervalNotes = new NotesTextPanelBase(playController, piano.config.isVertical);
        playController.setViewBase(intervalNotes);
        
        buildNotesPanel(playController, intervalNotes);
        
        // listen to interval-selection in list-frames for writing notes into text-area
        getDifferenceToneInversionsPiano().setIntervalSelectionListener(
            new DifferenceToneInversionsPiano.IntervalSelectionListener() {
                @Override
                public void intervalSelected(DifferenceToneInversions.TonePair interval, String lengthNotation) {
                    if (lengthNotation != null) {
                        final String upper = interval.upperTone().ipnName;
                        final String upperNote = NoteConnections.CHORD_START_SYMBOL + upper + Note.DURATION_SEPARATOR + lengthNotation;
                        final String lower = interval.lowerTone().ipnName;
                        final String lowerNote = lower + Note.DURATION_SEPARATOR + lengthNotation + NoteConnections.CHORD_END_SYMBOL;
                        writeSingleNote(intervalNotes, upperNote);
                        writeSingleNote(intervalNotes, lowerNote);
                    }
                }
            }
        );
        
        // change the title of text-area
        final JScrollPane scrollPane = (JScrollPane) intervalNotes.notesText.getParent().getParent();
        scrollPane.setBorder(BorderFactory.createTitledBorder("Difference-Tone Intervals"));
        
        // initialize empty state
        intervalNotes.formatBars.setEnabled(false);
        intervalNotes.playButtons.setEnabled(false);
        
        return intervalNotes;
    }
    
    private DifferenceToneInversionsPiano getDifferenceToneInversionsPiano() {
        return (DifferenceToneInversionsPiano) piano;
    }
    
    private DifferenceToneInversionsPiano.DifferenceToneInversionsMouseHandler getIntervalFrameOpeningMouseHandler() {
        return (DifferenceToneInversionsPiano.DifferenceToneInversionsMouseHandler) 
                getDifferenceToneInversionsPiano().getMouseHandler();
    }
}