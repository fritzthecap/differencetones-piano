package fri.music.instrument.notespiano.wave;

import java.awt.Point;
import java.util.ArrayList;
import java.util.List;
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
import fri.music.player.notelanguage.MelodyFactory;
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
            /** Avoid opening interval frames when playing intervals. */
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
        
        // listen to interval-selection in list-frames for writing notes into intervals text-area
        getDifferenceToneInversionsPiano().setIntervalSelectionListener(
            new DifferenceToneInversionsPiano.IntervalSelectionListener() {
                @Override
                public void intervalSelected(JComponent list, Point point, String ipnNoteName, DifferenceToneInversions.TonePair interval) {
                    writeIntervalForMelody(list, point, ipnNoteName, interval);
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


    private void writeIntervalForMelody(
            JComponent list,
            Point point, 
            String ipnNoteName, 
            DifferenceToneInversions.TonePair interval)
    {
        final String upper = interval.upperTone().ipnName;
        final String lower = interval.lowerTone().ipnName;
        final String[] lengthNotations = getLengthNotation(list, point, ipnNoteName);
        final boolean putInTie = (lengthNotations.length > 1);
        if (putInTie)
            intervalNotes.setPermanentNotesCheck(false);
        
        try {
            for (int i = 0; i < lengthNotations.length; i++) {
                final String lengthNotation = lengthNotations[i];
                String tieStart = "", tieEnd = "";
                if (putInTie)
                    if (i == 0)
                        tieStart = NoteConnections.TIE_START_SYMBOL;
                    else if (i == lengthNotations.length - 1)
                        tieEnd = NoteConnections.TIE_END_SYMBOL;
                
                final String upperNote = tieStart + NoteConnections.CHORD_START_SYMBOL + Note.toString(upper, lengthNotation);
                final String lowerNote = lower + NoteConnections.CHORD_END_SYMBOL + tieEnd; // 2nd chord note needs no length
                writeSingleNote(intervalNotes, upperNote);
                writeSingleNote(intervalNotes, lowerNote);
            }
        }
        finally {
            if (putInTie)
                intervalNotes.setPermanentNotesCheck(true);
        }
    }
    
    private String[] getLengthNotation(JComponent list, Point point, String ipnNoteName) {
        final String melodyText = view().notesText.getText().trim();
        if (melodyText.isEmpty() == false) {
            final MelodyFactory melodyFactory = newMelodyFactory();
            final Note[][] melodyNotes = melodyFactory.translate(melodyText);
            
            final int occurrencePosition;
            final String intervalsText = intervalNotes.notesText.getText();
            if (intervalsText.isEmpty()) {
                occurrencePosition = 0;
            }
            else {
                final Note[][] intervals = melodyFactory.translate(intervalsText);
                occurrencePosition = intervals.length;
            }
            
            boolean inTie = false;
            final List<String> lengthList = new ArrayList<>();
            for (int i = occurrencePosition; i < melodyNotes.length; i++) {
                final Note[] chord = melodyNotes[i];
                final Note note = chord[0]; // there is just one note
                if (note.isRest()) {
                    writeSingleNote(intervalNotes, note.toString());
                }
                else {
                    if (Boolean.TRUE.equals(note.connectionFlags.tied()))
                        inTie = true;
                    
                    if (note.ipnName.equals(ipnNoteName))
                        lengthList.add(note.lengthNotation);
                    
                    if (Boolean.FALSE.equals(note.connectionFlags.tied()))
                        inTie = false;
                    
                    if (inTie == false)
                        return lengthList.toArray(new String[lengthList.size()]);
                }
            }
        }
        return new String[] { MelodyFactory.DEFAULT_NOTE_LENGTH };
    }
    
    private DifferenceToneInversionsPiano getDifferenceToneInversionsPiano() {
        return (DifferenceToneInversionsPiano) piano;
    }
    
    private DifferenceToneInversionsPiano.DifferenceToneInversionsMouseHandler getIntervalFrameOpeningMouseHandler() {
        return (DifferenceToneInversionsPiano.DifferenceToneInversionsMouseHandler) 
                getDifferenceToneInversionsPiano().getMouseHandler();
    }
}