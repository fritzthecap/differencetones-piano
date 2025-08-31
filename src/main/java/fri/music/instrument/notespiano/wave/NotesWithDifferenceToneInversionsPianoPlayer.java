package fri.music.instrument.notespiano.wave;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.List;
import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.text.JTextComponent;
import fri.music.SoundChannel;
import fri.music.differencetones.DifferenceToneInversions;
import fri.music.differencetones.composer.AbstractComposer;
import fri.music.differencetones.composer.DefaultComposer;
import fri.music.instrument.notespiano.NotesPianoPlayer;
import fri.music.instrument.notespiano.NotesTextPanelBase;
import fri.music.instrument.notespiano.PlayController;
import fri.music.instrument.notespiano.PlayControllerBase;
import fri.music.instrument.wave.DifferenceToneForNotesPiano;
import fri.music.instrument.wave.DifferenceToneInversionsPiano;
import fri.music.player.Note;
import fri.music.player.NotesUtil;
import fri.music.player.notelanguage.MelodyFactory;
import fri.music.player.notelanguage.NoteConnections;
import fri.music.swingutils.BorderUtil;

/**
 * The most complex view of this project.
 * Two notes text areas that allow composing difference-tone intervals
 * for a melody, entered in left side area, into right side area.
 * Both areas can be played as sound.
 */
public class NotesWithDifferenceToneInversionsPianoPlayer extends NotesPianoPlayer
{
    private PlayControllerBase intervalPlayController;
    private NotesTextPanelBase intervalNotes;
    private JButton autoCompose;
    private JCheckBox writeToIntervalsCheckbox;
    
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
    
    @Override
    protected void enableUiOnReadNotes(Exception e, NotesTextPanelBase view) {
        super.enableUiOnReadNotes(e, view);
        
        autoCompose.setEnabled(
                e == null && 
                view().notesText.getDocument().getLength() > 0 &&
                view().error.getText().isEmpty() && 
                intervalNotes.notesText.getDocument().getLength() <= 0);
    }
    
    /** Overridden to disallow chords. */
    @Override
    protected PlayController newPlayController() {
        final PlayController playController = super.newPlayController();
        playController.setDisallowChords(true);
        return playController;
    }
    
    @Override
    public void transpose(String intervalName, boolean upwards, JTextComponent textArea) {
        super.transpose(intervalName, upwards, textArea);
        
        if (intervalNotes.notesText.getDocument().getLength() > 0)
            transpose(intervalName, upwards, intervalNotes.notesText, true);
    }
    
    private NotesTextPanelBase buildIntervalNotesView() {
        this.intervalPlayController = new IntervalPlayController(this);
        
        final NotesTextPanelBase intervalNotes = new NotesTextPanelBase(
                intervalPlayController, 
                piano.config.isVertical, 
                false); // add controls to EAST
        intervalPlayController.setBaseView(intervalNotes); // connect controller to view
        
        buildNotesPanel(intervalPlayController, intervalNotes);
        
        this.autoCompose = new JButton("Auto-Compose");
        autoCompose.setToolTipText("Automatically Find Difference-Tone Intervals for Notes and Write Them into Textarea");
        autoCompose.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                autoCompose();
            }
        });
        autoCompose.setAlignmentX(JComponent.CENTER_ALIGNMENT);
        autoCompose.setEnabled(false);
        intervalNotes.notesControlPanel.add(autoCompose);

        this.writeToIntervalsCheckbox = new JCheckBox("Write Intervals", true);
        writeToIntervalsCheckbox.setToolTipText("Write Intervals from Listselection to Textarea");
        intervalNotes.notesControlPanel.add(piano.config.isVertical ? Box.createHorizontalGlue() : Box.createVerticalGlue());
        writeToIntervalsCheckbox.setAlignmentX(JComponent.CENTER_ALIGNMENT);
        intervalNotes.notesControlPanel.add(writeToIntervalsCheckbox);
        writeToIntervalsCheckbox.setEnabled(false);
        
        // listen to interval-selection in list-frames for writing notes into intervals text-area
        getDifferenceToneInversionsPiano().setIntervalSelectionListener(
            new DifferenceToneInversionsPiano.IntervalSelectionListener() {
                @Override
                public void intervalsAvailable(boolean yes) {
                    writeToIntervalsCheckbox.setEnabled(yes);
                }
                
                @Override
                public void intervalSelected(String ipnNoteName, DifferenceToneInversions.TonePair interval) {
                    if (writeToIntervalsCheckbox.isSelected() /*&& writeToIntervalsCheckbox.isEnabled()*/)
                        writeIntervalForMelody(ipnNoteName, interval);
                }
            }
        );
        
        // change the title of text-area
        final JScrollPane scrollPane = (JScrollPane) intervalNotes.notesText.getParent().getParent();
        scrollPane.setBorder(BorderUtil.titledBorder("Difference-Tone Intervals", DifferenceToneInversionsPiano.TITLE_FONTSIZE_INCREMENT));
        
        return intervalNotes;
    }
    
    // callbacks
    
    private void autoCompose() {
        final Note[][] notesArray = readNotesFromTextAreaCatchExceptions(getPlayController(), view());
        if (notesArray != null) {
            final DifferenceToneForNotesPiano differenceTonePiano = getDifferenceToneInversionsPiano();
            final AbstractComposer composer = new DefaultComposer(
                    differenceTonePiano.getWaveSoundChannel().getTones(),
                    differenceTonePiano.narrowestAllowedInterval(),
                    differenceTonePiano.widestAllowedInterval(),
                    differenceTonePiano.getDeviation());
            try {
                intervalNotes.error.setText("");
                
                final Note[][] composedIntervals = composer.compose(NotesUtil.toSingleNotesArray(notesArray));
                
                final MelodyFactory melodyFactory = newMelodyFactory();
                melodyFactory.setDisallowChords(false); // allow chords here!
                final String formatted = melodyFactory.formatBarLines(composedIntervals, true, true);
                intervalNotes.notesText.setText(formatted);
            }
            catch (Exception e) { // some tunings like HARMONIC_SERIES can not generate certain difference-tones
                intervalNotes.error.setText(
                        e.getMessage()+
                        " Tuning "+differenceTonePiano.getSelectedTuning()+
                        ", deviation "+Math.round(differenceTonePiano.getDeviation() * 2.0 * 100.0)+
                        ", bounds "+differenceTonePiano.narrowestAllowedInterval()+
                        " to "+differenceTonePiano.widestAllowedInterval());
            }
        }
    }

    
    private void writeIntervalForMelody(String ipnNoteName, DifferenceToneInversions.TonePair interval) {
        final String upper = interval.upperTone().ipnName;
        final String lower = interval.lowerTone().ipnName;
        final String[] lengthNotations = getLengthNotationsWriteRests(ipnNoteName);
        if (lengthNotations == null)
            return; // ipnNoteName not found in melody
        
        final boolean putInTie = (lengthNotations.length > 1);
        if (putInTie)
            setPermanentNotesCheck(false);
        
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
                setPermanentNotesCheck(true);
        }
    }
    
    private String[] getLengthNotationsWriteRests(String ipnNoteName) {
        final String melodyText = view().notesText.getText().trim();
        if (melodyText.isEmpty())
            return new String[] { MelodyFactory.DEFAULT_NOTE_LENGTH }; // no melody present, return default length
        
        final MelodyFactory melodyFactory = newMelodyFactory();
        final Note[][] melodyNotes = melodyFactory.translate(melodyText); // the singel-voiced melody in left text area
        
        final int occurrencePosition; // find the start index to search for ipnNoteName in melody
        final String intervalsText = intervalNotes.notesText.getText(); // by checking how many intervals were written
        if (intervalsText.isEmpty()) {
            occurrencePosition = 0; // no intervals were written yet
        }
        else { // get the last index of intervals
            final Note[][] intervalNotes = melodyFactory.translate(intervalsText);
            occurrencePosition = intervalNotes.length; // so far written position
        }
        
        boolean inTie = false;
        final List<String> lengthList = new ArrayList<>(); // possibly several tied notes
        
        // search for ipnNoteName in melodyNotes starting from occurrencePosition
        for (int i = occurrencePosition; i < melodyNotes.length; i++) {
            final Note[] chord = melodyNotes[i];
            final Note note = chord[0]; // there is just one note
            if (note.isRest()) { // jump over any rest, but copy it to intervals
                writeSingleNote(intervalNotes, note.toString());
            }
            else { // not a rest
                if (Boolean.TRUE.equals(note.connectionFlags.tied())) // a tie starts with TRUE
                    inTie = true;
                
                if (note.ipnName.equals(ipnNoteName)) // collect any note within a tie
                    lengthList.add(note.lengthNotation); // within a tie, pitch is the same for all tied notes
                
                if (Boolean.FALSE.equals(note.connectionFlags.tied())) // a tie ends with FALSE
                    inTie = false;
                
                if (inTie == false) // tie ended, or was not a tie
                    return lengthList.toArray(new String[lengthList.size()]);
            }
        }
        return null; // ipnNoteName not found in melody - happens when it was edited
    }
    
    private DifferenceToneInversionsPiano getDifferenceToneInversionsPiano() {
        return (DifferenceToneInversionsPiano) piano;
    }
    
    private DifferenceToneInversionsPiano.DifferenceToneInversionsMouseHandler getIntervalFrameOpeningMouseHandler() {
        return (DifferenceToneInversionsPiano.DifferenceToneInversionsMouseHandler) 
                getDifferenceToneInversionsPiano().getMouseHandler();
    }
    
    
    /** PlayController for the right-side interval text-area. */
    private class IntervalPlayController extends PlayControllerBase
    {
        private Note firstIntervalNote;
        
        public IntervalPlayController(NotesPianoPlayer notesPianoPlayer) {
            super(notesPianoPlayer);
        }

        /** Overridden to clear all list selections. */
        @Override
        public void reversePressed() {
            reset();
            super.reversePressed();
        }
        /** Overridden to clear all list selections. */
        @Override
        public void playPressed() {
            reset();
            super.playPressed();
        }

        /** Disable writeToIntervalsCheckbox while playing. */
        @Override
        protected void onEnableUiOnPlaying(boolean isStop) {
            super.onEnableUiOnPlaying(isStop);
            writeToIntervalsCheckbox.setEnabled(isStop);
        }
        
        @Override
        protected void onEmptyNotes() {
            if (writeToIntervalsCheckbox.isSelected() == false)
                writeToIntervalsCheckbox.doClick(); // triggers actionPerformed()
        }
        
        /** Select interval list item in frame when having two notes on playing. */
        @Override
        protected void playOrStopNote(SoundChannel soundChannel, Note note, boolean noteOn) {
            super.playOrStopNote(soundChannel, note, noteOn);
            if (noteOn) // wait for two notes
                if (firstIntervalNote == null)
                    firstIntervalNote = note;
                else
                    selectInterval(note);
        }

        /** Avoid opening interval list frames when playing intervals. */
        @Override
        protected void aroundPlayEvent(boolean before, boolean isNoteOn) { // no Swing calls allowed here!
            if (before && isNoteOn) // before turning on
                getIntervalFrameOpeningMouseHandler().setActive(false); // de-activate frame opener
            else if (before == false && isNoteOn == false) // after turning off
                getIntervalFrameOpeningMouseHandler().setActive(true); // re-activate frame opener
        }
        
        
        private void selectInterval(Note secondIntervalNote) {
            getDifferenceToneInversionsPiano().setFrameAndIntervalSelected(firstIntervalNote, secondIntervalNote);
            firstIntervalNote = null; // reset for next tuple
        }
        
        private void reset() {
            if (isPlaying() == false)
                getDifferenceToneInversionsPiano().clearIntervalFrameSelections();
            firstIntervalNote = null;
        }
    }
}