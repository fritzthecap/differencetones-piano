package fri.music.instrument.notespiano.wave;

import java.awt.Dimension;
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
import fri.music.Tone;
import fri.music.ToneSystem;
import fri.music.differencetones.DifferenceToneInversions;
import fri.music.differencetones.DifferenceTones;
import fri.music.differencetones.composer.AbstractComposer;
import fri.music.differencetones.composer.DefaultComposer;
import fri.music.instrument.notespiano.NotesPianoPlayer;
import fri.music.instrument.notespiano.NotesTextPanelBase;
import fri.music.instrument.notespiano.PlayController;
import fri.music.instrument.notespiano.PlayControllerBase;
import fri.music.instrument.notespiano.abc.AbcExportComponent;
import fri.music.instrument.wave.DifferenceToneInversionsPiano;
import fri.music.player.Note;
import fri.music.player.NotesUtil;
import fri.music.player.notelanguage.MelodyFactory;
import fri.music.player.notelanguage.NoteConnections;
import fri.music.player.notelanguage.abc.AbcExport;
import fri.music.player.notelanguage.abc.AbcTunesCombiner;
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
    private JButton generateMelody;
    private JCheckBox writeToIntervalsCheckbox;
    
    public NotesWithDifferenceToneInversionsPianoPlayer(DifferenceToneInversionsPiano piano) {
        super(piano);
        
        piano.setTuningParametersChangeListener(new DifferenceToneInversionsPiano.TuningParametersChangeListener() {
            @Override
            public void tuningParametersChanged() {
                intervalNotes.notesText.setText("");
            }
        });
    }

    /** Overridden to also transpose interval text area. */
    @Override
    public boolean transpose(String intervalName, boolean upwards, JTextComponent textArea) {
        if (upwards) { // first try to transpose intervals as they are higher and may get out of range first
            if (transposeIntervals(intervalName, upwards))
                return super.transpose(intervalName, upwards, textArea);
        }
        else { // first try to transpose melody notes as they are lower and may get out of range first
            if (super.transpose(intervalName, upwards, textArea))
                return transposeIntervals(intervalName, upwards);
        }
        return false;
    }

    private boolean transposeIntervals(String intervalName, boolean upwards) {
        if (intervalNotes.notesText.getText().trim().length() > 0) {
            try {
                transposeThrowing(intervalName, upwards, intervalNotes.notesText);
            }
            catch (Exception e) {
                intervalNotes.error.setText(e.getMessage());
                return false;
            }
        }
        return true;
    }
    
    @Override
    protected JComponent buildCenterPanel() {
        final JComponent superCenterPanel = super.buildCenterPanel();
        
        // do not reuse interval list frames, because we want to compose clicking along from left to right
        getDifferenceToneInversionsPiano().setReuseIntervalLists(false);
        // piano should not open interval list frames by itself, because this is done here by text area listeners
        getDifferenceToneInversionsPiano().setOpenIntervalListWhenPianoKeyPressed(false);
        // but as soon as piano does not write to text area any more, it should open frames by itself
        melodyView().writeToNotesCheckbox.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                final boolean pianoWritesToTextArea = melodyView().writeToNotesCheckbox.isSelected();
                final boolean notesTextEmpty = (melodyView().notesText.getText().trim().length() <= 0);
                getDifferenceToneInversionsPiano().setOpenIntervalListWhenPianoKeyPressed(
                        false == pianoWritesToTextArea &&
                        notesTextEmpty);
            }
        });
        
        this.intervalNotes = buildIntervalNotesView(); // right side text area and play controller
        
        final JSplitPane centerPanel = new JSplitPane();
        centerPanel.setLeftComponent(superCenterPanel);
        centerPanel.setRightComponent(intervalNotes);
        centerPanel.setResizeWeight(0.5); // divider location in middle
        
        return centerPanel;
    }
    
    @Override
    protected void enableUiOnReadNotes(Exception e, Note[][] notes, NotesTextPanelBase view) {
        super.enableUiOnReadNotes(e, notes, view);
        
        final boolean notesTextPresent = (melodyView().notesText.getText().trim().length() > 0);
        final boolean intervalTextPresent = (intervalNotes.notesText.getText().trim().length() > 0);
        
        autoCompose.setEnabled(
                e == null &&
                notesTextPresent == true &&
                melodyView().error.getText().isEmpty() && 
                intervalTextPresent == false);
        
        generateMelody.setEnabled(
                e == null &&
                intervalTextPresent == true &&
                notesTextPresent == false);
        
        if (notes != null && view == melodyView()) // no error occurred, view is melody text
            manageIntervalListsOnMelodyChange(notes);
    }
    
    @Override
    protected void enableUiOnPlaying(boolean isStop, Note[][] sounds, int currentSoundIndex, NotesTextPanelBase view) {
        super.enableUiOnPlaying(isStop, sounds, currentSoundIndex, view);
        
        getDifferenceToneInversionsPiano().setTuningControlsEnabled(isStop);
        getDifferenceToneInversionsPiano().setDifferenceToneParametersEnabled(isStop);
        
        if (sounds != null && view == melodyView()) // no error occurred, view is melody text
            manageIntervalListsOnMelodyChange(sounds);
    }
    
    /** Overridden to disallow chords. */
    @Override
    protected PlayController newPlayController() {
        final PlayController playController  = new PlayController(this) {
            /** Overridden to let piano open interval list frames when a key was pressed. */
            @Override
            protected void onEmptyNotes() {
                super.onEmptyNotes();
                getDifferenceToneInversionsPiano().setOpenIntervalListWhenPianoKeyPressed(true);
            }
            @Override
            protected void onNonEmptyNotes(String timeSignatureOnTop, Integer tempoOnTop) {
                super.onNonEmptyNotes(timeSignatureOnTop, tempoOnTop);
                getDifferenceToneInversionsPiano().setOpenIntervalListWhenPianoKeyPressed(false);
            }
            /** Overridden to select the interval list frame of given note. */
            @Override
            protected void playOrStopNote(SoundChannel soundChannel, Note note, boolean noteOn) {
                super.playOrStopNote(soundChannel, note, noteOn);
                if (noteOn)
                    getDifferenceToneInversionsPiano().setFrameSelected(note, getCurrentIndexIgnoringRests());
            }
        };
        playController.setDisallowChords(true);
        return playController;
    }
    
    /** Overridden to export both melody and intervals to ABC. */
    @Override
    protected AbcExportComponent newAbcExportComponent(NotesTextPanelBase view) {
        if (view == intervalNotes) {
            final String melodyText = melodyView().notesText.getText().trim();
            final boolean includeMelodyOption = (melodyText.length() > 0);
            
            return new AbcExportComponent(view.notesText.getText(), newMelodyFactory(), includeMelodyOption) {
                @Override
                protected String export(AbcExport.Configuration configuration, String notesText, boolean includeTuning) {
                    final String intervalsAbcText = super.export(configuration, notesText, true); // intervals always need tuning
                    if (includeMelody == null || includeMelody.isSelected() == false || melodyText.length() <= 0)
                        return intervalsAbcText;
                    
                    final String melodyAbcText = super.export(configuration, melodyText, true);
                    return new AbcTunesCombiner().combine("Intervals", intervalsAbcText, "Melody", melodyAbcText);
                }
            };
        }
        return super.newAbcExportComponent(view);
    }
    
    /** Overridden return the ToneSystem from DifferenceToneInversionsPiano, needed by ABC export. */
    @Override
    protected ToneSystem getToneSystem() {
        return getDifferenceToneInversionsPiano().getToneSystem();
    }
    
    
    private NotesTextPanelBase buildIntervalNotesView() {
        this.intervalPlayController = new IntervalPlayController(this);
        
        final NotesTextPanelBase intervalNotes = 
                new NotesTextPanelBase(intervalPlayController, piano.config.isVertical, false); // false: add controls to EAST
        intervalPlayController.setBaseView(intervalNotes); // connect controller to view
        
        addExportActionListener(intervalNotes);
        
        buildNotesPanel(intervalPlayController, intervalNotes);
        
        this.autoCompose = new JButton("Auto-Compose");
        autoCompose.setToolTipText("Automatically Find Difference-Tone Intervals for Melody and Write Them into Textarea");
        autoCompose.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                autoCompose();
            }
        });
        autoCompose.setAlignmentX(JComponent.CENTER_ALIGNMENT);
        autoCompose.setEnabled(false);
        intervalNotes.notesControlPanel.add(autoCompose);

        this.generateMelody = new JButton("Generate Melody");
        generateMelody.setEnabled(false);
        generateMelody.setToolTipText("Generate Melody Notes from Intervals When No Melody Is Present");
        generateMelody.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                try {
                    generateMelody();
                }
                catch (Exception ex) {
                    intervalNotes.error.setText(ex.getMessage());
                }
            }
        });
        generateMelody.setAlignmentX(JComponent.CENTER_ALIGNMENT);
        intervalNotes.notesControlPanel.add(Box.createRigidArea(new Dimension(1, 2))); // space to button above
        intervalNotes.notesControlPanel.add(generateMelody);

        this.writeToIntervalsCheckbox = new JCheckBox("Write Intervals", true);
        writeToIntervalsCheckbox.setToolTipText("Write Intervals from List-Selection to Textarea");
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
                    if (writeToIntervalsCheckbox.isSelected())
                        writeIntervalForNote(ipnNoteName, interval);
                }
            }
        );
        
        // change the title of text-area
        final JScrollPane scrollPane = (JScrollPane) intervalNotes.notesText.getParent().getParent();
        scrollPane.setBorder(BorderUtil.titledBorder("Difference-Tone Intervals", DifferenceToneInversionsPiano.TITLE_FONTSIZE_INCREMENT));
        intervalNotes.notesText.setToolTipText("Write Intervals, in Order of Notes, by Clicking onto the According List Item");
        
        return intervalNotes;
    }
    
    // callbacks
    
    private Note[][] manageIntervalListsOnMelodyChange(Note[][] melody) {
        if (melody == null)
            melody = readNotesFromTextAreaCatchExceptions(getPlayController(), melodyView());
        
        if (melody != null) {
            // open interval chooser lists for all notes of melody
            // there are only single notes in view(), no intervals
            Note[] singleNotes = NotesUtil.toSingleNotesArray(melody);
            getDifferenceToneInversionsPiano().manageIntervalListFrames(singleNotes);
        }
        return melody;
    }
    
    private void autoCompose() {
        final Note[][] melody = manageIntervalListsOnMelodyChange(null);
        
        if (melody != null) {
            final DifferenceToneInversionsPiano differenceTonePiano = getDifferenceToneInversionsPiano();
            
            final AbstractComposer composer = new DefaultComposer(
                    differenceTonePiano.getWaveSoundChannel().getTones(),
                    differenceTonePiano.narrowestAllowedInterval(),
                    differenceTonePiano.widestAllowedInterval(),
                    differenceTonePiano.getDeviation());
            try {
                intervalNotes.error.setText("");
                
                final Note[][] composedIntervals = composer.compose(NotesUtil.toSingleNotesArray(melody));
                int index = 0;
                for (Note[] chord : composedIntervals) {
                    if (chord[0].isRest() == false) {
                        differenceTonePiano.setIntervalSelected(chord[0], chord[1], index);
                        index++;
                    }
                }
                
                final MelodyFactory melodyFactory = newMelodyFactory();
                //melodyFactory.setDisallowChords(false); // allow chords here!
                final String formatted = melodyFactory.formatBarLines(composedIntervals, false, false);
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
    
    private void writeIntervalForNote(String ipnNoteName, DifferenceToneInversions.TonePair interval) {
        final String upper = interval.upperTone().ipnName;
        final String lower = interval.lowerTone().ipnName;
        final String[] lengthNotations = getLengthNotationsWriteRests(ipnNoteName);
        if (lengthNotations == null)
            return; // ipnNoteName not found in melody
        
        final boolean putInTie = (lengthNotations.length > 1); // a tied note has been found
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
        final String melodyText = melodyView().notesText.getText().trim();
        if (melodyText.isEmpty())
            return new String[] { MelodyFactory.DEFAULT_NOTE_LENGTH }; // no melody present, return default length
        
        final MelodyFactory melodyFactory = newMelodyFactory();
        final Note[][] melodyNotes = melodyFactory.translate(melodyText); // the single-voiced melody in left text area
        
        // find the start index to search for ipnNoteName in melody by checking how many intervals were written
        final int occurrencePosition;
        final String intervalsText = intervalNotes.notesText.getText().trim();
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
    

    private void generateMelody() {
        final String intervalsText = intervalNotes.notesText.getText().trim();
        final MelodyFactory melodyFactory = newMelodyFactory();
        final Note[][] intervalNotes = melodyFactory.translate(intervalsText);
        
        final DifferenceTones differenceTones = new DifferenceTones(
                getDifferenceToneInversionsPiano().getWaveSoundChannel().getTones(),
                getDifferenceToneInversionsPiano().getDeviation()
            );
        
        final Note[][] melodyNotes = new Note[intervalNotes.length][];
        
        for (int i = 0; i < intervalNotes.length; i++) {
            final Note[] chord = intervalNotes[i];
            
            if (chord.length == 1 && chord[0].isRest()) {
                melodyNotes[i] = new Note[] { chord[0] };
            }
            else { // not a rest
                if (chord.length != 2)
                    throw new IllegalArgumentException("Can generate melody only from intervals containing exactly two notes!");
                    
                final Note upperNote = chord[0];
                final Note lowerNote = chord[1];
                final Tone melodyNote = differenceTones.findDifferenceTones(lowerNote, upperNote)[0];
                melodyNotes[i] = new Note[] { new Note(melodyNote, upperNote) };
            }
        }
        
        final boolean tempoOnTop = (melodyFactory.getTempoOnTop() != null);
        final boolean timeSignatureOnTop = (melodyFactory.getTimeSignatureOnTop() != null);
        
        final String melodyText = melodyFactory.formatBarLines(melodyNotes, tempoOnTop, timeSignatureOnTop);
        melodyView().notesText.setText(melodyText);
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
            super.aroundPlayEvent(before, isNoteOn);
            
            if (before && isNoteOn) // before turning on
                getIntervalFrameOpeningMouseHandler().setActive(false); // de-activate frame opener
            else if (before == false && isNoteOn == false) // after turning off
                getIntervalFrameOpeningMouseHandler().setActive(true); // re-activate frame opener
        }
        
        
        private void selectInterval(Note secondIntervalNote) {
            getDifferenceToneInversionsPiano().setFrameAndIntervalSelected(
                    firstIntervalNote, 
                    secondIntervalNote, 
                    getCurrentIndexIgnoringRests());
            firstIntervalNote = null; // reset for next tuple
        }
        
        private void reset() {
            if (isPlaying() == false)
                getDifferenceToneInversionsPiano().clearIntervalSelections();
            firstIntervalNote = null;
        }
    }
}