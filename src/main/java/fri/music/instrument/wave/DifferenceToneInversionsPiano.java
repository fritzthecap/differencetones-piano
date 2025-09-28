package fri.music.instrument.wave;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.InputEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSlider;
import javax.swing.JSplitPane;
import javax.swing.JToolBar;
import javax.swing.ListCellRenderer;
import javax.swing.ListModel;
import javax.swing.ListSelectionModel;
import javax.swing.SwingUtilities;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import fri.music.ToneSystem;
import fri.music.differencetones.DifferenceToneInversions;
import fri.music.instrument.PianoWithSound;
import fri.music.player.Note;
import fri.music.swingutils.BorderUtil;
import fri.music.swingutils.layout.FlowLayoutForScrollPane;
import fri.music.swingutils.layout.SizeUtil;
import fri.music.swingutils.window.DialogStarter;
import fri.music.wavegenerator.WaveSoundChannel;

/**
 * A piano that displays a list of intervals that can generate
 * a tone (that you just clicked on the piano keyboard) as difference-tone.
 * Clicking one of the intervals will play the interval and mark the
 * difference-tone on the piano keyboard.
 */
public class DifferenceToneInversionsPiano extends DifferenceToneForNotesPiano
{
    /** Amount of points the titled border should be bigger as normal. */
    public static final float TITLE_FONTSIZE_INCREMENT = 4f;

    /** Listen to mouse clicks onto a interval list item in a interval list frame. */
    public interface IntervalSelectionListener
    {
        /**
         * Called any time when interval list frames get opened or closed.
         * @param yes true for at least one available, false otherwise.
         */
        void intervalsAvailable(boolean yes);
        
        /**
         * The given interval was clicked and played on piano.
         * @param ipnNoteName the tone the interval represents as difference-tone.
         * @param interval the selected difference-tone interval.
         * @param indexIgnoringRests the notes-index of the interval, counted ignoring rests.
         */
        void intervalSelected(
                String ipnNoteName, 
                DifferenceToneInversions.TonePair interval,
                int indexIgnoringRests);
    }
    
    /** Listeners get notified when tuning, deviation or interval range changed. */
    public interface TuningParametersChangeListener
    {
        /** A tuning parameter has changed. */
        void tuningParametersChanged();
    }
    
    private static final int INTERVAL_FRAME_WIDTH = 190;
    private static final int INTERVAL_FRAME_HEIGHT = 160;
    
    private static final String INTERVAL_LISTS_TITLE = "Lists of Intervals Generating a Note as Difference-Tone";
    
    private JComponent pianoPanel;
    
    /** Contains all interval lists. */
    private JPanel intervalListsPanel;
    /** Workaround to have an initial height in intervalListsPanel, it would collapse when empty. */
    private Component initialHeightHolder;
    /** The scroll pane for intervalListsPanel. */
    private JScrollPane listsScrollPane;
    /** The container of the scroll pane for intervalListsPanel, also containing the toolbar. */
    private JPanel listsContainer;
    /** The button bar below interval list frames and above piano. */
    private JToolBar intervalListsToolbar;
    
    /** The detached dialog is visible when this is not null. */
    private JDialog intervalListsDialog;
    /** Retain last dialog size. */
    private Dimension dialogSize;
    /** Retain last dialog location. */
    private Point dialogLocation;
    
    /** Layout that can be used to add another center component. */
    private JPanel centerPanel;
    
    private JButton closeAllIntervalFrames;
    private JButton detachIntervalFrames;
    private JCheckBox sortListsByPitch;
    private JCheckBox reuseOpenLists;
    
    /** Plays difference-tone intervals. */
    private PianoKeyConnector pianoKeyConnector;
    /** Difference-tone inversions cache. */
    private DifferenceToneInversions differenceToneInversions;
    
    /** Clients can listen to click on (or selection of) difference-tone intervals. */
    private IntervalSelectionListener intervalSelectionListener;
    
    private TuningParametersChangeListener tuningParametersChangeListener;
    
    private ToneSystem selectedToneSystem;
    
    private boolean openIntervalListWhenPianoKeyPressed = true;
    
    private List<IntervalListFrame> previousIntervalLists;
    
    /**
     * @param configuration the piano design configuration.
     * @param soundChannel the sound player of the piano.
     */
    public DifferenceToneInversionsPiano(PianoWithSound.Configuration configuration, WaveSoundChannel soundChannel) {
        super(configuration, soundChannel);
    }
    
    /** @return the main UI of this class, cached object-bound singleton. */
    @Override
    public JComponent getKeyboard() {
        if (this.pianoPanel != null)
            return this.pianoPanel;
        
        final JComponent pianoPanel = super.getKeyboard();
        
        this.pianoKeyConnector = new PianoKeyConnector(this);
        
        this.listsContainer = buildUi();
        pianoPanel.add(listsContainer, BorderLayout.CENTER);
        
        return this.pianoPanel = pianoPanel;
    }
    
    /** Overridden to dynamically provide a panel for adding to <code>BorderLayout.CENTER</code>. */
    @Override
    public JComponent getPanelWithFreeCenter() {
        getKeyboard(); // make sure keyboard was built
        
        if (this.centerPanel == null) {
            this.centerPanel = new JPanel(new BorderLayout());
            pianoPanel.remove(listsContainer);
            
            final JSplitPane splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT);
            splitPane.setLeftComponent(centerPanel); // upper
            splitPane.setRightComponent(listsContainer); // lower
            splitPane.setResizeWeight(0.5);
            
            pianoPanel.add(splitPane, BorderLayout.CENTER);
        }
        return centerPanel;
    }
    
    /** Applications initially may prefer to not reuse interval list frames. */
    public void setReuseIntervalLists(boolean reuseIntervalLists) {
        if (reuseIntervalLists != reuseOpenLists.isSelected()) {
            reuseOpenLists.setEnabled(true); // to allow user to control it
            reuseOpenLists.doClick(0); // trigger actionPerformed() to affect "Sort by Pitch" checkbox
            
            // when not reusing interval-lists, sorting makes no sense
            sortListsByPitch.setSelected(reuseIntervalLists);
        }
    }
    
    /** Player or piano keys will not open interval lists when false. */
    public void setOpenIntervalListWhenPianoKeyPressed(boolean openIntervalListWhenPianoKeyPressed) {
        this.openIntervalListWhenPianoKeyPressed = openIntervalListWhenPianoKeyPressed;
    }
    
    /** Applications may want to add buttons to interval-lists toolbar. */
    public void addToIntervalListsToolbar(Component button, int index) {
        intervalListsToolbar.add(button, index);
    }
    
    /** @return the currently selected tone-system. */
    public ToneSystem getToneSystem() {
        return selectedToneSystem;
    }
    
    /** Listen to interval selection in any list frame. */
    public void setIntervalSelectionListener(IntervalSelectionListener intervalSelectionListener) {
        this.intervalSelectionListener = intervalSelectionListener;
    }
    
    /** Listen to changes in any tuning parameters. */
    public void setTuningParametersChangeListener(TuningParametersChangeListener tuningParametersChangeListener) {
        this.tuningParametersChangeListener = tuningParametersChangeListener;
    }
    
    /** Melody notes text has changed. Rebuild all interval lists for given notes. */
    public void manageIntervalListFrames(Note[] singleNotes) {
        final List<IntervalListFrame> frames = removeAllIntervalFrames();
        
        for (int i = 0; i < singleNotes.length; i++) {
            final Note note = singleNotes[i];
            
            if (note.isRest() == false) {
                boolean done = false;
                
                if (i < frames.size()) {
                    final IntervalListFrame frame = frames.get(i);
                    if (frame.ipnNoteName.equals(note.ipnName)) { // reuse frame
                        addOrRemoveIntervalListFrame(frame, true);
                        done = true;
                    }
                }
                    
                if (done == false)
                    addIntervalListFrame(note.ipnName, note.midiNumber);
            }
        }
    }
    
    /** Interval-player wants to select a list and item while playing. */
    public void setFrameSelected(Note note, int frameIndex) {
        int index = 0;
        for (IntervalListFrame frame : getIntervalListFrames()) {
            if (frame.ipnNoteName.equals(note.ipnName) && (reuseOpenLists.isSelected() || frameIndex == index)) {
                setFrameSelected(frame);
                return;
            }
            index++;
        }
        System.err.println("Missed to select frame for note "+note);
    }

    /** Interval-player wants to select a list and item while playing. */
    public void setFrameAndIntervalSelected(Note note1, Note note2, int intervalIndex) {
        setIntervalSelected(note1, note2, intervalIndex, true);
    }

    /** Auto-Compose wants to select a list item. */
    public void setIntervalSelected(Note note1, Note note2, int intervalIndex) {
        setIntervalSelected(note1, note2, intervalIndex, false);
    }

    /** Player wants to clear all list selections before playing. */
    public void clearIntervalSelections() {
        for (IntervalListFrame frame : getIntervalListFrames())
            frame.intervalList.clearSelection();
    }

    
    /** Overridden to listen to tuning changes. */
    @Override
    protected TuningComponent newTuningComponent(String lowestToneIpnName, int octaves, WaveSoundChannel soundChannel) {
        final TuningComponent.Listener listener = new TuningComponent.Listener() {
            @Override
            public void tuningChanged(ToneSystem toneSystem) {
                selectedToneSystem = toneSystem;
                tuningParametersHaveChanged();
            }
        };
        return new TuningComponent(
                lowestToneIpnName, 
                octaves, 
                soundChannel,
                listener);
    }
    
    /** Overridden to listen to deviation changes. */
    @Override
    protected DeviationComponent newDeviationComponent(double deviation, boolean isVertical) {
        final DeviationComponent deviationComponent = super.newDeviationComponent(deviation, isVertical);
        final JSlider deviationSlider = deviationComponent.getSlider();
        deviationSlider.addChangeListener(new ChangeListener() {
            @Override
            public void stateChanged(ChangeEvent e) {
                tuningParametersHaveChanged();
            }
        });
        return deviationComponent;
    }
    
    /** Overridden to return a DifferenceToneInversionsMouseHandler. */
    @Override
    protected MouseHandler newMouseHandler() {
        return new DifferenceToneInversionsMouseHandler(this, getWaveSoundChannel());
    }
    
    // list frame callbacks
    
    /**
     * Called when clicking into an interval-frame title bar.
     * No other than given frame should show selection.
     * Also scrolls the frame to visible.
     */
    void setFrameSelected(IntervalListFrame intervalListFrame) {
        for (IntervalListFrame frame : getIntervalListFrames()) {
            final boolean isThis = (frame == intervalListFrame); // true just once
            frame.setTitleBarSelected(isThis);
            if (isThis)
                frame.scrollToVisible();
        }
    }

    /** Called by interval-list frame when an interval is clicked. */
    void intervalSelected(IntervalListFrame activeFrame, DifferenceToneInversions.TonePair interval, boolean mouseDown) {
        setFrameSelected(activeFrame);
        
        playNotesWithoutOpeningIntervalFrames(
                new int[] { interval.lowerTone().midiNumber, interval.upperTone().midiNumber },
                mouseDown);
        
        if (mouseDown && intervalSelectionListener != null) {
            int index;
            if (reuseOpenLists.isSelected())
                index = -1;
            else
                index = activeFrame.getIndex();
            
            intervalSelectionListener.intervalSelected(activeFrame.ipnNoteName, interval, index);
        }
    }

    /** Called by interval-list frame when the title-bar of an interval is clicked. */
    void listTitleSelected(int midiNoteNumber, boolean mouseDown) {
        playNotesWithoutOpeningIntervalFrames(
                new int[] { midiNoteNumber },
                mouseDown);
    }

    // UI builder methods
    
    private JPanel buildUi() {
        addIntervalRangeActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                tuningParametersHaveChanged();
            }
        });
        
        return buildIntervalListsContainer();
    }

    /** Calculated difference-tone inversions are not valid anymore, replace or remove them. */
    private void tuningParametersHaveChanged() {
        this.differenceToneInversions = null; // force new calculation by getDifferenceToneInversions()
        
        for (IntervalListFrame frame : getIntervalListFrames())
            frame.fillList(
                getDifferenceToneInversions().getIntervalsGenerating(frame.ipnNoteName));

        refreshListsContainer();
        
        if (tuningParametersChangeListener != null)
            tuningParametersChangeListener.tuningParametersChanged();
    }
    
    private JPanel buildIntervalListsContainer() {
        this.intervalListsPanel = new JPanel();
        intervalListsPanel.setToolTipText(
                "Press a keyboard-key to display all intervals that can generate it as difference-tone");
        // set the panel to an initial height as long as there are no frames in it, else panel would collapse
        //intervalListsPanel.setPreferredSize(new Dimension(0, INTERVAL_FRAME_HEIGHT + 4));
        this.initialHeightHolder = Box.createVerticalStrut(INTERVAL_FRAME_HEIGHT + 4);
        intervalListsPanel.add(initialHeightHolder);
        
        this.sortListsByPitch = new JCheckBox("Sort Lists by Pitch", true);
        sortListsByPitch.setToolTipText("Insert New Interval-Lists Sorted by Difference-Tone Pitch");
        sortListsByPitch.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                sortIntervalFrames(sortListsByPitch.isSelected());
            }
        });
        
        this.reuseOpenLists = new JCheckBox("Reuse Open Lists", true);
        reuseOpenLists.setToolTipText("When OFF, Every Note Will Have Its Own Interval List");
        final ActionListener reuseActionListener = new ActionListener() {
            /** Switching off reuse of open lists  makes sortIntervalFrames useless. */
            @Override
            public void actionPerformed(ActionEvent e) {
                final boolean reuse = reuseOpenLists.isSelected();
                sortListsByPitch.setEnabled(reuse);
                if (reuse)
                    closeDuplicateIntervalListFrames();
            }
        };
        reuseOpenLists.addActionListener(reuseActionListener);
        reuseActionListener.actionPerformed(null);
        reuseOpenLists.setEnabled(false); // makes no sense here to use this
        
        this.closeAllIntervalFrames = new JButton("Close All Lists");
        closeAllIntervalFrames.setToolTipText("Close All Open Interval-List Frames");
        closeAllIntervalFrames.setEnabled(false);
        closeAllIntervalFrames.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                removeAllIntervalFrames();
            }
        });
        
        this.detachIntervalFrames = new JButton("Detach Lists");
        detachIntervalFrames.setToolTipText("Detach Interval-List Frames into Separate Window");
        detachIntervalFrames.setEnabled(false);
        detachIntervalFrames.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                detachIntervalFrames();
            }
        });
        
        this.intervalListsToolbar = new JToolBar();
        intervalListsToolbar.add(sortListsByPitch);
        intervalListsToolbar.add(reuseOpenLists);
        intervalListsToolbar.add(detachIntervalFrames);
        intervalListsToolbar.add(closeAllIntervalFrames);
        
        // add "Help" button at right side
        intervalListsToolbar.add(Box.createHorizontalGlue(), -1); // -1: append to end
        intervalListsToolbar.add(buildHelpButton(), -1);
        
        this.listsContainer = new JPanel(new BorderLayout());
        addIntervalListsToInternalScrollPane();
        listsContainer.add(intervalListsToolbar, BorderLayout.SOUTH);
        
        return listsContainer;
    }

    private JComponent buildHelpButton() {
        final JButton help = new JButton("Help");
        help.setToolTipText("Interval Lists Description");
        help.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                DialogStarter.showModelessHtmlDialog(
                        "Interval Lists Description", 
                        help, 
                        HelpForIntervalLists.URL,
                        null);
            }
        });
        return help;
    }
    
    private void addIntervalListsToInternalScrollPane() {
        setLayoutToIntervalListsPanel(false);
        
        if (this.listsScrollPane == null)
            this.listsScrollPane = new JScrollPane(intervalListsPanel);

        listsScrollPane.setBorder(BorderUtil.titledBorder(INTERVAL_LISTS_TITLE, TITLE_FONTSIZE_INCREMENT));
        listsContainer.add(listsScrollPane, BorderLayout.CENTER);
    }

    private void setLayoutToIntervalListsPanel(boolean forDetachedDialog) {
        final FlowLayout layout = forDetachedDialog
                ? new FlowLayoutForScrollPane(FlowLayout.LEADING, 0, 0)
                : new FlowLayout(FlowLayout.LEADING, 0, 0);
        intervalListsPanel.setLayout(layout);
    }
    
    // callback helpers
    
    private DifferenceToneInversions getDifferenceToneInversions() {
        if (this.differenceToneInversions == null) { // will be set to null on any parameter change
            this.differenceToneInversions = new DifferenceToneInversions(
                    new DifferenceToneInversions.Configuration(
                        getWaveSoundChannel().getTones(),
                        ToneSystem.semitoneSteps(narrowestAllowedInterval()),
                        ToneSystem.semitoneSteps(widestAllowedInterval()),
                        getDeviation())
                );
            if (ToneSystem.MINOR_SECOND.equals(narrowestAllowedInterval()) == false &&
                    ToneSystem.MAJOR_SEVENTH.equals(widestAllowedInterval()) == false)
                this.differenceToneInversions.removeDissonant(false);
                // removing dissonants on minor-second would make choosing minor-second useless!
        }
        return this.differenceToneInversions;
    }

    private void closeDuplicateIntervalListFrames() {
        final Set<String> ipnNames = new HashSet<>();
        boolean changesDone = false;
        for (IntervalListFrame frame : getIntervalListFrames()) {
            if (ipnNames.contains(frame.ipnNoteName)) {
                intervalListsPanel.remove(frame);
                changesDone = true;
            }
            ipnNames.add(frame.ipnNoteName);
        }
        
        if (changesDone)
            refreshListsContainer();
    }
    
    private void sortIntervalFrames(boolean doSort) {
        // remove and get current frames
        List<IntervalListFrame> currentIntervalLists = removeAllIntervalFrames();
        // rebuild
        if (doSort) { // memorize sort template for reversing
            if (currentIntervalLists.size() > 0)
                previousIntervalLists = new ArrayList<>(currentIntervalLists);
            else
                previousIntervalLists = null;
        }
        else if (previousIntervalLists != null) { // use memorized template when not null
            currentIntervalLists = merge(currentIntervalLists, previousIntervalLists);
        }
        
        for (IntervalListFrame frame : currentIntervalLists)
            addOrRemoveIntervalListFrame(frame, true);
    }

    private List<IntervalListFrame> merge(
            List<IntervalListFrame> currentIntervalLists, 
            List<IntervalListFrame> previousIntervalLists)
    {
        final List<IntervalListFrame> result = new ArrayList<>(currentIntervalLists.size());
        for (IntervalListFrame previousFrame : previousIntervalLists) {
            final IntervalListFrame inCurrent = currentIntervalLists.stream()
                .filter(f -> f.ipnNoteName.equals(previousFrame.ipnNoteName) && result.contains(f) == false)
                .findFirst()
                .orElse(null);
            if (inCurrent != null)
                result.add(inCurrent);
        }
        this.previousIntervalLists = null; // garbage-collect
        return result;
    }

    /** Called when user clicks a piano key. Opens an interval list for the key when not already existing. */
    private void addIntervalListFrame(String ipnNoteName, int midiNoteNumber) {
        // avoid duplicate frames
        if (reuseOpenLists.isSelected()) {
            for (IntervalListFrame frame : getIntervalListFrames()) {
                if (frame.ipnNoteName.equals(ipnNoteName)) {
                    setFrameSelected(frame);
                    return; // do not add a new one
                }
            }
        }
        
        // not found, add new one when intervals exist for it
        final List<DifferenceToneInversions.TonePair> intervals = 
                getDifferenceToneInversions().getIntervalsGenerating(ipnNoteName);
        
        final IntervalListFrame newFrame = new IntervalListFrame(ipnNoteName, midiNoteNumber, intervals);
        newFrame.frameCloseButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                addOrRemoveIntervalListFrame(newFrame, false);
            }
        });
        addOrRemoveIntervalListFrame(newFrame, true);
    }
    
    private void addOrRemoveIntervalListFrame(IntervalListFrame frame, boolean isAdd) {
        if (isAdd) {
            int targetIndex = -1; // by default append to end
            if (sortListsByPitch.isEnabled() && sortListsByPitch.isSelected()) { // sort-in new frame
                targetIndex = 0;
                final List<IntervalListFrame> frames = getIntervalListFrames();
                while (targetIndex < frames.size() && frames.get(targetIndex).midiNoteNumber < frame.midiNoteNumber)
                    targetIndex++;
            }
            intervalListsPanel.add(frame, targetIndex);
            //setFrameSelected(frame);
        }
        else {
            intervalListsPanel.remove(frame);
        }
        refreshListsContainer();
    }
    
    private boolean refreshListsContainer() {
        final boolean listFramesExist = (getIntervalListFrames().size() > 0);
        closeAllIntervalFrames.setEnabled(listFramesExist);
        detachIntervalFrames.setEnabled(intervalListsDialog == null && listFramesExist);
        
        listsScrollPane.getParent().revalidate(); // do NOT use listsContainer here, as lists my be in dialog!
        listsScrollPane.getParent().repaint();
        
        if (intervalSelectionListener != null)
            intervalSelectionListener.intervalsAvailable(listFramesExist);
        
        return listFramesExist;
    }
    
    private List<IntervalListFrame> removeAllIntervalFrames() {
        final List<IntervalListFrame> frames = getIntervalListFrames();
        for (IntervalListFrame frame : frames)
            intervalListsPanel.remove(frame);
        
        refreshListsContainer();
        
        return frames;
    }

    private void setIntervalSelected(Note note1, Note note2, int intervalIndex, boolean selectAlsoFrame) {
        int index = 0;
        for (IntervalListFrame frame : getIntervalListFrames()) {
            final DifferenceToneInversions.TonePair tonePair = frame.containsInterval(note1, note2);
            // ignore intervalIndex when reuseOpenFrames is on, 
            // because then the number of notes is different from the number of interval list frames
            if (tonePair != null && (reuseOpenLists.isSelected() || index == intervalIndex)) {
                if (selectAlsoFrame)
                    setFrameSelected(frame);
                frame.selectItem(tonePair);
                return;
            }
            index++;
        }
    }

    private List<IntervalListFrame> getIntervalListFrames() {
        final List<IntervalListFrame> list = new ArrayList<>();
        for (Component c : intervalListsPanel.getComponents())
            if (c instanceof IntervalListFrame) // there is also a vertical strut in it
                list.add((IntervalListFrame) c);
        return list;
    }
    
    private void detachIntervalFrames() {
        if (intervalListsDialog != null)
            return; // dialog is showing
        
        detachIntervalFrames.setEnabled(false); // mark dialog showing
        
        if (dialogSize == null) {
            final Dimension listsContainerSize = listsContainer.getSize();
            dialogSize = new Dimension(
                    listsContainerSize.width + 20, // let it protrude a little so that it is visible
                    listsContainerSize.height + 20);
        }
        
        listsContainer.remove(listsScrollPane);
        listsContainer.revalidate(); // make it visibly empty
        listsContainer.repaint();
        
        listsScrollPane.setBorder(null); // no titled border within titled dialog
        if (initialHeightHolder != null) {
            intervalListsPanel.remove(initialHeightHolder); // not needed any more
            initialHeightHolder = null;
        }
        
        setLayoutToIntervalListsPanel(true);
        
        intervalListsDialog = DialogStarter.showModelessDialog( // detach frames
                INTERVAL_LISTS_TITLE,
                listsContainer,
                listsScrollPane,
                dialogSize,
                dialogLocation,
                true);
        
        intervalListsDialog.addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                dialogSize = intervalListsDialog.getSize(); // remember for next launch
                dialogLocation = intervalListsDialog.getLocation();
                intervalListsDialog = null;
                
                addIntervalListsToInternalScrollPane();
                refreshListsContainer();
            }
        });
    }
    

    
    private void ignoreMouseEvents(boolean ignore) {
        final DifferenceToneInversionsMouseHandler mouseHandler = (DifferenceToneInversionsMouseHandler) getMouseHandler();
        mouseHandler.setActive(ignore == false);
        
        for (PianoWithSound.Keyboard.Key key : getKeys())
            key.setIgnoreMouse(ignore == true);
    }
    
    private void playNotesWithoutOpeningIntervalFrames(int[] midiNoteNumbers, boolean mouseDown) {
        if (mouseDown == true) // start tone
            ignoreMouseEvents(true);
            
        final int volume = getVolumeSlider().getValue();
        for (int midiNoteNumber : midiNoteNumbers)
            if (mouseDown)
                pianoKeyConnector.noteOn(midiNoteNumber, volume);
            else
                pianoKeyConnector.noteOff(midiNoteNumber);
            
        if (mouseDown == false) // stop tone
            ignoreMouseEvents(false);
    }

    
    
    /** For every clicked piano-key this creates an interval-list frame showing possible intervals. */
    public static class DifferenceToneInversionsMouseHandler extends DifferenceToneMouseHandler
    {
        private boolean active = true;
        
        public DifferenceToneInversionsMouseHandler(DifferenceToneInversionsPiano piano, WaveSoundChannel soundChannel) {
            super(piano, soundChannel);
        }
        
        /** Turns the listener on or off. */
        public void setActive(boolean active) {
            this.active = active;
        }

        /** Overridden to add an interval frame when active and a tone gets played. */
        @Override
        protected void pressed(InputEvent e) {
            final DifferenceToneInversionsPiano piano = getPiano();
            if (active && piano.openIntervalListWhenPianoKeyPressed) {
                final Keyboard.Key keyboardKey = getKey(e);
                piano.addIntervalListFrame(keyboardKey.ipnName, keyboardKey.midiNoteNumber);
            }
            super.pressed(e);
        }
        
        private DifferenceToneInversionsPiano getPiano() {
            return (DifferenceToneInversionsPiano) piano;
        }
    }
    
    
    /** The interval-list frame, showing possible intervals for a difference-tone. */
    private class IntervalListFrame extends JPanel
    {
        public final String ipnNoteName;
        public final int midiNoteNumber;
        public final JButton frameCloseButton;
        
        private final JList<DifferenceToneInversions.TonePair> intervalList;
        private final JLabel frameTitle;
        private final JPanel frameTitleBar;
        
        IntervalListFrame(String ipnNoteName, int midiNoteNumber, List<DifferenceToneInversions.TonePair> intervals) {
            super(new BorderLayout());
            
            this.ipnNoteName = ipnNoteName;
            this.midiNoteNumber = midiNoteNumber;
            
            final MouseListener frameSelectionListener = new MouseAdapter() {
                @Override
                public void mousePressed(MouseEvent e) {
                    setFrameSelected(IntervalListFrame.this);
                }
            };
            
            this.intervalList = new JList<>() {
                /** Avoid selection change by mouse drag. */
                protected void processMouseMotionEvent(MouseEvent e) {
                    if (e.getID() != MouseEvent.MOUSE_DRAGGED)
                        super.processMouseMotionEvent(e);
                }
            };
            intervalList.addMouseListener(frameSelectionListener);
            intervalList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
            intervalList.setCellRenderer(new IntervalListCellRenderer());
            
            // play the selected interval when mouse is pressed, as long as not released
            intervalList.addMouseListener(new MouseAdapter() {
                private DifferenceToneInversions.TonePair currentlyPlaying;
                
                @Override
                public void mousePressed(MouseEvent e) {
                    if (SwingUtilities.isLeftMouseButton(e)) {
                        final int index = intervalList.locationToIndex(e.getPoint());
                        if (index >= 0 && index < intervalList.getModel().getSize()) {
                            currentlyPlaying = intervalList.getModel().getElementAt(index);
                            if (currentlyPlaying != null)
                                intervalSelected(IntervalListFrame.this, currentlyPlaying, true);
                        }
                    }
                }
                @Override
                public void mouseReleased(MouseEvent e) {
                    if (SwingUtilities.isLeftMouseButton(e) && currentlyPlaying != null) {
                        intervalSelected(IntervalListFrame.this, currentlyPlaying, false);
                        currentlyPlaying = null;
                    }
                }
            });
            
            this.frameCloseButton = new JButton("X");
            frameCloseButton.setBorder(BorderFactory.createEmptyBorder(0, 8, 0, 8));
            frameCloseButton.setContentAreaFilled(false);
            frameCloseButton.setFocusPainted(false);
            frameCloseButton.setToolTipText("Close List");
            
            this.frameTitle = new JLabel(ipnNoteName, JLabel.CENTER);
            frameTitle.setToolTipText("All Difference-Tone Intervals Generating "+ipnNoteName);
            frameTitle.addMouseListener(frameSelectionListener);
            frameTitle.addMouseListener(new MouseAdapter() {
                @Override
                public void mousePressed(MouseEvent e) {
                    if (SwingUtilities.isLeftMouseButton(e))
                        listTitleSelected(IntervalListFrame.this.midiNoteNumber, true);
                }
                @Override
                public void mouseReleased(MouseEvent e) {
                    if (SwingUtilities.isLeftMouseButton(e))
                        listTitleSelected(IntervalListFrame.this.midiNoteNumber, false);
                }
            });
            
            this.frameTitleBar = new JPanel(new BorderLayout());
            frameTitleBar.add(frameTitle, BorderLayout.CENTER);
            frameTitleBar.add(frameCloseButton, BorderLayout.EAST);
            
            add(frameTitleBar, BorderLayout.NORTH);
            add(new JScrollPane(intervalList), BorderLayout.CENTER);
            
            setBorder(BorderFactory.createLineBorder(Color.GRAY, 1, true));
            
            fillList(intervals); // put data into list
            
            SizeUtil.forceSize(this, new Dimension(INTERVAL_FRAME_WIDTH, INTERVAL_FRAME_HEIGHT));
        }
        
        int getIndex() {
            int myIndex = 0;
            for (Component c : getParent().getComponents())
                if (c instanceof IntervalListFrame)
                    if (c == this)
                        return myIndex;
                    else
                        myIndex++;
            throw new IllegalStateException("The IntervaListFrame is not in parent list!");
        }

        // methods called by outer class
        
        void scrollToVisible() {
            Rectangle bounds = getBounds();
            if (bounds.width <= 0 && intervalListsDialog == null) // this workaround would not work in detached dialog!
                bounds = new Rectangle(INTERVAL_FRAME_WIDTH * getIndex(), 2, INTERVAL_FRAME_WIDTH, INTERVAL_FRAME_HEIGHT);
            intervalListsPanel.scrollRectToVisible(bounds);
        }

        void fillList(List<DifferenceToneInversions.TonePair> intervals) {
            final DefaultListModel<DifferenceToneInversions.TonePair> model = new DefaultListModel<>();
            final int numberOfItems;
            if (intervals != null) {
                for (DifferenceToneInversions.TonePair tonePair : intervals)
                    model.addElement(tonePair);
                numberOfItems = intervals.size();
            }
            else {
                numberOfItems = 0;
            }
            frameTitle.setText(ipnNoteName+"  ("+numberOfItems+")");
            intervalList.setModel(model);
        }
        
        void setTitleBarSelected(boolean selected) {
            final Color borderColor = selected ? Color.BLACK : Color.LIGHT_GRAY;
            frameTitleBar.setBorder(BorderFactory.createLineBorder(borderColor, 1, true));
            final Color backgroundColor = selected ? intervalList.getSelectionBackground() : intervalList.getBackground();
            frameTitleBar.setBackground(backgroundColor);
            frameTitleBar.paintImmediately(frameTitleBar.getVisibleRect());
        }
        
        DifferenceToneInversions.TonePair containsInterval(Note note1, Note note2) {
            final ListModel<DifferenceToneInversions.TonePair> model = intervalList.getModel();
            for (int i = 0; i < model.getSize(); i++) {
                final DifferenceToneInversions.TonePair tonePair = model.getElementAt(i);
                if ((tonePair.lowerTone().midiNumber == note1.midiNumber && 
                            tonePair.upperTone().midiNumber == note2.midiNumber) ||
                        (tonePair.lowerTone().midiNumber == note2.midiNumber && 
                            tonePair.upperTone().midiNumber == note1.midiNumber))
                    return tonePair;
            }
            return null;
        }

        void selectItem(DifferenceToneInversions.TonePair tonePair) {
            intervalList.setSelectedValue(tonePair, true);
        }
        
        @Override
        public String toString() {
            return ipnNoteName;
        }


        private static class IntervalListCellRenderer implements ListCellRenderer<DifferenceToneInversions.TonePair>
        {
            private final JPanel listLine = new JPanel(new BorderLayout());
            private JLabel intervalName = new JLabel();
            private JLabel lowerNoteName = new JLabel("", JLabel.RIGHT);
            private JLabel upperNoteName = new JLabel("", JLabel.LEFT);
            
            IntervalListCellRenderer() {
                listLine.add(intervalName, BorderLayout.WEST);
                
                listLine.add(Box.createRigidArea(new Dimension(4, -1)), BorderLayout.CENTER);
                
                final Dimension size = new Dimension(28, -1);
                SizeUtil.forceSize(lowerNoteName, size);
                SizeUtil.forceSize(upperNoteName, size);
                final JLabel separator = new JLabel(" ", JLabel.CENTER);
                final Dimension separatorSize = new Dimension(5, -1);
                SizeUtil.forceSize(separator, separatorSize);
                
                final JPanel noteNames = new JPanel(new BorderLayout());
                noteNames.setOpaque(false); // else panel will be gray
                noteNames.add(lowerNoteName, BorderLayout.WEST);
                noteNames.add(separator, BorderLayout.CENTER);
                noteNames.add(upperNoteName, BorderLayout.EAST);
                
                listLine.add(noteNames, BorderLayout.EAST);
            }
            
            @Override
            public Component getListCellRendererComponent(
                    JList<? extends DifferenceToneInversions.TonePair> list, 
                    DifferenceToneInversions.TonePair tonePair, 
                    int index, 
                    boolean isSelected, 
                    boolean cellHasFocus)
            {
                intervalName.setText(tonePair.intervalName());
                lowerNoteName.setText(tonePair.lowerTone() != null ? tonePair.lowerTone().ipnName : "");
                upperNoteName.setText(tonePair.upperTone() != null ? tonePair.upperTone().ipnName : "");
                
                if (isSelected) {
                    listLine.setBackground(list.getSelectionBackground());
                    listLine.setForeground(list.getSelectionForeground());
                }
                else {
                    listLine.setBackground(list.getBackground());
                    listLine.setForeground(list.getForeground());
                }
                return listLine;
            }
        }
    }
}