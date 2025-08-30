package fri.music.instrument.wave;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.InputEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.ArrayList;
import java.util.List;
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
import fri.music.swingutils.DialogUtil;
import fri.music.swingutils.FlowLayoutForScrollPane;
import fri.music.swingutils.SizeUtil;
import fri.music.wavegenerator.WaveSoundChannel;

/**
 * A piano that displays a list of intervals that can generate
 * a tone (that you just clicked on the piano keyboard) as difference-tone.
 * Clicking one of the intervals will play the interval and mark the
 * difference-tone on the piano keyboard.
 */
public class DifferenceToneInversionsPiano extends DifferenceToneForNotesPiano
{
    /** Listen to mouse clicks onto a interval list item in a interval list frame. */
    public interface IntervalSelectionListener
    {
        /**
         * The given interval was clicked and played on piano.
         * @param ipnNoteName the tone the interval represents as difference-tone.
         * @param interval the selected difference-tone interval.
         */
        void intervalSelected(JComponent list, Point point, String ipnNoteName, DifferenceToneInversions.TonePair interval);
    }
    
    private static final int INTERVAL_FRAME_WIDTH = 190;
    private static final int INTERVAL_FRAME_HEIGHT = 160;
    
    private static final String INTERVAL_LISTS_TITLE = "Lists of Intervals Generating a Tone as Difference-Tone";
    
    private JComponent pianoPanel;
    private JPanel intervalListsPanel; // contains all interval lists
    private Component initialHeightHolder;
    private JScrollPane listsScrollPane;
    private JPanel listsContainer; // contains intervalListsScrollPane and its toolbar
    private Dimension dialogSize;
    private Point dialogLocation;
    private JPanel centerPanel;
    private JButton closeAllIntervalFrames;
    private JButton detachIntervalFrames;
    private JCheckBox sortIntervalFrames;
    private JCheckBox reuseOpenFrames;
    private PianoKeyConnector pianoKeyConnector;
    private DifferenceToneInversions differenceToneInversions;
    
    private IntervalSelectionListener intervalSelectionListener;
    
    public DifferenceToneInversionsPiano(PianoWithSound.Configuration config, WaveSoundChannel soundChannel) {
        super(config, soundChannel);
    }
    
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
    
    /** Listen to interval selection in any list frame. */
    public void setIntervalSelectionListener(IntervalSelectionListener intervalSelectionListener) {
        this.intervalSelectionListener = intervalSelectionListener;
    }
    
    /** Player wants to select a list line while playing. */
    public void setFrameAndIntervalSelected(Note note1, Note note2) {
        for (IntervalListFrame frame : getIntervalListFrames()) {
            final DifferenceToneInversions.TonePair tonePair = frame.containsInterval(note1, note2);
            if (tonePair != null) {
                setFrameSelected(frame);
                frame.selectItem(tonePair);
                return;
            }
        }
    }

    /** Player wants to clear all list selections before playing. */
    public void clearIntervalFrameSelections() {
        for (IntervalListFrame frame : getIntervalListFrames())
            frame.intervalList.clearSelection();
    }
    
    
    /** Overridden to listen to tuning changes. */
    @Override
    protected TuningComponent newTuningComponent(String lowestToneIpnName, int octaves, WaveSoundChannel soundChannel) {
        final TuningComponent.Listener listener = new TuningComponent.Listener() {
            @Override
            public void tuningChanged(ToneSystem toneSystem) {
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
        deviationComponent.getSlider().addChangeListener(new ChangeListener() {
            @Override
            public void stateChanged(ChangeEvent e) {
                if (deviationSlider.getValueIsAdjusting() == false)
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
    
    /** Called when user clicks a piano key, opens an interval list for the key when not already existing. */
    void addIntervalListFrame(String ipnNoteName, int midiNoteNumber) {
        // avoid duplicate frames
        if (reuseOpenFrames.isSelected()) {
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
    void intervalSelected(IntervalListFrame activeFrame, Point point, DifferenceToneInversions.TonePair interval, boolean mouseDown) {
        setFrameSelected(activeFrame);
        
        playNotesWithoutOpeningIntervalFrames(
                new int[] { interval.lowerTone().midiNumber, interval.upperTone().midiNumber },
                mouseDown);
        
        if (mouseDown && intervalSelectionListener != null)
            intervalSelectionListener.intervalSelected(activeFrame, point, activeFrame.ipnNoteName, interval);
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

    private JPanel buildIntervalListsContainer() {
        this.intervalListsPanel = new JPanel();
        intervalListsPanel.setToolTipText(
                "Press a keyboard-key to display all intervals that can generate it as difference-tone");
        // set the panel to an initial height as long as there are no frames in it, else panel would collapse
        //intervalListsPanel.setPreferredSize(new Dimension(0, INTERVAL_FRAME_HEIGHT + 4));
        this.initialHeightHolder = Box.createVerticalStrut(INTERVAL_FRAME_HEIGHT + 4);
        intervalListsPanel.add(initialHeightHolder);
        
        this.sortIntervalFrames = new JCheckBox("Sort Lists by Pitch", true);
        sortIntervalFrames.setToolTipText("Insert New Interval-Lists Sorted by Difference-Tone Pitch");
        
        this.reuseOpenFrames = new JCheckBox("Reuse Open Lists", true);
        reuseOpenFrames.setToolTipText("When OFF, Every Note Will Have Its Own Interval List");
        reuseOpenFrames.addActionListener(new ActionListener() {
            /** Switching off reuse of open lists  makes sortIntervalFrames useless. */
            @Override
            public void actionPerformed(ActionEvent e) {
                sortIntervalFrames.setEnabled(reuseOpenFrames.isSelected());
            }
        });
        
        this.closeAllIntervalFrames = new JButton("Close All Lists");
        closeAllIntervalFrames.setToolTipText("Close All Open Interval-List Frames");
        closeAllIntervalFrames.setEnabled(false);
        closeAllIntervalFrames.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                closeAllIntervalFrames();
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
        
        final JToolBar intervalListsToolbar = new JToolBar();
        intervalListsToolbar.add(sortIntervalFrames);
        intervalListsToolbar.add(reuseOpenFrames);
        intervalListsToolbar.add(detachIntervalFrames);
        intervalListsToolbar.add(closeAllIntervalFrames);
        
        this.listsContainer = new JPanel(new BorderLayout());
        addIntervalListsToInternalScrollPane();
        listsContainer.add(intervalListsToolbar, BorderLayout.SOUTH);
        
        return listsContainer;
    }

    private void addIntervalListsToInternalScrollPane() {
        setLayoutToIntervalListsPanel(false);
        
        if (this.listsScrollPane == null)
            this.listsScrollPane = new JScrollPane(intervalListsPanel);

        listsScrollPane.setBorder(BorderFactory.createTitledBorder(INTERVAL_LISTS_TITLE));
        listsContainer.add(listsScrollPane, BorderLayout.CENTER);
    }

    private void setLayoutToIntervalListsPanel(boolean forDetachedDialog) {
        final FlowLayout layout = forDetachedDialog
                ? new FlowLayoutForScrollPane(FlowLayout.LEADING)
                : new FlowLayout(FlowLayout.LEADING);
        layout.setHgap(0);
        layout.setVgap(0);
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

    private void addOrRemoveIntervalListFrame(final IntervalListFrame frame, boolean isAdd) {
        if (isAdd) {
            final List<IntervalListFrame> frames = getIntervalListFrames(); 
            int targetIndex = 0;
            if (sortIntervalFrames.isEnabled() && sortIntervalFrames.isSelected()) { // sort in new frame
                while (targetIndex < frames.size() && frames.get(targetIndex).midiNoteNumber < frame.midiNoteNumber)
                    targetIndex++;
            }
            else { // append to end
                targetIndex = frames.size();
            }
            
            intervalListsPanel.add(frame, targetIndex);
            setFrameSelected(frame);
        }
        else {
            intervalListsPanel.remove(frame);
        }
        
        refreshIntervalListsPanel();
    }
    
    private void refreshIntervalListsPanel() {
        final boolean listFramesExist = (getIntervalListFrames().size() > 0);
        closeAllIntervalFrames.setEnabled(listFramesExist);
        detachIntervalFrames.setEnabled(listFramesExist);
        
        listsScrollPane.getParent().revalidate();
        listsScrollPane.getParent().repaint();
    }
    
    /** Calculated difference-tone inversions are not valid anymore, replace or remove them. */
    private void tuningParametersHaveChanged() {
        this.differenceToneInversions = null; // force new calculation by getDifferenceToneInversions()
        
        for (IntervalListFrame framePanel : getIntervalListFrames())
            framePanel.fillList(
                getDifferenceToneInversions().getIntervalsGenerating(framePanel.ipnNoteName));

        refreshIntervalListsPanel();
    }
    
    private void closeAllIntervalFrames() {
        for (IntervalListFrame framePanel : getIntervalListFrames())
            framePanel.getParent().remove(framePanel);
        
        refreshIntervalListsPanel();
    }

    private List<IntervalListFrame> getIntervalListFrames() {
        final List<IntervalListFrame> list = new ArrayList<>();
        for (Component c : intervalListsPanel.getComponents())
            if (c instanceof IntervalListFrame) // there is also a vertical strut in it
                list.add((IntervalListFrame) c);
        return list;
    }
    
    private void detachIntervalFrames() {
        if (detachIntervalFrames.isEnabled() == false)
            return; // dialog is showing
        
        if (dialogSize == null) {
            final Dimension listsContainerSize = listsContainer.getSize();
            dialogSize = new Dimension(
                    listsContainerSize.width + 20, // let it protrude a little so that it is visible
                    listsContainerSize.height + 16);
        }
        
        listsContainer.remove(listsScrollPane);
        listsContainer.revalidate(); // make it visibly empty
        listsContainer.repaint();
        
        listsScrollPane.setBorder(null); // no titled border within titled dialog
        intervalListsPanel.remove(initialHeightHolder); // not needed any more
        
        setLayoutToIntervalListsPanel(true);
        final JDialog dialog = DialogUtil.showModelessDialog(
                INTERVAL_LISTS_TITLE,
                listsContainer,
                listsScrollPane,
                dialogSize,
                dialogLocation);
        
        dialog.addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                dialogSize = dialog.getSize(); // remember for next launch
                dialogLocation = dialog.getLocation();
                
                addIntervalListsToInternalScrollPane();
                refreshIntervalListsPanel();
            }
            @Override
            public void windowClosed(WindowEvent e) {
                System.err.println("window is closed!");
            }
        });
        
        detachIntervalFrames.setEnabled(false);
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

    
    /** Creates an interval-frame showing possible intervals for every clicked piano-key. */
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
            if (active) {
                final Keyboard.Key keyboardKey = getKey(e);
                getPiano().addIntervalListFrame(keyboardKey.ipnName, keyboardKey.midiNoteNumber);
            }
            super.pressed(e);
        }
        
        private DifferenceToneInversionsPiano getPiano() {
            return (DifferenceToneInversionsPiano) piano;
        }
    }
    
    
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
                                intervalSelected(IntervalListFrame.this, e.getPoint(), currentlyPlaying, true);
                        }
                    }
                }
                @Override
                public void mouseReleased(MouseEvent e) {
                    if (SwingUtilities.isLeftMouseButton(e) && currentlyPlaying != null) {
                        intervalSelected(IntervalListFrame.this, e.getPoint(), currentlyPlaying, false);
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
        
        // methods called by outer class
        
        void scrollToVisible() {
            ((JComponent) getParent()).scrollRectToVisible(getBounds());
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