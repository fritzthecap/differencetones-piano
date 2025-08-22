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
import java.util.ArrayList;
import java.util.List;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JToolBar;
import javax.swing.ListCellRenderer;
import javax.swing.ListSelectionModel;
import javax.swing.SwingUtilities;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import fri.music.ToneSystem;
import fri.music.differencetones.DifferenceToneInversions;
import fri.music.differencetones.DifferenceToneInversions.TonePair;
import fri.music.instrument.PianoWithSound;
import fri.music.swingutils.SizeUtil;
import fri.music.wavegenerator.WaveSoundChannel;

/**
 * A piano that displays a list of intervals that can generate
 * the tone you just clicked on the piano keyboard as difference-tone.
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
    
    private static final int INTERVAL_FRAME_HEIGHT = 160;
    
    private JComponent pianoPanel;
    private JPanel intervalListsPanel;
    private JPanel mainContainer;
    private JPanel centerPanel;
    private JButton closeAllIntervalFrames;
    private JCheckBox sortIntervalFrames;
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
        
        pianoPanel.add(buildUi(), BorderLayout.CENTER);
        
        return this.pianoPanel = pianoPanel;
    }
    
    /** Overridden to dynamically provide a panel for adding to <code>BorderLayout.CENTER</code>. */
    @Override
    public JComponent getPanelWithFreeCenter() {
        getKeyboard();
        
        if (this.centerPanel == null) {
            this.centerPanel = new JPanel(new BorderLayout());
            mainContainer.add(centerPanel, 0);
        }
        return centerPanel;
    }
    
    /** Listen to interval selection in any list frame. */
    public void setIntervalSelectionListener(IntervalSelectionListener intervalSelectionListener) {
        this.intervalSelectionListener = intervalSelectionListener;
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
        deviationComponent.getSlider().addChangeListener(new ChangeListener() {
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
    
    /** Called when user clicks a piano key, opens an interval list for the key when not already existing. */
    void addIntervalListFrame(String ipnNoteName, int midiNoteNumber) {
        // avoid duplicate frames
        for (IntervalListFrame frame : getIntervalListFrames()) {
            if (frame.ipnNoteName.equals(ipnNoteName)) {
                setFrameSelected(frame);
                return;
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
    void intervalSelected(IntervalListFrame activeFrame, Point point, DifferenceToneInversions.TonePair pair, boolean mouseDown) {
        setFrameSelected(activeFrame);
        
        playNotesWithoutOpeningIntervalFrames(
                new int[] { pair.lowerTone().midiNumber, pair.upperTone().midiNumber },
                mouseDown);
        
        if (mouseDown && intervalSelectionListener != null)
            intervalSelectionListener.intervalSelected(activeFrame, point, activeFrame.ipnNoteName, pair);
    }

    /** Called by interval-list frame when the title-bar of an interval is clicked. */
    void listTitleSelected(int midiNoteNumber, boolean mouseDown) {
        playNotesWithoutOpeningIntervalFrames(
                new int[] { midiNoteNumber },
                mouseDown);
    }

    // UI builder methods
    
    private JPanel buildUi() {
        buildControls();
        final JPanel listsContainer = buildIntervalListsContainer();

        this.mainContainer = new JPanel();
        final BoxLayout mainLayout = new BoxLayout(mainContainer, BoxLayout.Y_AXIS);
        mainContainer.setLayout(mainLayout);
        
        mainContainer.add(listsContainer);
        return mainContainer;
    }

    private void buildControls() {
        final ActionListener rangeChangeListener = new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                tuningParametersHaveChanged();
            }
        };
        addIntervalRangeActionListener(rangeChangeListener);
    }
    
    private JPanel buildIntervalListsContainer() {
        final FlowLayout layout = new FlowLayout(FlowLayout.LEFT); // arrange frames from left to right
        layout.setHgap(0);
        this.intervalListsPanel = new JPanel(layout);
        intervalListsPanel.setToolTipText(
                "Press a keyboard-key to display all intervals that can generate it as difference-tone");
        // set the panel to an initial height as long as there are no frames in it, else panel would collapse
        intervalListsPanel.add(Box.createVerticalStrut(INTERVAL_FRAME_HEIGHT + 4));
        
        this.sortIntervalFrames = new JCheckBox("Sort Lists by Pitch", true);
        sortIntervalFrames.setToolTipText("Insert New Interval-List Frames Sorted by Pitch");
        
        this.closeAllIntervalFrames = new JButton("Close All Lists");
        closeAllIntervalFrames.setToolTipText("Close All Open Interval-List Frames");
        closeAllIntervalFrames.setEnabled(false);
        closeAllIntervalFrames.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                closeAllIntervalFrames();
            }
        });
        
        final JToolBar intervalListsToolbar = new JToolBar();
        //intervalListsToolbar.add(Box.createHorizontalGlue()); // for right alignment
        intervalListsToolbar.add(sortIntervalFrames);
        intervalListsToolbar.add(closeAllIntervalFrames);
        
        final JPanel intervalListsContainer = new JPanel(new BorderLayout());
        final JScrollPane intervalListsScrollPane = new JScrollPane(intervalListsPanel);
        intervalListsScrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_NEVER);
        intervalListsScrollPane.setBorder(BorderFactory.createTitledBorder(
                "Lists of Intervals Generating a Tone as Difference-Tone"));
        intervalListsContainer.add(intervalListsScrollPane, BorderLayout.CENTER);
        intervalListsContainer.add(intervalListsToolbar, BorderLayout.SOUTH);
        
        return intervalListsContainer;
    }
    
    // callback helpers
    
    private DifferenceToneInversions getDifferenceToneInversions() {
        if (differenceToneInversions == null) { // will be set to null on any parameter change
            differenceToneInversions = new DifferenceToneInversions(
                    new DifferenceToneInversions.Configuration(
                        getWaveSoundChannel().getTones(),
                        ToneSystem.semitoneSteps(narrowestAllowedInterval()),
                        ToneSystem.semitoneSteps(widestAllowedInterval()),
                        getDeviation())
                );
            if (ToneSystem.MINOR_SECOND.equals(narrowestAllowedInterval()) == false &&
                    ToneSystem.MAJOR_SEVENTH.equals(widestAllowedInterval()) == false)
                differenceToneInversions.removeDissonant(false);
                // removing dissonant on minor-second would make choosing minor-second useless!
        }
        return differenceToneInversions;
    }

    private void addOrRemoveIntervalListFrame(final IntervalListFrame frame, boolean isAdd) {
        if (isAdd) {
            final List<IntervalListFrame> frames = getIntervalListFrames(); 
            int targetIndex = 0;
            if (sortIntervalFrames.isSelected()) { // sort in new frame
                while (targetIndex < frames.size() && frames.get(targetIndex).midiNoteNumber < frame.midiNoteNumber)
                    targetIndex++;
            }
            else { // append to end
                targetIndex = frames.size();
            }
            
            intervalListsPanel.add(frame, targetIndex);
            SwingUtilities.invokeLater(() -> setFrameSelected(frame));
        }
        else {
            intervalListsPanel.remove(frame);
        }
        
        refreshFramesContainer();
    }
    
    private void refreshFramesContainer() {
        closeAllIntervalFrames.setEnabled(getIntervalListFrames().size() > 0);

        intervalListsPanel.revalidate(); // refresh layout
        intervalListsPanel.repaint(); // refresh UI
    }
    
    /** Calculated difference-tone inversions are not valid anymore, replace or remove them. */
    private void tuningParametersHaveChanged() {
        differenceToneInversions = null;
        
        for (IntervalListFrame framePanel : getIntervalListFrames()) {
            final List<DifferenceToneInversions.TonePair> intervals = 
                    getDifferenceToneInversions().getIntervalsGenerating(framePanel.ipnNoteName);

            framePanel.fillList(intervals);
        }
        
        refreshFramesContainer();
    }
    
    private void closeAllIntervalFrames() {
        for (IntervalListFrame framePanel : getIntervalListFrames())
            intervalListsPanel.remove(framePanel);
        
        refreshFramesContainer();
    }

    private List<IntervalListFrame> getIntervalListFrames() {
        final List<IntervalListFrame> list = new ArrayList<>();
        for (Component c : intervalListsPanel.getComponents())
            if (c instanceof IntervalListFrame) // there is also a vertical strut in it
                list.add((IntervalListFrame) c);
        return list;
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
            frameTitle.setToolTipText("Intervals Generating "+ipnNoteName);
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
            
            SizeUtil.forceSize(this, new Dimension(190, INTERVAL_FRAME_HEIGHT));
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
            public Component getListCellRendererComponent(JList<? extends TonePair> list, TonePair tonePair, int index, boolean isSelected, boolean cellHasFocus) {
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