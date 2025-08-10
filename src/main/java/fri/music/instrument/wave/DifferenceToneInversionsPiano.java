package fri.music.instrument.wave;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
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
    private JComponent pianoPanel;
    private IntervalRangeComponent intervalRange;
    private JPanel intervalListsPanel;
    private JPanel mainContainer;
    private JPanel centerPanel;
    private JButton closeAllIntervalFrames;
    private JCheckBox sortIntervalFrames;
    private PianoKeyConnector pianoKeyConnector;
    private DifferenceToneInversions differenceToneInversions;
    
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
        this.intervalRange = new IntervalRangeComponent(rangeChangeListener, rangeChangeListener);
        
        int index = 3;
        getControlPanel().add(intervalRange.getNarrowestChoice(), index++);
        getControlPanel().add(intervalRange.getWidestChoice(), index++);
    }
    
    private JPanel buildIntervalListsContainer() {
        this.intervalListsPanel = new JPanel(new FlowLayout(FlowLayout.LEFT)); // arrange frames from left to right
        intervalListsPanel.setToolTipText(
                "Press a keyboard-key to display all intervals that can generate it as difference-tone");
        intervalListsPanel.setBorder(BorderFactory.createTitledBorder(
                "Lists of Intervals Generating a Tone as Difference-Tone"));
        // set the panel to an initial height as long as there are no frames in it, else panel would collapse
        intervalListsPanel.add(Box.createVerticalStrut(160 + 10));
        
        this.sortIntervalFrames = new JCheckBox("Sorted Frames", true);
        sortIntervalFrames.setToolTipText("Insert New Interval-Frames Sorted by Pitch");
        
        this.closeAllIntervalFrames = new JButton("Close All Frames");
        closeAllIntervalFrames.setToolTipText("Close All Open Interval-Frames");
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
        intervalListsContainer.add(new JScrollPane(intervalListsPanel), BorderLayout.CENTER);
        intervalListsContainer.add(intervalListsToolbar, BorderLayout.SOUTH);
        
        return intervalListsContainer;
    }

    /** Called when user clicks a piano key. */
    protected void addIntervalListFrame(String ipnNoteName, int midiNoteNumber) {
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
        
        if (intervals != null) {
            final IntervalListFrame newFrame = new IntervalListFrame(ipnNoteName, midiNoteNumber, intervals);
            newFrame.frameCloseButton.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    addOrRemoveIntervalListFrame(newFrame, false);
                }
            });
            addOrRemoveIntervalListFrame(newFrame, true);
        }
    }
    
    private DifferenceToneInversions getDifferenceToneInversions() {
        if (differenceToneInversions == null) { // will be set to null on any parameter change
            differenceToneInversions = new DifferenceToneInversions(
                    new DifferenceToneInversions.Configuration(
                        getWaveSoundChannel().getTones(),
                        ToneSystem.semitoneSteps(intervalRange.narrowestAllowedInterval()),
                        ToneSystem.semitoneSteps(intervalRange.widestAllowedInterval()),
                        getDeviation())
                );
            differenceToneInversions.removeDissonant(false);
        }
        return differenceToneInversions;
    }

    private void addOrRemoveIntervalListFrame(IntervalListFrame frame, boolean isAdd) {
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
            setFrameSelected(frame);
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
    
    
    /** Called when clicking into an interval-frame title bar. */
    void setFrameSelected(IntervalListFrame intervalListFrame) {
        for (IntervalListFrame frame : getIntervalListFrames()) {
            final boolean isThis = (frame == intervalListFrame);
            frame.setTitleBarSelected(isThis);
            if (isThis)
                frame.scrollToVisible();
            else
                frame.clearListSelection();
        }
    }

    /** Called by interval-list frame when an interval is clicked. */
    void intervalSelected(IntervalListFrame activeFrame, DifferenceToneInversions.TonePair pair, boolean mouseDown) {
        // no other frame should show a selection
        setFrameSelected(activeFrame);
        
        if (mouseDown) { // start tone
            ignoreMouseEvents(true);
            
            final int volume = getVolumeSlider().getValue();
            pianoKeyConnector.noteOn(pair.lowerTone().midiNumber, volume);
            pianoKeyConnector.noteOn(pair.upperTone().midiNumber, volume);
        }
        else { // stop tone
            pianoKeyConnector.noteOff(pair.lowerTone().midiNumber);
            pianoKeyConnector.noteOff(pair.upperTone().midiNumber);
            
            ignoreMouseEvents(false);
        }
    }

    
    
    /** Overridden to return a DifferenceToneMouseHandler. */
    @Override
    protected MouseHandler newMouseHandler() {
        return new DifferenceToneInversionsMouseHandler(this, getWaveSoundChannel());
    }
    
    
    protected static class DifferenceToneInversionsMouseHandler extends DifferenceToneMouseHandler
    {
        private boolean active = true;
        
        public DifferenceToneInversionsMouseHandler(PianoWithSound piano, WaveSoundChannel soundChannel) {
            super(piano, soundChannel);
        }
        
        public void setActive(boolean active) {
            this.active = active;
        }

        @Override
        protected void pressed(InputEvent e) {
            if (active)
                getPiano().addIntervalListFrame(getKey(e).ipnName, getKey(e).midiNoteNumber);
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
        
        private final JList<DifferenceToneInversions.TonePair> list;
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
            
            this.list = new JList<>() {
                /** Avoid selection change by mouse drag. */
                protected void processMouseMotionEvent(MouseEvent e) {
                    if (e.getID() != MouseEvent.MOUSE_DRAGGED)
                        super.processMouseMotionEvent(e);
                }
            };
            list.addMouseListener(frameSelectionListener);
            list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
            list.setCellRenderer(new IntervalListCellRenderer());
            
            list.addMouseListener(new MouseAdapter() {
                private DifferenceToneInversions.TonePair currentlyPlaying;
                
                @Override
                public void mousePressed(MouseEvent e) {
                    if (SwingUtilities.isLeftMouseButton(e)) {
                        currentlyPlaying = list.getModel().getElementAt(list.locationToIndex(e.getPoint()));
                        if (currentlyPlaying != null)
                            intervalSelected(IntervalListFrame.this, currentlyPlaying, true);
                    }
                }
                @Override
                public void mouseReleased(MouseEvent e) {
                    if (currentlyPlaying != null) {
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
            frameTitle.setToolTipText("Intervals Generating "+ipnNoteName);
            frameTitle.addMouseListener(frameSelectionListener);
            
            this.frameTitleBar = new JPanel(new BorderLayout());
            frameTitleBar.add(frameTitle, BorderLayout.CENTER);
            frameTitleBar.add(frameCloseButton, BorderLayout.EAST);
            
            add(frameTitleBar, BorderLayout.NORTH);
            add(new JScrollPane(list), BorderLayout.CENTER);
            
            setBorder(BorderFactory.createLineBorder(Color.GRAY, 1, true));
            
            fillList(intervals); // put data into list
            
            SizeUtil.forceSize(this, new Dimension(190, 160)); // all frames should be equally sized
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
            list.setModel(model);
        }
        
        void clearListSelection() {
            list.clearSelection();
        }

        void setTitleBarSelected(boolean selected) {
            final Color borderColor = selected ? Color.BLACK : Color.LIGHT_GRAY;
            frameTitleBar.setBorder(BorderFactory.createLineBorder(borderColor, 1, true));
            final Color backgroundColor = selected ? list.getSelectionBackground() : list.getBackground();
            frameTitleBar.setBackground(backgroundColor);
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