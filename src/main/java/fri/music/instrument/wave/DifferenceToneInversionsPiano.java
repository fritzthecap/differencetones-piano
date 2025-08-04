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
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ListCellRenderer;
import javax.swing.ListSelectionModel;
import javax.swing.SwingUtilities;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import fri.music.ToneSystem;
import fri.music.differencetones.DifferenceToneInversions;
import fri.music.differencetones.DifferenceToneInversions.TonePair;
import fri.music.instrument.PianoWithSound;
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
    private JButton closeAllIntervalFrames;
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
        
        this.closeAllIntervalFrames = new JButton("Close Frames");
        closeAllIntervalFrames.setEnabled(false);
        closeAllIntervalFrames.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                tuningParametersHaveChanged();
            }
        });
        getControlPanel().add(closeAllIntervalFrames);
        
        final ActionListener rangeChangeListener = new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                tuningParametersHaveChanged();
            }
        };
        this.intervalRange = new IntervalRangeComponent(rangeChangeListener, rangeChangeListener);
        
        getControlPanel().add(intervalRange.getNarrowestChoice(), 3);
        getControlPanel().add(intervalRange.getWidestChoice(), 4);
        
        this.intervalListsPanel = createIntervalListsPanel();
        final JComponent intervalListsScrollPane = new JScrollPane(intervalListsPanel);
        pianoPanel.add(intervalListsScrollPane, BorderLayout.CENTER);
        
        return this.pianoPanel = pianoPanel;
    }
    
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
    
    private JPanel createIntervalListsPanel() {
        final JPanel intervalListsPanel = new JPanel(new FlowLayout(FlowLayout.LEFT)); // arrange frames from left to right, not LEADING
        intervalListsPanel.setToolTipText(
                "Press a keyboard-key to display all intervals that can generate it as difference-tone");
        intervalListsPanel.setBorder(BorderFactory.createTitledBorder(
                "Lists of Intervals Generating a Tone as Difference-Tone"));
        
        // set the panel to an initial height as long as there are no frames in it, else panel would collapse
        intervalListsPanel.add(Box.createVerticalStrut(160));
        
        return intervalListsPanel;
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
        
        final List<DifferenceToneInversions.TonePair> intervals = 
                differenceToneInversions.getIntervalsGenerating(ipnNoteName);
        
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

    private void addOrRemoveIntervalListFrame(IntervalListFrame framePanel, boolean isAdd) {
        if (isAdd) {
            final List<IntervalListFrame> frames = getIntervalListFrames(); // sort in new frame
            int targetIndex = 0;
            while (targetIndex < frames.size() && frames.get(targetIndex).midiNoteNumber < framePanel.midiNoteNumber)
                targetIndex++;
            
            intervalListsPanel.add(framePanel, targetIndex);
            setFrameSelected(framePanel);
            closeAllIntervalFrames.setEnabled(true);
        }
        else {
            intervalListsPanel.remove(framePanel);
            closeAllIntervalFrames.setEnabled(getIntervalListFrames().size() > 0);
        }
        intervalListsPanel.revalidate(); // refresh layout
        intervalListsPanel.repaint(); // refresh UI
    }
    
    /** Reset calculated difference tone inversions, close all interval-frames. */
    private void tuningParametersHaveChanged() {
        differenceToneInversions = null;
        
        for (IntervalListFrame framePanel : getIntervalListFrames())
            intervalListsPanel.remove(framePanel);
        
        intervalListsPanel.revalidate(); // refresh layout
        intervalListsPanel.repaint(); // refresh UI
        
        closeAllIntervalFrames.setEnabled(false);
    }
    
    /** Called when clicking into an interval-frame title bar. */
    void setFrameSelected(IntervalListFrame intervalListFrame) {
        for (IntervalListFrame frame : getIntervalListFrames())
            frame.setTitleBarSelected(frame == intervalListFrame);
    }

    private List<IntervalListFrame> getIntervalListFrames() {
        final List<IntervalListFrame> list = new ArrayList<>();
        for (Component c : intervalListsPanel.getComponents())
            if (c instanceof IntervalListFrame) // there is also a vertical strut in it
                list.add((IntervalListFrame) c);
        return list;
    }
    
    /** Called when an interval is clicked. */
    void intervalSelected(IntervalListFrame activeFrame, DifferenceToneInversions.TonePair pair, boolean mouseDown) {
        
        // no other frame should show a selection
        for (IntervalListFrame frame : getIntervalListFrames())
            if (activeFrame != frame)
                frame.clearListSelection();
        
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

    private void ignoreMouseEvents(boolean ignore) {
        final DifferenceToneInversionsMouseHandler mouseHandler = (DifferenceToneInversionsMouseHandler) getMouseHandler();
        mouseHandler.setActive(ignore == false);
        
        for (PianoWithSound.Keyboard.Key key : getKeys())
            key.setIgnoreMouse(ignore == true);
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
            
            final DefaultListModel<DifferenceToneInversions.TonePair> model = new DefaultListModel<>();
            for (DifferenceToneInversions.TonePair tonePair : intervals)
                model.addElement(tonePair);
            list.setModel(model);
            
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
            
            final JLabel frameTitle = new JLabel(ipnNoteName, JLabel.CENTER);
            frameTitle.setToolTipText("Intervals Generating "+ipnNoteName);
            frameTitle.addMouseListener(frameSelectionListener);
            
            this.frameTitleBar = new JPanel(new BorderLayout());
            frameTitleBar.add(frameTitle, BorderLayout.CENTER);
            frameTitleBar.add(frameCloseButton, BorderLayout.EAST);
            
            add(frameTitleBar, BorderLayout.NORTH);
            add(new JScrollPane(list), BorderLayout.CENTER);
            
            setBorder(BorderFactory.createLineBorder(Color.GRAY, 1, true));
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
                
                listLine.add(Box.createRigidArea(new Dimension(12, 4)), BorderLayout.CENTER);
                
                final Dimension size = new Dimension(30, -1);
                forceSize(lowerNoteName, size);
                forceSize(upperNoteName, size);
                final JLabel separator = new JLabel("-", JLabel.CENTER);
                forceSize(separator, size);
                
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
                lowerNoteName.setText(tonePair.lowerTone().ipnName);
                upperNoteName.setText(tonePair.upperTone().ipnName);
                
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
            
            private void forceSize(Component c, Dimension size) {
                c.setPreferredSize(size);
                c.setMinimumSize(size);
                c.setMaximumSize(size);
            }
        }
    }
}