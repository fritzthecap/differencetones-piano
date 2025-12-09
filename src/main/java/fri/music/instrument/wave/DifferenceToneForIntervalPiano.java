package fri.music.instrument.wave;

import java.awt.Color;
import java.awt.Component;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JSlider;
import javax.swing.border.Border;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import fri.music.Tone;
import fri.music.ToneSystem;
import fri.music.differencetones.DifferenceTones;
import fri.music.instrument.PianoWithSound;
import fri.music.utils.swing.layout.ToolBarUtil;
import fri.music.utils.swing.text.HelpWindowSingleton;
import fri.music.utils.swing.window.Notification;
import fri.music.wavegenerator.WaveSoundChannel;

/**
 * Piano that displays difference-tones for two high notes sounding together.
 * The "Difference Tone" state is rendered by a red border combined with visual selection.
 */
public class DifferenceToneForIntervalPiano extends IntervalPlayingPiano
{
    private JComponent pianoPanel;
    private DeviationComponent deviationComponent;
    
    public DifferenceToneForIntervalPiano(PianoWithSound.Configuration config, WaveSoundChannel soundChannel) {
        super(config, soundChannel);
    }
    
    @Override
    public JComponent getKeyboard() {
        if (this.pianoPanel != null)
            return this.pianoPanel;

        final JComponent pianoPanel = super.getKeyboard();
        
        this.deviationComponent = new DeviationComponent(DifferenceTones.DEFAULT_DEVIATION, config.isVertical);
        final JSlider deviationSlider = deviationComponent.deviationSlider;
        getControlPanel().add(deviationSlider, 4); // add after "Tuning" choice
        deviationSlider.addChangeListener(new ChangeListener() {
            /** Must change playing keys according to deviation. */
            @Override
            public void stateChanged(ChangeEvent e) {
                ((DifferenceToneMouseHandler) getMouseHandler()).reviseDifferenceTone();
            }
        });
        
        final JButton help = new JButton("Help");
        help.setToolTipText("Description of Difference-Tone Displaying Piano");
        help.addActionListener(event -> HelpWindowSingleton.start(pianoPanel, "Difference-Tone Displaying Piano", HelpForDifferenceToneForIntervalsPiano.URL));
        getControlPanel().add(ToolBarUtil.getHelpButtonLookWrapper(help));
        
        return this.pianoPanel = pianoPanel;
    }
    
    /** Overridden to alter the tuning while intervals may be playing. */
    @Override
    protected WaveTuningComponent newTuningComponent() {
        final WaveTuningComponent.Listener tuningChangeListener = new WaveTuningComponent.Listener() {
            @Override
            public void tuningChanged(ToneSystem toneSystem) {
                ((DifferenceToneMouseHandler) getMouseHandler()).reviseDifferenceTone();
            }
        };
        return new WaveTuningComponent(
                config.lowestToneIpnName, 
                config.octaves, 
                getWaveSoundChannel(), 
                tuningChangeListener);
    }
    
    public WaveSoundChannel getWaveSoundChannel() {
        return (WaveSoundChannel) getSoundChannel();
    }

    public double getDeviation() {
        return deviationComponent.getDeviation();
    }
    

    /** Overridden to return a DifferenceToneMouseHandler. */
    @Override
    protected MouseHandler newMouseHandler() {
        return new DifferenceToneMouseHandler(this);
    }
    
    
    /**
     * Mouse handler that marks difference-tones.
     * CAUTION: this class and the DifferenceToneMouseHandler of DifferenceToneForNotesPiano are similar!
     */
    private static class DifferenceToneMouseHandler extends AdditionalTonesPlayingMouseHandler
    {
        private final Border differenceToneBorder;
        private Border originalBorder;
        private Keyboard.Key selectedDifferenceKey;
        private ToneNotification notification;
        
        public DifferenceToneMouseHandler(PianoWithSound piano) {
            super(piano);
            
            if (piano.getSoundChannel() instanceof WaveSoundChannel == false)
                throw new IllegalArgumentException("Can run only with a WaveSoundChannel!");
                
            this.differenceToneBorder = BorderFactory.createLineBorder(Color.RED, SELECTION_BORDER_THICKNESS);
        }

        /** Changes pressed and held keys "on the fly" when tuning changes. */
        public void reviseDifferenceTone() {
            handleDifferenceKey();
        }
        
        @Override
        public void close() {
            super.close();
            if (notification != null) {
                waveSoundChannel().setNoteListener(null);
                notification.close();
                notification = null;
            }
        }
        
        @Override
        protected void reset() {
            super.reset();
            
            if (selectedDifferenceKey != null)
                selectDifferenceTone(selectedDifferenceKey, false);
        }
        
        @Override
        protected void noteOn(Keyboard.Key key) {
            ensureNotesListener();
            super.noteOn(key);
            handleDifferenceKey();
        }

        @Override
        protected void noteOff(Keyboard.Key key) {
            super.noteOff(key);
            handleDifferenceKey();
        }
        
        private void ensureNotesListener() {
            if (notification == null) { // can not do this in constructor as keyboardPanel is not yet built there
                notification = new ToneNotification(piano.getKeyboardPanel());
                waveSoundChannel().setNoteListener(new WaveSoundChannel.NoteListener() {
                    @Override
                    public void noteOn(Tone tone) {
                        notification.addLine(tone);
                    }
                    @Override
                    public void noteOff(Tone tone) {
                        notification.removeLine(tone);
                    }
                    @Override
                    public void allNotesOff() {
                        notification.removeAll();
                    }
                });
            }
        }

        private WaveSoundChannel waveSoundChannel() {
            return (WaveSoundChannel) soundChannel();
        }
        
        private void handleDifferenceKey() {
            final Keyboard.Key[] twoPlayingKeys = getPlayingKeyTuple();
            
            if (twoPlayingKeys != null) { // try to calculate difference tone of the two
                if (selectedDifferenceKey != null) // remove currently selected
                    selectDifferenceTone(selectedDifferenceKey, false);
                
                final Keyboard.Key differenceToneKey = DifferenceToneUtil.getDifferenceToneKey(
                        waveSoundChannel().getTones(), 
                        ((DifferenceToneForIntervalPiano) piano).getDeviation(),
                        piano,
                        twoPlayingKeys[0].midiNoteNumber,
                        twoPlayingKeys[1].midiNoteNumber);
                
                if (differenceToneKey != null) {
                    selectedDifferenceKey = differenceToneKey;
                    selectDifferenceTone(selectedDifferenceKey, true); // select and add red border
                }
            }
            else if (selectedDifferenceKey != null) {
                if (holdPlayingNotes.contains(selectedDifferenceKey) == false) // selectedDifferenceKey is not on "Hold"
                    selectDifferenceTone(selectedDifferenceKey, false); // de-select and remove red border
                
                selectedDifferenceKey = null;
            }
        }
        
        private Keyboard.Key[] getPlayingKeyTuple() {
            if (keyToAdditional.size() == 1) {
                final Keyboard.Key key1 = keyToAdditional.keySet().iterator().next();
                final Keyboard.Key key2 = (Keyboard.Key) keyToAdditional.get(key1);
                return new Keyboard.Key[] { key1, key2 };
            }
            else if (holdPlayingNotes.size() == 2) {
                final Iterator<Keyboard.Key> iterator = holdPlayingNotes.iterator();
                final Keyboard.Key key1 = iterator.next();
                final Keyboard.Key key2 = iterator.next();
                return new Keyboard.Key[] { key1, key2 };
            }
            return null;
        }
        
        private void selectDifferenceTone(Keyboard.Key key, boolean select) {
            if (originalBorder == null) // save original border for reset
                originalBorder = key.getBorder();
            key.setBorder(select ? differenceToneBorder : originalBorder);
        }
        
        
        private static class ToneNotification extends Notification
        {
            private final List<Tone> tones = new ArrayList<>();
            
            ToneNotification(Component parent) {
                super(parent);
            }
            
            @Override
            protected String getTooltipText() {
                return "List of Actually Pressed Piano Keys. "+super.getTooltipText();
            }
            
            void addLine(Tone tone) {
                if (tones.contains(tone))
                    return; // do not duplicate tones that are just once on the piano
                tones.add(tone);
                tones.sort((t1, t2) -> t2.midiNumber - t1.midiNumber);
                setLines(toLines());
            }
            void removeLine(Tone tone) {
                if (tones.remove(tone))
                    setLines(toLines());
            }
            void removeAll() {
                tones.clear();
                setLines(toLines());
            }
            
            private List<String> toLines() {
                final List<String> lines = new ArrayList<>();
                for (Tone tone : tones)
                    lines.add(tone.ipnName);
                return lines;
            }
        }   // end class ToneNotification
    }   // end class DifferenceToneMouseHandler
}