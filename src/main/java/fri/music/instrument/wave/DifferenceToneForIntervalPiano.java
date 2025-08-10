package fri.music.instrument.wave;

import java.util.Iterator;
import javax.swing.JComponent;
import javax.swing.JSlider;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import fri.music.ToneSystem;
import fri.music.differencetones.DifferenceTones;
import fri.music.instrument.PianoWithSound;
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
        final JSlider deviationSlider = deviationComponent.getSlider();
        getControlPanel().add(deviationSlider, 4); // add after "Tuning" choice
        deviationSlider.addChangeListener(new ChangeListener() {
            @Override
            public void stateChanged(ChangeEvent e) {
                ((DifferenceToneMouseHandler) getMouseHandler()).reviseDifferenceTone();
            }
        });
        
        return this.pianoPanel = pianoPanel;
    }
    
    /** Overridden to alter the tuning while intervals may be playing. */
    @Override
    protected TuningComponent newTuningComponent() {
        final TuningComponent.Listener tuningChangeListener = new TuningComponent.Listener() {
            @Override
            public void tuningChanged(ToneSystem toneSystem) {
                ((DifferenceToneMouseHandler) getMouseHandler()).reviseDifferenceTone();
            }
        };
        return new TuningComponent(
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
        return new DifferenceToneMouseHandler(this, getWaveSoundChannel());
    }
    
    
    protected static class DifferenceToneMouseHandler extends AdditionalTonesPlayingMouseHandler
    {
        private final WaveSoundChannel soundChannel;
        private Keyboard.Key selectedDifferenceKey;
        
        public DifferenceToneMouseHandler(PianoWithSound piano, WaveSoundChannel soundChannel) {
            super(piano);
            this.soundChannel = soundChannel;
        }
        
        /** Changes pressed and held keys "on the fly" when tuning changes. */
        public void reviseDifferenceTone() {
            handleDifferenceKey();
        }

        @Override
        protected void reset() {
            super.reset();
            
            if (selectedDifferenceKey != null)
                selectDifferenceTone(selectedDifferenceKey, false);
        }
        
        @Override
        protected void noteOn(Keyboard.Key key) {
            super.noteOn(key);
            handleDifferenceKey();
        }

        @Override
        protected void noteOff(Keyboard.Key key) {
            super.noteOff(key);
            handleDifferenceKey();
        }
        
        private void handleDifferenceKey() {
            final Keyboard.Key[] twoPlayingKeys = getPlayingKeyTuple();
            
            if (twoPlayingKeys != null) { // try to calculate difference tone of the two
                if (selectedDifferenceKey != null) // remove currently selected
                    selectDifferenceTone(selectedDifferenceKey, false);
                
                final Keyboard.Key differenceToneKey = DifferenceToneUtil.getDifferenceToneKey(
                        soundChannel.getTones(), 
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
                else
                    visualSelect(selectedDifferenceKey, false); // de-select, but leave the red border
                
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
            setRedBorder(key, select);
            visualSelect(key, select);
        }
    }
}