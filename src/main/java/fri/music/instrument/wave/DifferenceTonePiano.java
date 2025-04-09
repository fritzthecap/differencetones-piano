package fri.music.instrument.wave;

import java.util.Iterator;
import java.util.List;
import javax.swing.BorderFactory;
import javax.swing.JComponent;
import javax.swing.JSlider;
import javax.swing.SwingConstants;
import javax.swing.border.TitledBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import fri.music.Tone;
import fri.music.ToneSystem;
import fri.music.differencetones.DifferenceTones;
import fri.music.instrument.PianoWithSound;
import fri.music.wavegenerator.WaveSoundChannel;

/**
 * Piano that displays difference-tones for two high notes sounding together.
 */
public class DifferenceTonePiano extends IntervalPlayingPiano
{
    private JComponent pianoPanel;
    private double deviation;
    
    public DifferenceTonePiano(PianoWithSound.Configuration config, WaveSoundChannel soundChannel) {
        super(config, soundChannel);
    }
    
    @Override
    public JComponent getKeyboard() {
        if (this.pianoPanel != null)
            return this.pianoPanel;

        final JComponent pianoPanel = super.getKeyboard();
        
        final int defaultDeviationPercent = (int) // 74 works for major second in EDO-12
                Math.round(DifferenceTones.TOLERANT_DEVIATION_EDO_12 * 2.0 * 100.0);
        this.deviation = deviationPercentToDouble(defaultDeviationPercent);
        
        final JSlider deviationSlider = buildDeviationSlider(defaultDeviationPercent);
        getControlPanel().add(deviationSlider, 4);
        
        return this.pianoPanel = pianoPanel;
    }
    
    @Override
    protected TuningComponent newTuningComponent() {
        final TuningComponent.Listener tuningChangeListener = new TuningComponent.Listener() {
            @Override
            public void tuningChanged(ToneSystem toneSystem) {
                ((DifferenceToneMouseHandler) getMouseHandler()).reviseDifferenceTone();
            }
        };
        return new TuningComponent(config.lowestToneIpnName, config.octaves, (WaveSoundChannel) getSoundChannel(), tuningChangeListener);
    }
    
    private WaveSoundChannel getWaveSoundChannel() {
        return (WaveSoundChannel) getSoundChannel();
    }

    private JSlider buildDeviationSlider(int defaultDeviationPercent) {
        final String title = "Deviation Tolerance Percent: ";
        final String tooltip = "Allowed deviation for finding difference-tones, 100 being the middle between two tones";
        
        final JSlider deviationSlider = new JSlider(0, 90, defaultDeviationPercent); // min, max, current
        deviationSlider.setBorder(BorderFactory.createTitledBorder(title+defaultDeviationPercent));
        deviationSlider.setToolTipText(tooltip);
        deviationSlider.setOrientation(config.isVertical ? SwingConstants.VERTICAL : SwingConstants.HORIZONTAL);
        deviationSlider.setPaintLabels(true);
        deviationSlider.setMajorTickSpacing(10);
        deviationSlider.addChangeListener(new ChangeListener() {
            @Override
            public void stateChanged(ChangeEvent e) {
                final int deviationPercent = deviationSlider.getValue();
                DifferenceTonePiano.this.deviation = deviationPercentToDouble(deviationPercent);
                ((TitledBorder) deviationSlider.getBorder()).setTitle(title+deviationPercent);
                ((DifferenceToneMouseHandler) getMouseHandler()).reviseDifferenceTone();
            }
        });
        return deviationSlider;
    }
    
    private double deviationPercentToDouble(int percent) {
        return ((double) percent / 2.0) / 100.0;
    }
    
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
                
                final DifferenceTones toneDifferencess = new DifferenceTones(
                        soundChannel.getTones(), 
                        ((DifferenceTonePiano) piano).deviation,
                        true); // find primary difference tone only
                final Tone[] differenceTones = toneDifferencess.findDifferenceTones(
                        twoPlayingKeys[0].midiNoteNumber, 
                        twoPlayingKeys[1].midiNoteNumber);
                
                if (differenceTones[0] != null) {
                    final List<Keyboard.Key> keys = piano.getKeys();
                    final int index = findKeyboardIndex(differenceTones[0].midiNumber, keys);
                    
                    if (index >= 0 && index < keys.size()) {
                        selectedDifferenceKey = keys.get(index);
                        selectDifferenceTone(selectedDifferenceKey, true);
                    }
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