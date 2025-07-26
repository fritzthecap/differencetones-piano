package fri.music.instrument.wave;

import java.awt.Color;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.SequencedSet;
import javax.swing.BorderFactory;
import javax.swing.JComponent;
import javax.swing.border.Border;
import fri.music.differencetones.DifferenceTones;
import fri.music.instrument.PianoWithSound;
import fri.music.instrument.PianoWithVolume;
import fri.music.wavegenerator.WaveSoundChannel;

/**
 * Piano that displays difference-tones for two high notes sounding together.
 */
public class DifferenceToneForNotesPiano extends PianoWithVolume
{
    private JComponent pianoPanel;
    private TuningComponent tuningComponent;
    private DeviationComponent deviationComponent;
    
    public DifferenceToneForNotesPiano(PianoWithSound.Configuration config, WaveSoundChannel soundChannel) {
        super(config, soundChannel);
    }
    
    @Override
    public JComponent getKeyboard() {
        if (this.pianoPanel != null)
            return this.pianoPanel;

        final JComponent pianoPanel = super.getKeyboard();
        
        this.tuningComponent = new TuningComponent(
                config.lowestToneIpnName, 
                config.octaves, 
                getWaveSoundChannel());
        getControlPanel().add(tuningComponent.getChoice(null), 1);
        
        this.deviationComponent = new DeviationComponent(DifferenceTones.DEFAULT_DEVIATION, config.isVertical);
        getControlPanel().add(deviationComponent.getSlider(), 2);
        
        return this.pianoPanel = pianoPanel;
    }
    
    /** @return the used WaveSoundChannel, holding tones of a tone-system. */
    public WaveSoundChannel getWaveSoundChannel() {
        return (WaveSoundChannel) getSoundChannel();
    }

    public String getSelectedTuning() {
        return (String) tuningComponent.getChoice(null).getSelectedItem();
    }
    
    public void setTuningControlsEnabled(boolean enable) {
        deviationComponent.getSlider().setEnabled(enable);
        tuningComponent.getChoice(null).setEnabled(enable);
    }
    
    /** @return the current deviation value from slider as floating-point number 0..1. */
    public double getDeviation() {
        return deviationComponent.getDeviation();
    }
    

    /** Overridden to return a DifferenceToneMouseHandler. */
    @Override
    protected MouseHandler newMouseHandler() {
        return new DifferenceToneMouseHandler(this, getWaveSoundChannel());
    }
    
    
    protected static class DifferenceToneMouseHandler extends MouseHandler
    {
        private final WaveSoundChannel soundChannel;
        private final SequencedSet<Keyboard.Key> playingNotes = new LinkedHashSet<>();
        private final Border redBorder;
        private Border originalBorder;
        private Keyboard.Key selectedDifferenceKey;
        
        public DifferenceToneMouseHandler(PianoWithSound piano, WaveSoundChannel soundChannel) {
            super(piano);
            this.soundChannel = soundChannel;
            this.redBorder = BorderFactory.createLineBorder(Color.RED, 2);
        }
        
        @Override
        protected void noteOn(Keyboard.Key key) {
            super.noteOn(key);
            playingNotes.add(key);
            handleDifferenceKey();
        }

        @Override
        protected void noteOff(Keyboard.Key key) {
            super.noteOff(key);
            playingNotes.remove(key);
            handleDifferenceKey();
        }
        
        private void handleDifferenceKey() {
            final Keyboard.Key[] twoPlayingKeys = getPlayingKeyTuple();
            
            if (twoPlayingKeys != null) { // try to calculate difference tone of the two
                if (selectedDifferenceKey != null) // remove currently selected
                    selectDifferenceTone(selectedDifferenceKey, false);
                
                final Keyboard.Key differenceToneKey = DifferenceToneUtil.getDifferenceToneKey(
                        soundChannel.getTones(), 
                        ((DifferenceToneForNotesPiano) piano).getDeviation(),
                        piano,
                        twoPlayingKeys[0].midiNoteNumber,
                        twoPlayingKeys[1].midiNoteNumber);
                
                if (differenceToneKey != null) {
                    selectedDifferenceKey = differenceToneKey;
                    selectDifferenceTone(selectedDifferenceKey, true); // select and add red border
                }
            }
            else if (selectedDifferenceKey != null) {
                selectDifferenceTone(selectedDifferenceKey, false); // de-select and remove red border
                selectedDifferenceKey = null;
            }
        }
        
        private Keyboard.Key[] getPlayingKeyTuple() {
            if (playingNotes.size() == 2) {
                final Iterator<Keyboard.Key> iterator = playingNotes.iterator();
                final Keyboard.Key key1 = iterator.next();
                final Keyboard.Key key2 = iterator.next();
                return new Keyboard.Key[] { key1, key2 };
            }
            return null;
        }
        
        private void selectDifferenceTone(Keyboard.Key key, boolean select) {
            setRedBorder(key, select);
            //visualSelect(key, select);
            key.paintImmediately(key.getVisibleRect());
        }
        
        private void setRedBorder(Keyboard.Key key, boolean select) {
            if (originalBorder == null) // save original border for reset
                originalBorder = key.getBorder();
            key.setBorder(select ? redBorder : originalBorder);
        }
    }
}