package fri.music.instrument.wave;

import javax.swing.JComboBox;
import javax.swing.JComponent;
import fri.music.Tone;
import fri.music.instrument.PianoWithHold;
import fri.music.instrument.PianoWithSound;
import fri.music.wavegenerator.GenericWaveSoundChannel;
import fri.music.wavegenerator.SineWaveGenerator;

/**
 * Piano that can play different wave-forms like sine, sawtooth and square.
 */
public class WavePiano extends PianoWithHold
{
    private JComponent pianoPanel;
    
    public WavePiano(PianoWithSound.Configuration config, Tone[] tones) {
        super(config, new GenericWaveSoundChannel(tones, SineWaveGenerator.class));
    }
    
    @Override
    public JComponent getKeyboard() {
        if (this.pianoPanel != null)
            return this.pianoPanel;

        final JComponent pianoPanel = super.getKeyboard();
        
        final JComboBox<String> waveChoice = 
                new WaveChoiceComponent((GenericWaveSoundChannel) getSoundChannel()).choice;
        
        getControlPanel().add(waveChoice, 2);
        
        return this.pianoPanel = pianoPanel;
    }
}