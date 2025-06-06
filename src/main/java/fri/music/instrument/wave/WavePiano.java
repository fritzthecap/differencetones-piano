package fri.music.instrument.wave;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.BorderFactory;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import fri.music.Tone;
import fri.music.instrument.PianoWithHold;
import fri.music.instrument.PianoWithSound;
import fri.music.swingutils.SmartComboBox;
import fri.music.wavegenerator.GenericWaveSoundChannel;
import fri.music.wavegenerator.SineWaveGenerator;
import fri.music.wavegenerator.WaveGenerator;
import fri.music.wavegenerator.WaveNames;

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
        
        final JComboBox<String> waveChoice = new SmartComboBox(WaveNames.getNames());
        waveChoice.setBorder(BorderFactory.createTitledBorder("Wave Form"));
        waveChoice.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                final String waveName = (String) waveChoice.getSelectedItem();
                final Class<? extends WaveGenerator> waveClass = WaveNames.getClass(waveName);
                ((GenericWaveSoundChannel) getSoundChannel()).setGeneratorClass(waveClass);
            }
        });
        
        getControlPanel().add(waveChoice, 2);
        
        return this.pianoPanel = pianoPanel;
    }
}