package fri.music.instrument.wave;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.BorderFactory;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import fri.music.Tone;
import fri.music.instrument.PianoWithHold;
import fri.music.instrument.PianoWithSound;
import fri.music.instrument.SmartComboBox;
import fri.music.wavegenerator.GenericWaveSoundChannel;
import fri.music.wavegenerator.SawtoothWaveGenerator;
import fri.music.wavegenerator.SineWaveGenerator;
import fri.music.wavegenerator.SquareWaveGenerator;
import fri.music.wavegenerator.TriangleWaveGenerator;
import fri.music.wavegenerator.WaveGenerator;

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
        
        final JComboBox<String> waveChoice = new SmartComboBox(getWaveGeneratorClassnames());
        waveChoice.setBorder(BorderFactory.createTitledBorder("Wavetype"));
        waveChoice.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                final String simpleName = (String) waveChoice.getSelectedItem();
                
                final String waveGenClassName = SineWaveGenerator.class.getName();
                final String packageName = waveGenClassName.substring(0, waveGenClassName.lastIndexOf("."));
                try {
                    @SuppressWarnings("unchecked")
                    Class<? extends WaveGenerator> generatorClass = (Class<? extends WaveGenerator>) 
                        Class.forName(packageName+"."+simpleName+"WaveGenerator");
                    
                    ((GenericWaveSoundChannel) getSoundChannel()).setGeneratorClass(generatorClass);
                }
                catch (ClassNotFoundException ex) {
                    throw new RuntimeException(ex);
                }
            }
        });
        
        getControlPanel().add(waveChoice, 2);
        
        return this.pianoPanel = pianoPanel;
    }

    private String[] getWaveGeneratorClassnames() {
        return new String[] {
            SineWaveGenerator.class.getSimpleName().substring(0, 4),
            TriangleWaveGenerator.class.getSimpleName().substring(0, 8),
            SawtoothWaveGenerator.class.getSimpleName().substring(0, 8),
            SquareWaveGenerator.class.getSimpleName().substring(0, 6),
        };
    }
}