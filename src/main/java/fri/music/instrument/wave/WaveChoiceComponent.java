package fri.music.instrument.wave;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.BorderFactory;
import javax.swing.JComboBox;
import fri.music.swingutils.SmartComboBox;
import fri.music.wavegenerator.GenericWaveSoundChannel;
import fri.music.wavegenerator.WaveGenerator;
import fri.music.wavegenerator.WaveNames;

public class WaveChoiceComponent
{
    public final JComboBox<String> choice;
    
    public WaveChoiceComponent(final GenericWaveSoundChannel soundChannel) {
        this.choice = new SmartComboBox(WaveNames.getNames());
        choice.setBorder(BorderFactory.createTitledBorder("Wave Form"));
        choice.setToolTipText("Choose Type of Sound");
        
        choice.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                final String waveName = (String) choice.getSelectedItem();
                final Class<? extends WaveGenerator> waveClass = WaveNames.getClass(waveName);
                soundChannel.setGeneratorClass(waveClass);
            }
        });
    }
}