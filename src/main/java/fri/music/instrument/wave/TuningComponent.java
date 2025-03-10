package fri.music.instrument.wave;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import javax.swing.BorderFactory;
import javax.swing.JComboBox;
import fri.music.EqualTemperament;
import fri.music.JustIntonation;
import fri.music.ToneSystem;
import fri.music.instrument.SmartComboBox;
import fri.music.wavegenerator.WaveSoundChannel;

/**
 * Lets change tunings on a wave-soundchannel.
 */
public class TuningComponent
{
    public interface Listener
    {
        void tuningChanged(ToneSystem toneSystem);
    }
    
    private final String lowestToneIpnName;
    private final int octaves;
    private final WaveSoundChannel soundChannel;
    private final Listener listener;
    
    private JComboBox<String> toneSystemChoice;
    
    public TuningComponent(String lowestToneIpnName, int octaves, WaveSoundChannel soundChannel) {
        this(lowestToneIpnName, octaves, soundChannel, null);
    }
    public TuningComponent(String lowestToneIpnName, int octaves, WaveSoundChannel soundChannel, Listener listener) {
        if (soundChannel == null && listener == null)
            throw new IllegalArgumentException("Need either sound-channel or tuning-listener!");
        
        this.lowestToneIpnName = lowestToneIpnName;
        this.octaves = octaves;
        this.soundChannel = soundChannel;
        this.listener = listener;
    }
    
    public JComboBox<String> getToneSystemChoice(ToneSystem initialToneSystem) {
        if (this.toneSystemChoice != null)
            return this.toneSystemChoice;

        final String[] tonesystemNames = getTonesystemNames();
        final JComboBox<String> toneSystemChoice = new SmartComboBox(tonesystemNames);
        toneSystemChoice.setBorder(BorderFactory.createTitledBorder("Tuning"));
        
        if (initialToneSystem != null)
            toneSystemChoice.setSelectedIndex(getSelectedIndex(initialToneSystem, tonesystemNames));
        
        toneSystemChoice.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                final String name = (String) toneSystemChoice.getSelectedItem();
                final ToneSystem toneSystem = toToneSystem(name);
                
                if (soundChannel != null)
                    soundChannel.setTones(toneSystem.tones());
                
                if (listener != null)
                    listener.tuningChanged(toneSystem);
            }
        });
        
        return this.toneSystemChoice = toneSystemChoice;
    }

    private String[] getTonesystemNames() {
        final String SPACE = " ";
        final List<String> scales = new ArrayList<>();
        
        scales.add(EqualTemperament.class.getSimpleName()+" (12-TET, EDO-12)"); // default tone-system on top
        scales.addAll(
            Stream.of(JustIntonation.ChromaticScales.values())
                .map(scale -> 
                    scale.name()+SPACE+
                    (scale.name().startsWith("HARMONIC")
                        ? "(Overtone Scale)"
                        : "("+JustIntonation.class.getSimpleName()+")"))
                .toList());
        
        return scales.toArray(new String[scales.size()]);
    }
    
    private int getSelectedIndex(ToneSystem initialToneSystem, String[] tonesystemNames) {
        final String namePartToSearch = (initialToneSystem instanceof JustIntonation)
            ? ((JustIntonation) initialToneSystem).chromaticScale.name()
            : initialToneSystem.getClass().getSimpleName(); // EqualTemperament
        
        return IntStream.range(0, tonesystemNames.length)
            .filter(index -> tonesystemNames[index].contains(namePartToSearch))
            .findFirst()
            .orElseThrow();
    }
    
    private ToneSystem toToneSystem(String name) {
        if (name.startsWith(EqualTemperament.class.getSimpleName()))
            return new EqualTemperament(lowestToneIpnName, octaves);
        
        final JustIntonation.ChromaticScale twelveToneScale = Stream.of(JustIntonation.ChromaticScales.values())
                .filter(scale -> name.startsWith(scale.name()))
                .findFirst()
                .orElseThrow();
        
        return new JustIntonation(lowestToneIpnName, octaves, twelveToneScale);
    }
}