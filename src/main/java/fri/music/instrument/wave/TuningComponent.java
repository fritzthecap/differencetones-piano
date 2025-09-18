package fri.music.instrument.wave;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import javax.swing.BorderFactory;
import javax.swing.JComboBox;
import javax.swing.SwingUtilities;
import fri.music.EqualTemperament;
import fri.music.JustIntonation;
import fri.music.ToneSystem;
import fri.music.swingutils.layout.SmartComboBox;
import fri.music.wavegenerator.WaveSoundChannel;

/**
 * Lets change tunings on a wave-soundchannel, or notifies a listener.
 */
public class TuningComponent
{
    /** 
     * If no sound-channel is given to constructor, 
     * this listener will be notified on any tuning change.
     * It is possible to use both listener and sound-channel.
     */
    public interface Listener
    {
        void tuningChanged(ToneSystem toneSystem);
    }
    
    private final String lowestToneIpnName;
    private final int octaves;
    private final WaveSoundChannel soundChannel;
    private final Listener listener;
    
    private JComboBox<String> tuningChoice;
    
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
    
    /**
     * Call this to get a UI-control for choosing tunings.
     * @param initialTuning optional, the name of a tuning to select initially.
     * @return a UI-control that lets choose tunings 
     *      and sets the chosen tuning into the optional sound-channel
     *      and notifies the optional listener.
     */
    public JComboBox<String> getChoice(ToneSystem initialTuning) {
        if (this.tuningChoice != null)
            return this.tuningChoice;

        final String[] tuningNames = getTuningNames();
        final JComboBox<String> tuningChoice = new SmartComboBox(tuningNames);
        tuningChoice.setBorder(BorderFactory.createTitledBorder("Tuning"));
        
        if (initialTuning != null)
            tuningChoice.setSelectedIndex(getInitiallySelectedIndex(initialTuning, tuningNames));
        
        final ActionListener actionListener = new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                final String name = (String) tuningChoice.getSelectedItem();
                final ToneSystem toneSystem = toTuning(name);
                
                if (soundChannel != null)
                    soundChannel.setTones(toneSystem.tones());
                
                if (listener != null)
                    listener.tuningChanged(toneSystem);
            }
        };
        tuningChoice.addActionListener(actionListener);
        
        SwingUtilities.invokeLater(() -> actionListener.actionPerformed(null)); // let callers initialize first
        
        return this.tuningChoice = tuningChoice;
    }

    /** Called once when building the choice. */
    private String[] getTuningNames() {
        final String SPACE = " ";
        final List<String> scales = new ArrayList<>();
        
        // put default tone-system on top of list
        scales.add(EqualTemperament.class.getSimpleName()+SPACE+EqualTemperament.NAME_POSTFIX);
        
        // add different just-intonations below
        scales.addAll(
            Stream.of(JustIntonation.ChromaticScales.values())
                .map(scale -> 
                    scale.name()+SPACE+ // visible name must start with chromaticScale.name()
                    (scale.name().startsWith("HARMONIC")
                        ? "(Overtone Scale)"
                        : "("+JustIntonation.class.getSimpleName()+")"))
                .toList());
        
        return scales.toArray(new String[scales.size()]);
    }
    
    /** Called every time an item was selected from choice. */
    private ToneSystem toTuning(final String chosenName) {
        if (chosenName.startsWith(EqualTemperament.class.getSimpleName()))
            return new EqualTemperament(lowestToneIpnName, octaves);
        
        final JustIntonation.ChromaticScale twelveToneScale = 
            Stream.of(JustIntonation.ChromaticScales.values())
                .filter(scale -> matchTuning(chosenName, scale.name()))
                .findFirst()
                .orElseThrow();
        
        return new JustIntonation(lowestToneIpnName, octaves, twelveToneScale);
    }
    
    private int getInitiallySelectedIndex(ToneSystem initialTuning, final String[] tuningNames) {
        final String namePrefix = (initialTuning instanceof JustIntonation)
            ? ((JustIntonation) initialTuning).chromaticScale.name()
            : initialTuning.getClass().getSimpleName(); // "EqualTemperament"
        
        return IntStream.range(0, tuningNames.length)
            .filter(index -> matchTuning(tuningNames[index], namePrefix))
            .findFirst()
            .orElseThrow();
    }
    
    private boolean matchTuning(String choosableTuningName, String namePrefix) {
        return choosableTuningName.startsWith(namePrefix);
    }
}