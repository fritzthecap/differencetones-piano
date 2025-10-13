package fri.music.instrument;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import javax.swing.BorderFactory;
import javax.swing.JComboBox;
import fri.music.EqualTemperament;
import fri.music.JustIntonation;
import fri.music.ToneSystem;
import fri.music.utils.swing.layout.SmartComboBox;

/**
 * Lets choose tunings.
 */
public class TuningComponent
{
    private String lowestToneIpnName;
    private int octaves;
    private double frequencyOfA4;
    private String modalScaleStartIpnName;
    
    private JComboBox<String> tuningChoice;
    
    public TuningComponent(String lowestToneIpnName, int octaves) {
        setLowestToneIpnName(lowestToneIpnName);
        setOctaves(octaves);
        setFrequencyOfA4(ToneSystem.DEFAULT_REFERENCE_FREQUENCY);
        setModalScaleStartIpnName(lowestToneIpnName);
    }
    
    public final void setLowestToneIpnName(String lowestToneIpnName) {
        this.lowestToneIpnName = (lowestToneIpnName != null) ? lowestToneIpnName : ToneSystem.DEFAULT_BASETONE_IPN_NAME;
    }
    public final void setOctaves(int octaves) {
        this.octaves = (octaves > 0 && octaves <= ToneSystem.MAXIMUM_OCTAVES) ? octaves : ToneSystem.MAXIMUM_OCTAVES;
    }
    public final void setFrequencyOfA4(double frequencyOfA4) {
        this.frequencyOfA4 = frequencyOfA4;
    }
    public final void setModalScaleStartIpnName(String modalScaleStartIpnName) {
        this.modalScaleStartIpnName = (modalScaleStartIpnName != null) ? modalScaleStartIpnName : ToneSystem.DEFAULT_BASETONE_IPN_NAME;
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
        
        addListeners(this.tuningChoice = tuningChoice, initialTuning != null);
        
        return this.tuningChoice;
    }

    /** Called at end of getChoice(), does nothing, to be overridden by selection listeners. */
    protected void addListeners(final JComboBox<String> tuningChoice, boolean initialTuningWasSet) {
    }

    /** @return the currently chosen tone-system. */
    public final ToneSystem getTuning() {
        final String chosenName = (String) tuningChoice.getSelectedItem();
        if (chosenName.startsWith(EqualTemperament.class.getSimpleName()))
            return new EqualTemperament(
                    frequencyOfA4, 
                    lowestToneIpnName, 
                    octaves);
        
        final JustIntonation.ChromaticScale twelveToneScale = 
            Stream.of(JustIntonation.ChromaticScales.values())
                .filter(scale -> matchTuning(chosenName, scale.name()))
                .findFirst()
                .orElseThrow();
        
        return new JustIntonation(
                frequencyOfA4,
                lowestToneIpnName, 
                modalScaleStartIpnName, 
                octaves, 
                twelveToneScale);
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