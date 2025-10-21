package fri.music.instrument.configuration;

import java.awt.FlowLayout;
import javax.swing.BorderFactory;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JPanel;
import fri.music.ToneSystem;
import fri.music.instrument.TuningComponent;
import fri.music.utils.swing.layout.SmartComboBox;

/**
 * A panel that lets configure all options for
 * ToneSystem implementations.
 */
public class ToneSystemConfigurationPanel extends ToneRangeConfigurationPanel
{
    private TuningComponent tuningComponent;
    private FrequencyOfA4Component frequencyOfA4;
    private JComboBox<String> modalScaleStartBaseName;
    
    /** Default constructor for "C4" on 440 Hz with 1 octave. */
    public ToneSystemConfigurationPanel() {
        this(1, "C", 4, 0.0, null);
    }
    
    /**
     * All parameters are optional and will be replaced by defaults when null or out of range.
     * @param octaves number of octaves of the tone-system to construct.
     * @param lowestToneBaseName the base-name (without octave) of the lowest tone, 
     *      its frequency will be taken from an equal-temperament 12-tone scale.
     * @param lowestToneOctave the octave of the lowest tone.
     * @param frequencyOfA4Param the desired frequency of the reference-tone A4.
     * @param modalScaleStartBaseNameParam the desired lowest tone of the resulting tone-system,
     *      calculated from lowestToneBaseName through interval fractions.
     */
    public ToneSystemConfigurationPanel(
            int octaves, 
            String lowestToneBaseName, 
            int lowestToneOctave,
            double frequencyOfA4Param,
            String modalScaleStartBaseNameParam)
    {
        super(octaves, lowestToneBaseName, lowestToneOctave, false); // false: no scale names at "Lowest Tone"
        
        // field construction
        buildToneSystemConfigurationFields(lowestToneBaseName, frequencyOfA4Param, octaves);
        
        if (modalScaleStartBaseNameParam != null)
            modalScaleStartBaseName.setSelectedItem(modalScaleStartBaseNameParam);

        // field layout
        final JComponent tuningsPanel = tuningComponent.getLeftAlignedChoice(); // null: default tuning
        // set tunings choice on top
        panel.add(tuningsPanel, 0); // null: default tuning
        
        panel.add(frequencyOfA4.frequencySlider);
        
        final JPanel modalScaleStartTonePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        modalScaleStartTonePanel.add(modalScaleStartBaseName);
        modalScaleStartTonePanel.add(buildScaleNameChoice(modalScaleStartBaseName));
        
        modalScaleStartTonePanel.setBorder(BorderFactory.createTitledBorder("Modal Scale Start Tone"));
        panel.add(modalScaleStartTonePanel);
    }
    
    public final double getFrequencyOfA4() {
        return frequencyOfA4.getValue();
    }
    
    public final String getModalScaleStartIpnName() {
        final String modalStartBaseName = (String) modalScaleStartBaseName.getSelectedItem();
        return modalStartBaseName + getLowestToneOctave();
    }
    
    public final ToneSystem getToneSystem() {
        tuningComponent.setLowestToneIpnName(getLowestToneIpnName());
        tuningComponent.setOctaves(getOctaves());
        tuningComponent.setFrequencyOfA4(getFrequencyOfA4());
        tuningComponent.setModalScaleStartIpnName(getModalScaleStartIpnName());
        return tuningComponent.getTuning();
    }
    

    private void buildToneSystemConfigurationFields(
            String lowestToneBaseNameParam, 
            double frequencyOfA4Param, 
            int octavesParam)
    {
        this.frequencyOfA4 = new FrequencyOfA4Component(frequencyOfA4Param);
        
        this.modalScaleStartBaseName = new SmartComboBox(getToneBaseNames());
        modalScaleStartBaseName.setBorder(BorderFactory.createTitledBorder("Key"));
        modalScaleStartBaseName.setToolTipText("Start Tone of the Modal Scale to Build");
        
        final String lowestToneIpnName;
        if (lowestToneBaseNameParam != null) {
            modalScaleStartBaseName.setSelectedItem(lowestToneBaseNameParam);
            lowestToneIpnName = lowestToneBaseNameParam + getLowestToneOctave();
        }
        else {
            lowestToneIpnName = ToneSystem.DEFAULT_BASETONE_IPN_NAME;
        }
        
        this.tuningComponent = new TuningComponent(lowestToneIpnName, octavesParam);
    }
}