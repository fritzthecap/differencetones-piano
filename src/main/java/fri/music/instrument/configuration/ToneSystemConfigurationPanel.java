package fri.music.instrument.configuration;

import java.awt.Color;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.Dictionary;
import java.util.Hashtable;
import java.util.List;
import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSlider;
import javax.swing.border.TitledBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import fri.music.ToneSystem;
import fri.music.instrument.TuningComponent;
import fri.music.swingutils.layout.SmartComboBox;

/**
 * A panel that lets configure all options for
 * ToneSystem implementations.
 */
public class ToneSystemConfigurationPanel extends ToneRangeConfigurationPanel
{
    private static final double A4_FREQUENCY_MINIMUM = 410.0;
    private static final double A4_FREQUENCY_MAXIMUM = 460.0;

    private JSlider frequencyOfA4;
    private JComboBox<String> modalScaleStartBaseName;
    private TuningComponent tuningComponent;
    
    private final NumberFormat decimalFormatLabels = new DecimalFormat("0");
    private final NumberFormat decimalFormatTooltip = new DecimalFormat("0.0");
    
    public ToneSystemConfigurationPanel(
            int octaves, 
            String lowestToneBaseName, 
            int lowestToneOctave,
            double frequencyOfA4Param,
            String modalScaleStartBaseNameParam)
    {
        super(octaves, lowestToneBaseName, lowestToneOctave, false); // false: no scale names at lowest tone
        
        // field construction
        buildToneSystemConfigurationFields(lowestToneBaseName, lowestToneOctave, frequencyOfA4Param, octaves);
        
        // field layout
        panel.add(frequencyOfA4);
        
        final JPanel modalScaleStartTonePanel = new JPanel();
        modalScaleStartTonePanel.setLayout(new BoxLayout(modalScaleStartTonePanel, BoxLayout.X_AXIS));
        modalScaleStartTonePanel.setBorder(BorderFactory.createTitledBorder(
                        BorderFactory.createLineBorder(Color.LIGHT_GRAY, 4, true),
                        "Modal Scale Start Tone"));
        modalScaleStartTonePanel.add(modalScaleStartBaseName);
        modalScaleStartTonePanel.add(buildScaleNameChoice(modalScaleStartBaseName));
        modalScaleStartBaseName.setSelectedItem(modalScaleStartBaseNameParam);

        panel.add(modalScaleStartTonePanel);
        
        panel.add(tuningComponent.getChoice(null));
    }
    
    public final double getFrequencyOfA4() {
        return toDouble(frequencyOfA4.getValue());
    }
    
    public final String getModalScaleStartIpnName() {
        final String lowestToneBaseName = getLowestToneBaseName();
        final String modalStartBaseName = (String) modalScaleStartBaseName.getSelectedItem();
        
        // modalStartIpnName must not be below lowestToneIpnName
        final List<String> toneBaseNamesList = List.of(getToneBaseNames()); // always starts from C
        final int lowestToneIndex = toneBaseNamesList.indexOf(lowestToneBaseName);
        final int modalStartToneIndex = toneBaseNamesList.indexOf(modalStartBaseName);
        final int lowestToneOctave = getLowestToneOctave() + (lowestToneIndex <= modalStartToneIndex ? 0 : 1);
        
        return modalStartBaseName + lowestToneOctave;
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
            int lowestToneOctave, 
            double frequencyOfA4Param, 
            int octavesParam)
    {
        final int A4_FREQ_MIN = toInt(A4_FREQUENCY_MINIMUM);
        final int A4_FREQ_MAX = toInt(A4_FREQUENCY_MAXIMUM);
        final int SPACING = toInt(5);// a tick every 5 Hertz
        
        final double finalFrequencyOfA4 = (frequencyOfA4Param >= A4_FREQUENCY_MINIMUM && frequencyOfA4Param <= A4_FREQUENCY_MAXIMUM)
                ? frequencyOfA4Param
                : ToneSystem.DEFAULT_REFERENCE_FREQUENCY;
        
        this.frequencyOfA4 = new JSlider(
                A4_FREQ_MIN,
                A4_FREQ_MAX,
                toInt(finalFrequencyOfA4));
        frequencyOfA4.setToolTipText("The Calculation Base for All Semitones in Tuning, Default "+ToneSystem.DEFAULT_REFERENCE_FREQUENCY);
        frequencyOfA4.setLabelTable(frequencyOfA4LableTable(A4_FREQ_MIN, A4_FREQ_MAX, SPACING));
        frequencyOfA4.setMajorTickSpacing(SPACING); 
        frequencyOfA4.setPaintLabels(true);
        frequencyOfA4.setPaintTicks(true);
        final String frequencyOfA4Title = "Frequency of A4: ";
        frequencyOfA4.setBorder(BorderFactory.createTitledBorder(
                BorderFactory.createLineBorder(Color.LIGHT_GRAY, 4, true),
                "Frequency of A4"));
        final ChangeListener frequencyListener = new ChangeListener() {
            @Override
            public void stateChanged(ChangeEvent e) {
                final String value = decimalFormatTooltip.format(toDouble(frequencyOfA4.getValue()));
                ((TitledBorder) frequencyOfA4.getBorder()).setTitle(frequencyOfA4Title+value);
            }
        };
        frequencyListener.stateChanged(null);
        frequencyOfA4.addChangeListener(frequencyListener);
        
        this.modalScaleStartBaseName = new SmartComboBox(getToneBaseNames());
        modalScaleStartBaseName.setBorder(BorderFactory.createTitledBorder("Key"));
        modalScaleStartBaseName.setToolTipText("Lowest Tone of the Chromatic Tonesystem to be Built");
        
        final String lowestToneIpnName;
        if (lowestToneBaseNameParam != null) {
            modalScaleStartBaseName.setSelectedItem(lowestToneBaseNameParam);
            lowestToneIpnName = lowestToneBaseNameParam + lowestToneOctave;
        }
        else {
            lowestToneIpnName = ToneSystem.DEFAULT_BASETONE_IPN_NAME;
        }
        
        this.tuningComponent = new TuningComponent(lowestToneIpnName, octavesParam);
    }

    private Dictionary<Integer,JLabel> frequencyOfA4LableTable(int minimum, int maximum, int spacing) {
        final Hashtable<Integer,JLabel> table = new Hashtable<>();
        for (int i = minimum; i <= maximum; i += spacing)
            table.put(i, new JLabel(decimalFormatLabels.format(toDouble(i))));
        return table;
    }

    private int toInt(double frequency) {
        return (int) Math.round(frequency * 10.0);
    }
    private double toDouble(int frequencySliderValue) {
        return ((double) frequencySliderValue) / 10.0;
    }
    
    
    /*public static void main(String[] args) {
        final ToneSystemConfigurationPanel config = new ToneSystemConfigurationPanel(5, "D", 3, 435.6, "A");
        final javax.swing.JButton button = new javax.swing.JButton("Print Settings");
        button.addActionListener(event -> {
            System.out.println(
                    "Octaves = "+config.getOctaves()+", "+
                    "FrequencyOfA4 = "+config.getFrequencyOfA4()+", "+
                    "LowestToneIpnName = "+config.getLowestToneIpnName()+", "+
                    "ModalScaleStartIpnName = "+config.getModalScaleStartIpnName()+", "+
                    "ToneSystem = \n"+config.getToneSystem()
                );
        });
        final JPanel app = new JPanel(new java.awt.BorderLayout());
        app.add(config.panel);
        app.add(button, java.awt.BorderLayout.SOUTH);
        fri.music.swingutils.window.FrameStarter.start("Tonesystem Settings Test", app, (java.awt.Dimension) null);
    }*/
}