package fri.music.instrument.configuration;

import java.awt.FlowLayout;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.Dictionary;
import java.util.Hashtable;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSlider;
import javax.swing.border.TitledBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import fri.music.ToneSystem;
import fri.music.instrument.TuningComponent;
import fri.music.utils.swing.layout.SmartComboBox;

/**
 * A panel that lets configure all options for
 * ToneSystem implementations.
 */
public class ToneSystemConfigurationPanel extends ToneRangeConfigurationPanel
{
    private static final double A4_FREQUENCY_MINIMUM = 410.0;
    private static final double A4_FREQUENCY_MAXIMUM = 460.0;
    
    private static final double SLIDER_MAGNIFICTION_FACTOR = 10.0;

    private JSlider frequencyOfA4;
    private JComboBox<String> modalScaleStartBaseName;
    private TuningComponent tuningComponent;
    
    private final NumberFormat decimalFormatLabels = new DecimalFormat("0");
    private final NumberFormat decimalFormatTooltip = new DecimalFormat("0.0");
    
    /** Default constructor. */
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
        final JComponent tuningsChoice = tuningComponent.getChoice(null); // null: default tuning
        final JPanel tuningsPanel = new JPanel();
        tuningsPanel.setLayout(new BoxLayout(tuningsPanel, BoxLayout.X_AXIS));
        tuningsPanel.add(tuningsChoice);
        tuningsPanel.add(Box.createHorizontalGlue()); // to keep choice left-aligned
        
        // set tunings choice on top
        panel.add(tuningsPanel, 0); // null: default tuning
        
        panel.add(frequencyOfA4);
        
        final JPanel modalScaleStartTonePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        modalScaleStartTonePanel.add(modalScaleStartBaseName);
        modalScaleStartTonePanel.add(buildScaleNameChoice(modalScaleStartBaseName));
        
        modalScaleStartTonePanel.setBorder(BorderFactory.createTitledBorder("Modal Scale Start Tone"));
        panel.add(modalScaleStartTonePanel);
    }
    
    public final double getFrequencyOfA4() {
        return toDouble(frequencyOfA4.getValue());
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
        frequencyOfA4.setBorder(BorderFactory.createTitledBorder("Frequency of A4"));
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

    private Dictionary<Integer,JLabel> frequencyOfA4LableTable(int minimum, int maximum, int spacing) {
        final Hashtable<Integer,JLabel> table = new Hashtable<>();
        for (int i = minimum; i <= maximum; i += spacing)
            table.put(i, new JLabel(decimalFormatLabels.format(toDouble(i))));
        return table;
    }

    private int toInt(double frequency) {
        return (int) Math.round(frequency * SLIDER_MAGNIFICTION_FACTOR);
    }
    
    private double toDouble(int frequencySliderValue) {
        return ((double) frequencySliderValue) / SLIDER_MAGNIFICTION_FACTOR;
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