package fri.music.instrument.configuration;

import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.Dictionary;
import java.util.Hashtable;
import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JSlider;
import javax.swing.border.TitledBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import fri.music.ToneSystem;

/**
 * Slider wrapper.
 */
public class FrequencyOfA4Component
{
    private static final double A4_FREQUENCY_MINIMUM = 410.0;
    private static final double A4_FREQUENCY_MAXIMUM = 460.0;
    
    private static final double SLIDER_MAGNIFICTION_FACTOR = 10.0;
    private static final NumberFormat decimalFormat = new DecimalFormat("0");
    
    public final JSlider frequencySlider;
    
    public FrequencyOfA4Component() {
        this(-1.0);
    }
    public FrequencyOfA4Component(double frequencyOfA4) {
        final int A4_FREQ_MIN = toInt(A4_FREQUENCY_MINIMUM);
        final int A4_FREQ_MAX = toInt(A4_FREQUENCY_MAXIMUM);
        final int SPACING = toInt(5);// a tick every 5 Hertz
        
        frequencyOfA4 = (frequencyOfA4 >= A4_FREQUENCY_MINIMUM && frequencyOfA4 <= A4_FREQUENCY_MAXIMUM)
                ? frequencyOfA4
                : ToneSystem.DEFAULT_REFERENCE_FREQUENCY;
        
        this.frequencySlider = new JSlider(
                A4_FREQ_MIN,
                A4_FREQ_MAX,
                toInt(frequencyOfA4));
        frequencySlider.setToolTipText("The Calculation Base for All Semitones in Tuning, Default "+ToneSystem.DEFAULT_REFERENCE_FREQUENCY);
        frequencySlider.setLabelTable(frequencyOfA4LableTable(A4_FREQ_MIN, A4_FREQ_MAX, SPACING));
        frequencySlider.setMajorTickSpacing(SPACING); 
        frequencySlider.setPaintLabels(true);
        frequencySlider.setPaintTicks(true);
        
        final String frequencyOfA4Title = "Frequency of A4: ";
        frequencySlider.setBorder(BorderFactory.createTitledBorder(frequencyOfA4Title));
        
        final ChangeListener frequencyListener = new ChangeListener() {
            private static final NumberFormat decimalFormat = new DecimalFormat("0.0");
            
            @Override
            public void stateChanged(ChangeEvent e) {
                final String value = decimalFormat.format(toDouble(frequencySlider.getValue()))+" Hertz";
                ((TitledBorder) frequencySlider.getBorder()).setTitle(frequencyOfA4Title+value);
            }
        };
        frequencyListener.stateChanged(null); // set the current value to titled border
        frequencySlider.addChangeListener(frequencyListener);
    }

    /** @return the currently set frequency of A4. */
    public double getValue() {
        return toDouble(frequencySlider.getValue());
    }
    
    private Dictionary<Integer,JLabel> frequencyOfA4LableTable(int minimum, int maximum, int spacing) {
        final Hashtable<Integer,JLabel> table = new Hashtable<>();
        for (int i = minimum; i <= maximum; i += spacing)
            table.put(i, new JLabel(decimalFormat.format(toDouble(i))));
        return table;
    }

    private int toInt(double frequency) {
        return (int) Math.round(frequency * SLIDER_MAGNIFICTION_FACTOR);
    }
    
    private double toDouble(int frequencySliderValue) {
        return ((double) frequencySliderValue) / SLIDER_MAGNIFICTION_FACTOR;
    }
}