package fri.music.instrument.wave;

import javax.swing.BorderFactory;
import javax.swing.JSlider;
import javax.swing.SwingConstants;
import javax.swing.border.TitledBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

public class DeviationComponent
{
    private JSlider deviationSlider;
    
    public DeviationComponent(double defaultDeviation, boolean isVertical) {
        this.deviationSlider = buildDeviationSlider(doubleToDeviationPercent(defaultDeviation), isVertical);
    }
    
    /** @return the current deviation value from slider as floating-point number from 0 to 0.5. */
    public double getDeviation() {
        return deviationPercentToDouble(getSlider().getValue());
    }
    
    /** @return the slider component, displaying integer deviation percentage towards quarter-tone. */
    public JSlider getSlider() {
        return deviationSlider;
    }
    
    
    private JSlider buildDeviationSlider(int defaultDeviationPercent, boolean isVertical) {
        final String title = "Deviation Tolerance: ";
        
        final JSlider deviationSlider = new JSlider(0, 90, defaultDeviationPercent); // min, max, current
        deviationSlider.setBorder(BorderFactory.createTitledBorder(title));
        deviationSlider.setToolTipText(
                "Allowed deviation for finding difference-tones, 100 % being the middle between two semitones");
        deviationSlider.setOrientation(isVertical ? SwingConstants.VERTICAL : SwingConstants.HORIZONTAL);
        deviationSlider.setPaintLabels(true);
        deviationSlider.setMajorTickSpacing(10);
        
        final ChangeListener deviationChangeListener = new ChangeListener() {
            @Override
            public void stateChanged(ChangeEvent e) {
                final int deviationPercent = deviationSlider.getValue();
                ((TitledBorder) deviationSlider.getBorder()).setTitle(title+deviationPercent+" %");
            }
        };
        deviationSlider.addChangeListener(deviationChangeListener);
        deviationChangeListener.stateChanged(null); // establish title with percentage
        
        return deviationSlider;
    }
    
    private double deviationPercentToDouble(int percent) {
        return ((double) percent / 2.0) / 100.0;
    }
    
    private int doubleToDeviationPercent(double deviation) {
        return (int) Math.round(deviation * 2.0 * 100.0);
    }
}