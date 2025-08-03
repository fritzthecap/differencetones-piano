package fri.music.instrument.wave;

import java.awt.event.ActionListener;
import javax.swing.BorderFactory;
import javax.swing.JComboBox;
import fri.music.ToneSystem;

/**
 * Used for difference-tone interval construction.
 * If you want to find all intervals that generate a certain difference-tone,
 * you need to define which is the narrowest and the widest interval allowed.
 */
public class IntervalRangeComponent
{
    private static final String[] narrowestIntervalNames = new String[] {
            ToneSystem.MAJOR_SECOND,
            ToneSystem.MINOR_THIRD,
            ToneSystem.MAJOR_THIRD,
            ToneSystem.FOURTH
        };
    private static final String[] widestIntervalNames = new String[] {
            ToneSystem.FIFTH,
            ToneSystem.MINOR_SIXTH,
            ToneSystem.MAJOR_SIXTH,
            ToneSystem.MINOR_SEVENTH
        };

    private JComboBox<String> narrowestIntervalChoice;
    private JComboBox<String> widestIntervalChoice;

    public IntervalRangeComponent(ActionListener narrowestListener, ActionListener widestListener) {
        this.narrowestIntervalChoice = new JComboBox<String>(narrowestIntervalNames);
        narrowestIntervalChoice.setBorder(BorderFactory.createTitledBorder("Narrowest Interval"));
        narrowestIntervalChoice.setToolTipText("Narrowest Allowed Interval for Generating Difference Tones");
        narrowestIntervalChoice.setSelectedItem(ToneSystem.MINOR_THIRD); // make good default
        if (narrowestListener != null)
            narrowestIntervalChoice.addActionListener(narrowestListener);

        this.widestIntervalChoice = new JComboBox<String>(widestIntervalNames);
        widestIntervalChoice.setBorder(BorderFactory.createTitledBorder("Widest Interval"));
        widestIntervalChoice.setToolTipText("Widest Allowed Interval for Generating Difference Tones");
        widestIntervalChoice.setSelectedItem(ToneSystem.MAJOR_SIXTH);
        if (widestListener != null)
            widestIntervalChoice.addActionListener(widestListener);
    }
    
    public JComboBox<String> getNarrowestChoice() {
        return narrowestIntervalChoice;
    }

    public JComboBox<String> getWidestChoice() {
        return widestIntervalChoice;
    }

    public String narrowestAllowedInterval() {
        return (String) narrowestIntervalChoice.getSelectedItem();
    }

    public String widestAllowedInterval() {
        return (String) widestIntervalChoice.getSelectedItem();
    }

    public void setEnabled(boolean enabled) {
        narrowestIntervalChoice.setEnabled(enabled);;
        widestIntervalChoice.setEnabled(enabled);;
    }
}