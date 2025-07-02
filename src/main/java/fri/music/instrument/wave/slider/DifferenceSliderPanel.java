package fri.music.instrument.wave.slider;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JSlider;
import fri.music.Tones;
import fri.music.differencetones.DifferenceToneMath;
import fri.music.swingutils.ButtonUtil;

public class DifferenceSliderPanel extends SliderPanel
{
    private static final String arrowExpand = "\u23F7";
    private static final String arrowCollapse = "\u23F6";

    private final JButton expandButton;
    private DifferenceSliderPanel secondaryDifference;
    private DifferenceSliderPanel tertiaryDifference;
    
    public DifferenceSliderPanel(
            Tones tones, 
            String title, 
            String numberLabel, 
            JSlider amplitudeSlider,
            JSlider gainSlider, 
            JComboBox<String> waveChoice)
    {
        this(true, tones, title, numberLabel, amplitudeSlider, gainSlider, waveChoice);
    }
    
    private DifferenceSliderPanel(
            boolean isPrimaryDifference,
            Tones tones, 
            String title, 
            String numberLabel, 
            JSlider amplitudeSlider,
            JSlider gainSlider, 
            JComboBox<String> waveChoice)
    {
        super(tones, title, numberLabel, amplitudeSlider, gainSlider, waveChoice);
        
        frequencySlider.setEnabled(false);
        
        if (isPrimaryDifference) {
            secondaryDifference = 
                new DifferenceSliderPanel(false, tones, "Secondary", numberLabel, amplitudeSlider, gainSlider, waveChoice) {
                    @Override
                    protected double calculateDifference(double frequency1, double frequency2) {
                        return DifferenceToneMath.secondaryDifference(frequency1, frequency2);
                    }
                };
            secondaryDifference.startStop.setText(secondaryDifference.startStop.getText()+" (2nd)");
            
            tertiaryDifference = 
                new DifferenceSliderPanel(false, tones, "Tertiary", numberLabel, amplitudeSlider, gainSlider, waveChoice) {
                    @Override
                    protected double calculateDifference(double frequency1, double frequency2) {
                        return DifferenceToneMath.tertiaryDifference(frequency1, frequency2);
                    }
                };
            tertiaryDifference.startStop.setText(tertiaryDifference.startStop.getText()+" (3rd)");
            
            expandButton = createExpandButton();
            sliderPanel.add(expandButton, BorderLayout.NORTH);
            sliderPanel.add(createExpandablePanel(), BorderLayout.SOUTH);
        }
        else {
            expandButton = null;
        }
    }

    
    /** Called from setValue() to calculate difference tone for given frequencies. To be overridden. */
    protected double calculateDifference(double frequency1, double frequency2) {
        return DifferenceToneMath.primaryDifference(frequency1, frequency2);
    }
    
    /** Disables "Play" button when no valid difference-tone exists. */
    @Override
    public void setValue(double frequency) {
        super.setValue(frequency);
        startStop.setEnabled(isOnMinimum() == false);
    }
    
    /** Shows the difference-tone of given frequencies. */
    public void setValue(double frequency1, double frequency2) {
        final boolean someFrequencyIsZero = (frequency1 <= 0.0 || frequency2 <= 0.0);
        final double difference = someFrequencyIsZero ? 0.0 : calculateDifference(frequency1, frequency2);
        setValue(difference);
        
        if (expandButton != null) {
            secondaryDifference.setValue(frequency1, frequency2);
            tertiaryDifference.setValue(frequency1, frequency2);
        }
    }
    
    /** @return true if also secondary and tertiary difference is visible. */
    public boolean isExpanded() {
        return expandButton != null && expandButton.getText().equals(arrowExpand) == false;
    }
    
    /** Shows or hides secondary and tertiary difference. */
    public void setExpanded(boolean expanded) {
        if (expandButton != null && isExpanded() != expanded)
            ButtonUtil.doClick(expandButton);
    }
    
    @Override
    public void close() {
        super.close();
        
        if (expandButton != null) {
            secondaryDifference.close();
            tertiaryDifference.close();
        }
    }

    @Override
    public String getNote() {
        return isOnMinimum() ? "" : super.getNote();
    }
    
    
    private boolean isOnMinimum() {
        return frequencySlider.getValue() <= frequencySlider.getMinimum();
    }
    
    private JButton createExpandButton() {
        final JButton expandButton = new JButton(arrowExpand);
        expandButton.setToolTipText("Click to also see Secondary and Tertiary Difference Tones");
        final Dimension size = new Dimension(-1, 18);
        expandButton.setMaximumSize(size);
        expandButton.setPreferredSize(size);
        return expandButton;
    }
    
    private JComponent createExpandablePanel() {
        final JPanel secondaryTertiaryPanel = new JPanel();
        secondaryTertiaryPanel.setLayout(new BoxLayout(secondaryTertiaryPanel, BoxLayout.Y_AXIS));
        secondaryTertiaryPanel.setVisible(isExpanded());
        
        expandButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                final boolean becomeVisible = (isExpanded() == false);
                secondaryTertiaryPanel.setVisible(becomeVisible);
                
                if (becomeVisible) {
                    expandButton.setText(arrowCollapse);
                    expandButton.setToolTipText("Click to see only Primary Difference Tone");
                }
                else {
                    expandButton.setText(arrowExpand);
                    expandButton.setToolTipText("Click to also see Secondary and Tertiary Difference Tones");
                }
            }
        });
        
        secondaryTertiaryPanel.add(secondaryDifference.sliderPanel);
        secondaryTertiaryPanel.add(tertiaryDifference.sliderPanel);
        
        return secondaryTertiaryPanel;
    }
}
