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

public class DifferenceSliderPanel extends SliderPanel
{
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
        this(tones, title, numberLabel, amplitudeSlider, gainSlider, waveChoice, true);
    }
    
    private DifferenceSliderPanel(
            Tones tones, 
            String title, 
            String numberLabel, 
            JSlider amplitudeSlider,
            JSlider gainSlider, 
            JComboBox<String> waveChoice,
            boolean isPrimaryDifference)
    {
        super(tones, title, numberLabel, amplitudeSlider, gainSlider, waveChoice);
        
        frequencySlider.setEnabled(false);
        
        if (isPrimaryDifference) {
            secondaryDifference = 
                new DifferenceSliderPanel(tones, "Secondary", numberLabel, amplitudeSlider, gainSlider, waveChoice, false) {
                    @Override
                    protected double calculateDifference(double frequency1, double frequency2) {
                        return DifferenceToneMath.secondaryDifference(frequency1, frequency2);
                    }
                };
            tertiaryDifference = 
                new DifferenceSliderPanel(tones, "Tertiary", numberLabel, amplitudeSlider, gainSlider, waveChoice, false) {
                    @Override
                    protected double calculateDifference(double frequency1, double frequency2) {
                        return DifferenceToneMath.tertiaryDifference(frequency1, frequency2);
                    }
                };
            sliderPanel.add(createExpandablePanel(), BorderLayout.NORTH);
        }
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
        
        if (secondaryDifference != null)
            secondaryDifference.setValue(frequency1, frequency2);
        
        if (tertiaryDifference != null)
            tertiaryDifference.setValue(frequency1, frequency2);
    }

    protected double calculateDifference(double frequency1, double frequency2) {
        return DifferenceToneMath.primaryDifference(frequency1, frequency2);
    }
    
    @Override
    public String getNote() {
        return isOnMinimum() ? "" : super.getNote();
    }
    
    private boolean isOnMinimum() {
        return frequencySlider.getValue() <= frequencySlider.getMinimum();
    }
    
    private JComponent createExpandablePanel() {
        final JButton expandButton = new JButton("\u23F7"); // arrow down  
        final JButton collapseButton = new JButton("\u23F6"); // arrow up
        expandButton.setToolTipText("Click to also see Secondary and Tertiary Difference Tones");
        collapseButton.setToolTipText("Click to see only Primary Difference Tone");
        
        final Dimension size = new Dimension(-1, 18);
        expandButton.setMaximumSize(size);
        expandButton.setPreferredSize(size);
        collapseButton.setMaximumSize(size);
        collapseButton.setPreferredSize(size);
        
        final JPanel centerPanel = new JPanel();
        centerPanel.setLayout(new BoxLayout(centerPanel, BoxLayout.Y_AXIS));
        centerPanel.setVisible(false);
        
        final JPanel buttonPanel = new JPanel(new BorderLayout());
        buttonPanel.add(expandButton);
        
        expandButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                centerPanel.setVisible(true);
                
                buttonPanel.remove(expandButton);
                buttonPanel.add(collapseButton);
                buttonPanel.revalidate();
                buttonPanel.repaint();
            }
        });
        collapseButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                centerPanel.setVisible(false);
                
                buttonPanel.remove(collapseButton);
                buttonPanel.add(expandButton);
                buttonPanel.revalidate();
                buttonPanel.repaint();
            }
        });
        
        centerPanel.add(secondaryDifference.sliderPanel);
        centerPanel.add(tertiaryDifference.sliderPanel);
        
        sliderPanel.add(centerPanel, BorderLayout.SOUTH);
        
        return buttonPanel;
    }
}
