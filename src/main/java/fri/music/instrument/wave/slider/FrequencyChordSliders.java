package fri.music.instrument.wave.slider;

import javax.swing.JComboBox;
import javax.swing.JPanel;
import javax.swing.JSlider;
import fri.music.ToneSystem;

/**
 * Lets try out 4 frequencies combined.
 */
public class FrequencyChordSliders extends AbstractFrequencySliders
{
    private SliderPanel frequencyPanel1;
    private SliderPanel frequencyPanel2;
    private SliderPanel frequencyPanel3;
    private SliderPanel frequencyPanel4;

    /** Render tone range C3 - C8. */
    public FrequencyChordSliders() {
        this(null);
    }
    
    /** Render given tone range. */
    public FrequencyChordSliders(ToneSystem toneSystem) {
        super(toneSystem);
    }
    
    @Override
    protected SliderPanel[] getSliderPanels() {
        return new SliderPanel[] {
            frequencyPanel1,
            frequencyPanel2,
            frequencyPanel3,
            frequencyPanel4
        };
    }
    
    @Override
    protected JPanel[] createFrequencySliderPanels(JSlider amplitudeSlider, JSlider gainSlider, JComboBox<String> waveChoice) {
        frequencyPanel1 = new SliderPanel(tones(), "Frequency", "1", amplitudeSlider, gainSlider, waveChoice);
        frequencyPanel1.setNote("G4");
        frequencyPanel2 = new SliderPanel(tones(), "Frequency", "2", amplitudeSlider, gainSlider, waveChoice);
        frequencyPanel2.setNote("B4");
        frequencyPanel3 = new SliderPanel(tones(), "Frequency", "3", amplitudeSlider, gainSlider, waveChoice);
        frequencyPanel3.setNote("D5");
        frequencyPanel4 = new SliderPanel(tones(), "Frequency", "4", amplitudeSlider, gainSlider, waveChoice);
        frequencyPanel4.setNote("F5");
        
        return new JPanel[] {
            frequencyPanel1.sliderPanel,
            frequencyPanel2.sliderPanel,
            frequencyPanel3.sliderPanel,
            frequencyPanel4.sliderPanel
        };
    }
}