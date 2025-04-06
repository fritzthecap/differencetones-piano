package fri.music.demos;

import javax.swing.JFrame;
import fri.music.instrument.wave.slider.AbstractFrequencySliders;
import fri.music.instrument.wave.slider.FrequencyChordSliders;

public class FrequencyChordSlidersDemo
{
    public static void main(String[] args) {
        final JFrame frame = new JFrame();
        frame.setTitle("FrequencyChordSliders Demo");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        
        final AbstractFrequencySliders sliders = new FrequencyChordSliders();
        frame.addWindowListener(sliders.getWindowClosingListener());
        frame.getContentPane().add(sliders.panel);
        frame.setSize(1200, 510);
        frame.setLocationRelativeTo(null);
        frame.setVisible(true);
    }
}
