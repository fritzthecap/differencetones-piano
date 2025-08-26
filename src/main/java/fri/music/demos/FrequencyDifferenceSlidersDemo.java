package fri.music.demos;

import javax.swing.JFrame;
import javax.swing.SwingUtilities;
import fri.music.instrument.wave.slider.AbstractFrequencySliders;
import fri.music.instrument.wave.slider.FrequencyDifferenceSliders;

public class FrequencyDifferenceSlidersDemo
{
    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            final JFrame frame = new JFrame();
            frame.setTitle("See Intervals and Their Difference Tones on Frequency Sliders");
            frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            
            final AbstractFrequencySliders sliders = new FrequencyDifferenceSliders();
            frame.addWindowListener(sliders.getWindowClosingListener());
            frame.getContentPane().add(sliders.panel);
            frame.setSize(1200, 410);
            frame.setLocationRelativeTo(null);
            frame.setVisible(true);
        });
    }
}
