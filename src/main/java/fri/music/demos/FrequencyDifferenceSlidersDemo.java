package fri.music.demos;

import javax.swing.JFrame;
import fri.music.instrument.wave.slider.FrequencyDifferenceSliders;

public class FrequencyDifferenceSlidersDemo
{
    public static void main(String[] args) {
        final JFrame frame = new JFrame();
        frame.setTitle("FrequencyDifferenceSlider Demo");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        
        final FrequencyDifferenceSliders sliders = new FrequencyDifferenceSliders();
        frame.addWindowListener(sliders.getWindowClosingListener());
        frame.getContentPane().add(sliders.panel);
        frame.setSize(1200, 410);
        frame.setLocationRelativeTo(null);
        frame.setVisible(true);
    }
}
