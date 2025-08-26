package fri.music.demos;

import javax.swing.JFrame;
import javax.swing.SwingUtilities;
import fri.music.instrument.wave.slider.AbstractFrequencySliders;
import fri.music.instrument.wave.slider.FrequencyChordSliders;

public class FrequencyChordSlidersDemo
{
    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            final JFrame frame = new JFrame();
            frame.setTitle("Study Chords with Frequency Sliders");
            frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            
            final AbstractFrequencySliders sliders = new FrequencyChordSliders();
            frame.addWindowListener(sliders.getWindowClosingListener());
            frame.getContentPane().add(sliders.panel);
            frame.setSize(1200, 510);
            frame.setLocationRelativeTo(null);
            frame.setVisible(true);
        });
    }
}
