package fri.music.demos;

import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import javax.swing.JFrame;
import fri.music.instrument.wave.slider.FrequencySliders;

public class FrequencySlidersDemo
{
    public static void main(String[] args) {
        final JFrame frame = new JFrame();
        frame.setTitle("FrequencySlider Demo");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        
        final FrequencySliders frequencySlider = new FrequencySliders();
        frame.addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                frequencySlider.close();
            }
        });
        frame.getContentPane().add(frequencySlider.panel);
        frame.setSize(1200, 410);
        frame.setLocationRelativeTo(null);
        frame.setVisible(true);
    }
}
