package fri.music.demos;

import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.SwingUtilities;
import fri.music.instrument.PianoWithSound;
import fri.music.instrument.PianoWithVolume;
import fri.music.wavegenerator.SineWaveSoundChannel;

public class PianoWithVolumeDemo
{ 
    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            final int octaves = 4;
            final String lowestToneIpnName = "C3";
            
            final JFrame frame = new JFrame("Piano With Volume Example ("+octaves+" Octaves)");
            frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            
            final PianoWithSound.Configuration config = new PianoWithSound.Configuration(octaves, lowestToneIpnName);
            final PianoWithVolume piano = new PianoWithVolume(config, new SineWaveSoundChannel(null));
            final JComponent pianoPanel = piano.getKeyboard();
            
            frame.addWindowListener(piano.getWindowClosingListener());
            frame.getContentPane().add(pianoPanel);
            frame.pack();
            frame.setLocationRelativeTo(null);
            frame.setVisible(true);
        });
    }
}