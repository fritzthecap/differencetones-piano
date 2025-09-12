package fri.music.demos;

import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.SwingUtilities;
import fri.music.EqualTemperament;
import fri.music.ToneSystem;
import fri.music.instrument.PianoWithSound;
import fri.music.instrument.wave.DifferenceToneInversionsPiano;
import fri.music.wavegenerator.GenericWaveSoundChannel;

public class DifferenceToneInversionsPianoDemo
{ 
    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            final int octaves = 6;
            final String lowestToneIpnName = "C3";
            
            final JFrame frame = new JFrame("See Which Intervals Can Generate a Difference-Tone");
            frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            
            final ToneSystem toneSystem = new EqualTemperament(lowestToneIpnName, octaves);
            final PianoWithSound.Configuration config = new PianoWithSound.Configuration(octaves, lowestToneIpnName, 18);
            final PianoWithSound piano = new DifferenceToneInversionsPiano(config, new GenericWaveSoundChannel(toneSystem.tones(), null));
            final JComponent pianoPanel = piano.getKeyboard();
            
            frame.addWindowListener(piano.getWindowClosingListener());
            frame.getContentPane().add(pianoPanel);
            frame.pack();
            frame.setLocationRelativeTo(null);
            frame.setVisible(true);
        });
    }
}