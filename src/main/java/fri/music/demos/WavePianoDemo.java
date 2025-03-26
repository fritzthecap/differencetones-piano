package fri.music.demos;

import javax.swing.JComponent;
import javax.swing.JFrame;
import fri.music.ScaleTypes;
import fri.music.instrument.PianoWithSound;
import fri.music.instrument.wave.WavePiano;

public class WavePianoDemo
{ 
    public static void main(String[] args) {
        final int octaves = 4; //3; //7;
        final String lowestToneIpnName = "C3"; //"B1";
        
        final String scale = ScaleTypes.scaleName(lowestToneIpnName);
        final JFrame frame = new JFrame("Wave Piano ("+scale+", "+octaves+" Octaves)");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        
        final PianoWithSound.Configuration config = new PianoWithSound.Configuration(octaves, lowestToneIpnName, 16);
        final WavePiano piano = new WavePiano(config, null);
        final JComponent pianoPanel = piano.getKeyboard();
        
        frame.addWindowListener(piano.getWindowClosingListener());
        frame.add(pianoPanel);
        frame.pack();
        frame.setLocationRelativeTo(null);
        frame.setVisible(true);
    }
}