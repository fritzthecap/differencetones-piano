package fri.music.demos;

import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
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
        final WavePiano wavePiano = new WavePiano(config, null);
        final JComponent pianoPanel = wavePiano.getKeyboard();
        
        frame.addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent windowEvent) {
                wavePiano.destroyKeyboard(pianoPanel);
            }
        });
        frame.add(pianoPanel);
        frame.pack();
        frame.setLocationRelativeTo(null);
        frame.setVisible(true);
    }
}