package fri.music.demos;

import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import javax.swing.JComponent;
import javax.swing.JFrame;
import fri.music.ScaleTypes;
import fri.music.instrument.PianoWithSound;
import fri.music.instrument.PianoWithHold;
import fri.music.wavegenerator.SineWaveSoundChannel;

public class PianoWithHoldDemo
{ 
    public static void main(String[] args) {
        final int octaves = 4; //7;
        final String lowestToneIpnName = "C3";
        
        final String scale = ScaleTypes.scaleName(lowestToneIpnName);
        final JFrame frame = new JFrame("PianoWithHold ("+scale+", "+octaves+" Octaves)");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        
        final PianoWithSound.Configuration config = new PianoWithSound.Configuration(octaves, lowestToneIpnName, 18);
        final PianoWithSound piano = new PianoWithHold(config, new SineWaveSoundChannel(null));
        final JComponent pianoPanel = piano.getKeyboard();
        
        frame.addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent windowEvent) {
                piano.destroyKeyboard(pianoPanel);
            }
        });
        frame.add(pianoPanel);
        frame.pack();
        frame.setLocationRelativeTo(null);
        frame.setVisible(true);
    }
}