package fri.music.demos;

import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import javax.swing.JComponent;
import javax.swing.JFrame;
import fri.music.ScaleTypes;
import fri.music.instrument.PianoWithSound;
import fri.music.instrument.PianoWithVolume;
import fri.music.wavegenerator.SineWaveSoundChannel;

public class PianoWithVolumeDemo
{ 
    public static void main(String[] args) {
        final int octaves = 4; //ToneSystem.DEFAULT_OCTAVES;
        final String lowestToneIpnName = "C3"; //null;
        
        final String scale = ScaleTypes.scaleName(lowestToneIpnName);
        final JFrame frame = new JFrame("PianoWithVolume ("+scale+", "+octaves+" Octaves)");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        
        final PianoWithSound.Configuration config = new PianoWithSound.Configuration(octaves, lowestToneIpnName, 12);
        final PianoWithVolume pianoWithVolume = new PianoWithVolume(config, new SineWaveSoundChannel(null));
        final JComponent pianoPanel = pianoWithVolume.getKeyboard();
        
        frame.addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent windowEvent) {
                pianoWithVolume.destroyKeyboard(pianoPanel);
            }
        });
        frame.add(pianoPanel);
        frame.pack();
        frame.setLocationRelativeTo(null);
        frame.setVisible(true);
    }
}