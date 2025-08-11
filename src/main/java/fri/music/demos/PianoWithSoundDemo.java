package fri.music.demos;

import javax.swing.JComponent;
import javax.swing.JFrame;
import fri.music.ScaleTypes;
import fri.music.instrument.PianoWithSound;
import fri.music.wavegenerator.SineWaveSoundChannel;

public class PianoWithSoundDemo
{ 
    public static void main(String[] args) {
        final int octaves = 3;
        final String lowestToneIpnName = "D3";
        
        final String scale = ScaleTypes.scaleName(lowestToneIpnName);
        
        final PianoWithSound.Configuration config = new PianoWithSound.Configuration(
                octaves, 
                lowestToneIpnName, 
                true, // vertical
                14); // black key pixel width
        final PianoWithSound piano = new PianoWithSound(config, new SineWaveSoundChannel(null));
        final JComponent pianoPanel = piano.getKeyboard();
        
        final JFrame frame = new JFrame("Vertical Piano With Sound Example ("+scale+", "+octaves+" Octaves)");
        frame.addWindowListener(piano.getWindowClosingListener());
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.add(pianoPanel);
        frame.pack();
        frame.setLocationRelativeTo(null);
        frame.setVisible(true);
    }
}