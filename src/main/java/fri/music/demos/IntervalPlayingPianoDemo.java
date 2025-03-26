package fri.music.demos;

import javax.swing.JComponent;
import javax.swing.JFrame;
import fri.music.ScaleTypes;
import fri.music.instrument.PianoWithSound;
import fri.music.instrument.wave.IntervalPlayingPiano;
import fri.music.wavegenerator.SineWaveSoundChannel;

public class IntervalPlayingPianoDemo
{ 
    public static void main(String[] args) {
        final int octaves = 5;
        final String lowestToneIpnName = "C3";
        
        final String scale = ScaleTypes.scaleName(lowestToneIpnName);
        final JFrame frame = new JFrame("IntervalPlaying Piano ("+scale+", "+octaves+" Octaves)");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        
        final PianoWithSound.Configuration config = new PianoWithSound.Configuration(octaves, lowestToneIpnName, 22);
        final PianoWithSound piano = new IntervalPlayingPiano(config, new SineWaveSoundChannel(null));
        final JComponent keyboard = piano.getKeyboard();
        
        frame.addWindowListener(piano.getWindowClosingListener());
        frame.add(keyboard);
        frame.pack();
        frame.setLocationRelativeTo(null);
        frame.setVisible(true);
    }
}