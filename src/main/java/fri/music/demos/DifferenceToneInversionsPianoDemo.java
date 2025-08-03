package fri.music.demos;

import javax.swing.JComponent;
import javax.swing.JFrame;
import fri.music.EqualTemperament;
import fri.music.ScaleTypes;
import fri.music.ToneSystem;
import fri.music.instrument.PianoWithSound;
import fri.music.instrument.wave.DifferenceToneInversionsPiano;
import fri.music.wavegenerator.SineWaveSoundChannel;

public class DifferenceToneInversionsPianoDemo
{ 
    public static void main(String[] args) {
        final int octaves = 6;
        final String lowestToneIpnName = "C3";
        
        final String scale = ScaleTypes.scaleName(lowestToneIpnName);
        final JFrame frame = new JFrame("DifferenceToneInversionsPiano ("+scale+", "+octaves+" Octaves)");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        
        final ToneSystem toneSystem = new EqualTemperament(lowestToneIpnName, octaves);
        final PianoWithSound.Configuration config = new PianoWithSound.Configuration(octaves, lowestToneIpnName, 18);
        final PianoWithSound piano = new DifferenceToneInversionsPiano(config, new SineWaveSoundChannel(toneSystem.tones()));
        final JComponent pianoPanel = piano.getKeyboard();
        
        frame.addWindowListener(piano.getWindowClosingListener());
        frame.add(pianoPanel);
        frame.pack();
        frame.setLocationRelativeTo(null);
        frame.setVisible(true);
    }
}