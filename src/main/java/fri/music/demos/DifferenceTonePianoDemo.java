package fri.music.demos;

import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import javax.swing.JComponent;
import javax.swing.JFrame;
import fri.music.EqualTemperament;
import fri.music.ScaleTypes;
import fri.music.ToneSystem;
import fri.music.instrument.PianoWithSound;
import fri.music.instrument.wave.DifferenceTonePiano;
import fri.music.wavegenerator.SineWaveSoundChannel;

public class DifferenceTonePianoDemo
{ 
    public static void main(String[] args) {
        final int octaves = 6;
        final String lowestToneIpnName = "C3";
        
        final String scale = ScaleTypes.scaleName(lowestToneIpnName);
        final JFrame frame = new JFrame("DifferenceTone Piano ("+scale+", "+octaves+" Octaves)");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        
        final ToneSystem toneSystem = new EqualTemperament(lowestToneIpnName, octaves);
        final PianoWithSound.Configuration config = new PianoWithSound.Configuration(octaves, lowestToneIpnName, 18);
        final PianoWithSound piano = new DifferenceTonePiano(config, new SineWaveSoundChannel(toneSystem.tones()));
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