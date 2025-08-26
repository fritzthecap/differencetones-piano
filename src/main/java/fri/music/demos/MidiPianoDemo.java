package fri.music.demos;

import javax.sound.midi.MidiSystem;
import javax.sound.midi.Synthesizer;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.SwingUtilities;
import fri.music.ScaleTypes;
import fri.music.instrument.PianoWithSound;
import fri.music.instrument.midi.MidiSoundChannel;

public class MidiPianoDemo
{
    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            final int octaves = 2;
            final String lowestToneIpnName = "F3";
            
            final String scale = ScaleTypes.scaleName(lowestToneIpnName);
            final JFrame frame = new JFrame("Vertical Java MIDI Piano ("+scale+", "+octaves+" Octaves)");
            frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            
            Synthesizer synth;
            try {
                synth = MidiSystem.getSynthesizer();
                synth.open();
                final PianoWithSound.Configuration config = new PianoWithSound.Configuration(
                        octaves, 
                        lowestToneIpnName, 
                        true, // vertical
                        16);
                final PianoWithSound piano = new PianoWithSound(config, new MidiSoundChannel(synth));
                final JComponent pianoPanel = piano.getKeyboard();
                
                frame.addWindowListener(piano.getWindowClosingListener());
                frame.getContentPane().add(pianoPanel);
                frame.pack();
                frame.setLocationRelativeTo(null);
                frame.setVisible(true);
            }
            catch (Exception e) {
                throw new RuntimeException(e);
            }
        });
    }
}
