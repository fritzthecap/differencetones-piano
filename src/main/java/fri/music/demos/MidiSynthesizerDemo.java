package fri.music.demos;

import javax.sound.midi.MidiSystem;
import javax.sound.midi.MidiUnavailableException;
import javax.sound.midi.Synthesizer;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.SwingUtilities;
import fri.music.instrument.midi.MidiSynthesizer;

public class MidiSynthesizerDemo
{
    public static void main(String[] args) throws MidiUnavailableException {
        SwingUtilities.invokeLater(() -> {
            final int octaves = 7;
            final String lowestToneIpnName = "C1";
            
            final JFrame frame = new JFrame("Hear Java MIDI Synthesizer Instruments ("+octaves+" Octaves)");
            frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            
            Synthesizer synthesizer;
            try {
                synthesizer = MidiSystem.getSynthesizer();
                synthesizer.open();
                final MidiSynthesizer.Configuration config = 
                        new MidiSynthesizer.Configuration(
                                octaves, 
                                lowestToneIpnName, 
                                false, 
                                16, 
                                null, 
                                null, 
                                true); // colored
                final MidiSynthesizer piano = new MidiSynthesizer(config, synthesizer);
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