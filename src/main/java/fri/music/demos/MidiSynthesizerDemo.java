package fri.music.demos;

import javax.sound.midi.MidiSystem;
import javax.sound.midi.MidiUnavailableException;
import javax.sound.midi.Synthesizer;
import javax.swing.JComponent;
import javax.swing.JFrame;
import fri.music.ScaleTypes;
import fri.music.instrument.midi.MidiSynthesizer;

public class MidiSynthesizerDemo
{
    public static void main(String[] args) throws MidiUnavailableException {
        final int octaves = 7;
        final String lowestToneIpnName = "C1";
        
        final String scale = ScaleTypes.scaleName(lowestToneIpnName);
        final JFrame frame = new JFrame("MIDI Synthesizer ("+scale+", "+octaves+" Octaves)");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        
        final Synthesizer synthesizer = MidiSystem.getSynthesizer();
        synthesizer.open();
        final MidiSynthesizer.Configuration config = 
                new MidiSynthesizer.Configuration(octaves, lowestToneIpnName, false, 16, null, null, true);
        final MidiSynthesizer piano = new MidiSynthesizer(config, synthesizer);
        final JComponent pianoPanel = piano.getKeyboard();
        
        frame.addWindowListener(piano.getWindowClosingListener());
        frame.add(pianoPanel);
        frame.pack();
        frame.setLocationRelativeTo(null);
        frame.setVisible(true);
    }
}