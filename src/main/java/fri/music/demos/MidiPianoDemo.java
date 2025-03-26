package fri.music.demos;

import javax.sound.midi.MidiSystem;
import javax.sound.midi.MidiUnavailableException;
import javax.sound.midi.Synthesizer;
import javax.swing.JComponent;
import javax.swing.JFrame;
import fri.music.ScaleTypes;
import fri.music.instrument.PianoWithSound;
import fri.music.instrument.midi.MidiSoundChannel;

public class MidiPianoDemo
{
    public static void main(String[] args) throws MidiUnavailableException {
        final int octaves = 2;
        final String lowestToneIpnName = "F3";
        
        final String scale = ScaleTypes.scaleName(lowestToneIpnName);
        final JFrame frame = new JFrame("MIDI Piano ("+scale+", "+octaves+" Octaves)");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        
        final Synthesizer synth = MidiSystem.getSynthesizer();
        synth.open();
        final PianoWithSound.Configuration config = new PianoWithSound.Configuration(octaves, lowestToneIpnName, true, 16);
        final PianoWithSound piano = new PianoWithSound(config, new MidiSoundChannel(synth));
        final JComponent pianoPanel = piano.getKeyboard();
        
        frame.addWindowListener(piano.getWindowClosingListener());
        frame.add(pianoPanel);
        frame.pack();
        frame.setLocationRelativeTo(null);
        frame.setVisible(true);
    }
}
