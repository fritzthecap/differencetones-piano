package fri.music.demos;

import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
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
        final PianoWithSound midiPiano = new PianoWithSound(config, new MidiSoundChannel(synth.getChannels()[0]));
        final JComponent pianoPanel = midiPiano.getKeyboard();
        
        frame.addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent windowEvent) {
                midiPiano.destroyKeyboard(pianoPanel);
            }
        });
        frame.add(pianoPanel);
        frame.pack();
        frame.setLocationRelativeTo(null);
        frame.setVisible(true);
    }
}
