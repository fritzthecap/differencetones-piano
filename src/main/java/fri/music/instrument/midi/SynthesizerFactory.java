package fri.music.instrument.midi;

import javax.sound.midi.MidiChannel;
import javax.sound.midi.MidiSystem;
import javax.sound.midi.MidiUnavailableException;
import javax.sound.midi.Synthesizer;

public class SynthesizerFactory
{
    public static Synthesizer getOpenSynthesizer() {
        try {
            Synthesizer synthesizer = MidiSystem.getSynthesizer();
            synthesizer.open();
            return synthesizer;
        }
        catch (MidiUnavailableException e) {
            throw new RuntimeException(e);
        }
    }

    public static MidiChannel getMidiChannel() {
        return getOpenSynthesizer().getChannels()[0];
    }
}