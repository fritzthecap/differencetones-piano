package fri.music.instrument.midi;

import javax.sound.midi.MidiChannel;
import javax.sound.midi.Synthesizer;
import fri.music.SoundChannel;

public class MidiSoundChannel implements SoundChannel
{
    public final MidiChannel delegate;
    
    /** Chooses channel[0] of given synthesizer as sound channel. */
    public MidiSoundChannel(Synthesizer synthesizer) {
        this(synthesizer.getChannels()[0]);
    }
    
    private MidiSoundChannel(MidiChannel delegate) {
        this.delegate = delegate;
    }

    @Override
    public void noteOn(int midiNoteNumber, int velocity) {
        delegate.noteOn(midiNoteNumber, velocity);
    }

    @Override
    public void noteOff(int midiNoteNumber) {
        delegate.noteOff(midiNoteNumber); 
    }
    
    @Override
    public void allNotesOff() {
        delegate.allNotesOff();
    }
    
    @Override
    public void volumeChange(int volume) {
        delegate.controlChange(7, volume); // 7 = MIDI volume controller
    }
    
    // MIDI controller numbers: 
    // See https://www.paulcecchettimusic.com/full-list-of-midi-cc-numbers/
    // 84   Portamento Control
    //      Controls the amount of Portamento.
    // 65   Portamento  On/Off switch
    //      0 -63 = Off, 64 -127 = On
    // 5    Portamento Time
    //      Controls portamento rate to slide between 2 notes played subsequently.
    // 10   Pan(orama)
    //      Controls the left and right balance, generally for mono patches.
    //      0= hard left, 64 = center, 127 = hard right
}
