package fri.music.instrument.midi;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.LinkedHashSet;
import java.util.SequencedSet;
import javax.sound.midi.Instrument;
import javax.sound.midi.MidiChannel;
import javax.sound.midi.Synthesizer;
import javax.swing.BorderFactory;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import fri.music.instrument.PianoWithHold;
import fri.music.instrument.PianoWithSound;
import fri.music.swingutils.SmartComboBox;

/**
 * Different instruments for the piano keyboard.
 */
public class MidiSynthesizer extends PianoWithHold
{
    private final Synthesizer synthesizer;
    private final MidiChannel midiChannel;
    private JComponent pianoPanel;
    
    public MidiSynthesizer() {
        this((PianoWithSound.Configuration) null);
    }
    public MidiSynthesizer(PianoWithSound.Configuration config) {
        this(config, SynthesizerFactory.getOpenSynthesizer());
    }
    public MidiSynthesizer(Synthesizer synthesizer) {
        this(null, synthesizer);
    }
    public MidiSynthesizer(PianoWithSound.Configuration config, Synthesizer synthesizer) {
        super(config, new MidiSoundChannel(synthesizer));
        
        this.synthesizer = synthesizer;
        this.midiChannel = synthesizer.getChannels()[0];
    }
    
    @Override
    public JComponent getKeyboard() {
        if (this.pianoPanel != null)
            return this.pianoPanel; // just one view, due to mouseHandler that stores UI-state
        
        final JComponent pianoPanel = super.getKeyboard();
        
        final JComboBox<String> instrumentChoice = new SmartComboBox(getAvailableInstrumentNames());
        instrumentChoice.setBorder(BorderFactory.createTitledBorder("Instrument"));
        instrumentChoice.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                changeInstrument((String) instrumentChoice.getSelectedItem());
            }
        });
        
        getControlPanel().add(instrumentChoice, 2);
        
        getVolumeSlider().setValue(127 / 2); // MIDI volume is much lower than WaveGenerator volumes
        getVelocitySlider().setValue(127 / 2);
        
        moveControlPanelBelowKeyboard();
        
        return this.pianoPanel = pianoPanel;
    }
    
    @Override
    public void destroyKeyboard(JComponent pianoPanel) {
        super.destroyKeyboard(pianoPanel);
        synthesizer.close();
    }
    
    private String[] getAvailableInstrumentNames() {
        final Instrument[] availableInstruments = synthesizer.getAvailableInstruments();
        final String[] instruments = new String[availableInstruments.length];
        for (int i = 0; i < availableInstruments.length; i++)
            instruments[i] = availableInstruments[i].getName();
        
        return instruments;
    }
    
    /** Sets given instrument into the synthesizer channel this keyboard obtains. */
    private void changeInstrument(String instrumentName) {
        for (Instrument instrument : synthesizer.getAvailableInstruments())
            if (instrument.getName().equals(instrumentName))
                if (synthesizer.loadInstrument(instrument))
                    midiChannel.programChange(instrument.getPatch().getProgram());
        
        ((InstrumentChangingMouseHandler) getMouseHandler()).restartHoldNotes();
    }
    
    @Override
    protected MouseHandler newMouseHandler() {
        return new InstrumentChangingMouseHandler(this);
    }
    
    
    protected static class InstrumentChangingMouseHandler extends HoldMouseHandler
    {
        public InstrumentChangingMouseHandler(PianoWithSound piano) {
            super(piano);
        }
        
        /** Restarts currently playing notes after instrument change. */
        public final void restartHoldNotes() {
            final SequencedSet<Keyboard.Key> playingNotesCopy = new LinkedHashSet<>(holdPlayingNotes);
            
            reset();
            
            for (Keyboard.Key key : playingNotesCopy)
                noteOn(key);
        }
    }
}