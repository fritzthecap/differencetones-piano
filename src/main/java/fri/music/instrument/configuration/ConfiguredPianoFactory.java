package fri.music.instrument.configuration;

import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.lang.reflect.Constructor;
import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import fri.music.SoundChannel;
import fri.music.instrument.PianoWithHold;
import fri.music.instrument.PianoWithSound;
import fri.music.instrument.PianoWithVolume;
import fri.music.instrument.midi.MidiSynthesizer;
import fri.music.instrument.midi.SynthesizerFactory;
import fri.music.instrument.wave.DifferenceToneForIntervalPiano;
import fri.music.instrument.wave.IntervalPlayingPiano;
import fri.music.instrument.wave.TriadPlayingPiano;
import fri.music.wavegenerator.GenericWaveSoundChannel;
import fri.music.wavegenerator.WaveGenerator;
import fri.music.wavegenerator.WaveNames;
import fri.music.wavegenerator.WaveSoundChannel;

/**
 * A panel that lets configure all options for different
 * kinds of Piano implementations, and then builds a piano out of them.
 * <p/>
 * This is more an 'instrument' API guide than an application.
 */
public class ConfiguredPianoFactory extends PianoConfigurationPanel
{
    private JCheckBox isVertical;
    
    private JRadioButton radioButtonMidi;
    private JRadioButton radioButtonWave;
    private JComboBox<String> waveChoice;
    
    private JComboBox<PianoClassChoiceItem> pianoClassChoice;
    
    public ConfiguredPianoFactory() {
        super();
        
        this.isVertical = new JCheckBox("Vertical Orientation"); // selected = false by default
        isVertical.setToolTipText("Not supported by all pianos. Please select 'PianoWithSound' to test vertical orientation!");
        
        displayOptionsPanel.add(isVertical);

        buildSoundChoiceFields();
        final JPanel soundChoice = new JPanel();
        soundChoice.setBorder(BorderFactory.createTitledBorder("Sound"));
        soundChoice.setLayout(new BoxLayout(soundChoice, BoxLayout.Y_AXIS));
        soundChoice.add(radioButtonWave);
        soundChoice.add(radioButtonMidi);
        final JPanel moveLeftSoundPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        moveLeftSoundPanel.add(soundChoice);
        moveLeftSoundPanel.add(waveChoice);
        moveLeftSoundPanel.add(pianoClassChoice);

        panel.add(moveLeftSoundPanel);
    }

    /**
     * Creates and returns a configured piano.
     * Call <code>piano.getKeyboard()</code> for a visible UI panel.
     * Install the </code>piano.getWindowClosingListener()</code> onto the parent frame.
     */
    public PianoWithSound getPiano() {
        final PianoWithSound.Configuration configuration = getPianoConfiguration();
        
        if (radioButtonWave.isSelected()) {
            final SoundChannel soundChannel = getSoundChannel();
            final Class<? extends PianoWithSound> pianoClass = 
                    ((PianoClassChoiceItem) pianoClassChoice.getSelectedItem()).pianoClass;
            
            Constructor<? extends PianoWithSound> constructor;
            try {
                constructor = pianoClass.getConstructor(PianoWithSound.Configuration.class, SoundChannel.class);
            }
            catch (NoSuchMethodException e) {
                try {
                    constructor = pianoClass.getConstructor(PianoWithSound.Configuration.class, WaveSoundChannel.class);
                }
                catch (Exception e1) {
                    throw new RuntimeException(e1);
                }
            }
            catch (Exception e) {
                throw new RuntimeException(e);
            }
            
            try {
                return constructor.newInstance(configuration, soundChannel);
            }
            catch (Exception e) {
                throw new RuntimeException(e);
            }
        }
        else {
            return new MidiSynthesizer(configuration, SynthesizerFactory.getOpenSynthesizer());
        }
    }
    
    /** @return a piano configuration that also contains the <code>vertical</code> property. */
    @Override
    public PianoWithSound.Configuration getPianoConfiguration() {
        PianoWithSound.Configuration configuration = super.getPianoConfiguration();
        return new PianoWithSound.Configuration(
            configuration.octaves,
            configuration.lowestToneIpnName,
            isVertical.isEnabled() && isVertical.isSelected(),
            configuration.blackKeyWidth,
            configuration.showIpnNameOnKey,
            configuration.showMidiNumberAsTooltip,
            configuration.colouredOctaves
        );
    }
    
    private SoundChannel getSoundChannel() {
        final String waveName = (String) waveChoice.getSelectedItem();
        final Class<? extends WaveGenerator> waveClass = WaveNames.getClass(waveName);
        return new GenericWaveSoundChannel(null, waveClass);
    }
    
    
    private void buildSoundChoiceFields() {
        this.radioButtonWave = new JRadioButton("Wave Generator", true);
        radioButtonWave.setToolTipText("Wave Generators Support Different Tunings");
        this.radioButtonMidi = new JRadioButton("MIDI Synthesizer", false);
        radioButtonMidi.setToolTipText("Java Sound Framework, no Support for Different Tunings");
        
        final ButtonGroup radioGroup = new ButtonGroup(); // exclusive selection
        radioGroup.add(radioButtonWave);
        radioGroup.add(radioButtonMidi);
        
        final ActionListener soundSelectionListener = new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                final boolean waveSelected = radioButtonWave.isSelected();
                waveChoice.setEnabled(waveSelected);
                pianoClassChoice.setEnabled(waveSelected);

                final PianoClassChoiceItem selectedPiano = waveSelected ? (PianoClassChoiceItem) pianoClassChoice.getSelectedItem() : null;
                final boolean canDisplayVertical = (selectedPiano != null && selectedPiano.pianoClass == PianoWithSound.class);
                isVertical.setEnabled(canDisplayVertical);
            }
        };
        radioButtonWave.addActionListener(soundSelectionListener);
        radioButtonMidi.addActionListener(soundSelectionListener);
        
        this.waveChoice = new JComboBox<>(WaveNames.getNames());
        waveChoice.setBorder(BorderFactory.createTitledBorder("Wave Type"));
        waveChoice.setToolTipText("Different Piano Sounds");
        
        final PianoClassChoiceItem[] pianoClasses = new PianoClassChoiceItem[] {
                new PianoClassChoiceItem(PianoWithSound.class),
                new PianoClassChoiceItem(PianoWithHold.class),
                new PianoClassChoiceItem(PianoWithVolume.class),
                new PianoClassChoiceItem(IntervalPlayingPiano.class),
                new PianoClassChoiceItem(TriadPlayingPiano.class),
                new PianoClassChoiceItem(DifferenceToneForIntervalPiano.class),
            };
        pianoClassChoice = new JComboBox<>(pianoClasses);
        pianoClassChoice.setBorder(BorderFactory.createTitledBorder("Piano Type"));
        pianoClassChoice.setToolTipText("Capabilities of Piano (Java Class)");
        pianoClassChoice.addActionListener(soundSelectionListener);
        
        soundSelectionListener.actionPerformed(null); // enable choice according to current setting
    }
    
    
    private static class PianoClassChoiceItem
    {
        public final Class<? extends PianoWithSound> pianoClass;
        
        PianoClassChoiceItem(Class<? extends PianoWithSound> pianoClass) {
            this.pianoClass = pianoClass;
        }
        
        @Override
        public String toString() {
            return pianoClass.getSimpleName();
        }
    }
}