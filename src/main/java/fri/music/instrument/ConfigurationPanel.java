package fri.music.instrument;

import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.lang.reflect.Constructor;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JSlider;
import fri.music.ScaleTypes;
import fri.music.SoundChannel;
import fri.music.ToneSystem;
import fri.music.instrument.midi.MidiSynthesizer;
import fri.music.instrument.midi.SynthesizerFactory;
import fri.music.instrument.swing.SmartComboBox;
import fri.music.instrument.wave.DifferenceTonePiano;
import fri.music.instrument.wave.IntervalPlayingPiano;
import fri.music.instrument.wave.TriadPlayingPiano;
import fri.music.wavegenerator.GenericWaveSoundChannel;
import fri.music.wavegenerator.WaveGenerator;
import fri.music.wavegenerator.WaveNames;
import fri.music.wavegenerator.WaveSoundChannel;

/**
 * A panel that lets configure all options for different
 * kinds of Piano implementations, and then run them.
 */
public class ConfigurationPanel
{
    public final JPanel panel;
    
    private JSlider octaves;
    private JComboBox<String> lowestToneBaseName;
    private JComboBox<String> lowestToneScaleName;
    private JSlider lowestToneOctave;
    private JCheckBox isVertical;
    private JSlider blackKeyPixelWidth;
    private JCheckBox showIpnNameOnKey;
    private JCheckBox showMidiNumberAsTooltip;
    private JCheckBox colouredOctaves;
    
    private JRadioButton radioButtonMidi;
    private JRadioButton radioButtonWave;
    private JComboBox<String> waveChoice;
    
    private JComboBox<PianoClassChoiceItem> pianoClassChoice;
    
    public ConfigurationPanel() {
        buildPianoConfigurationFields();
        buildSoundChoiceFields();
        
        final JPanel lowestTonePanel = new JPanel();
        lowestTonePanel.setLayout(new BoxLayout(lowestTonePanel, BoxLayout.X_AXIS));
        lowestTonePanel.setBorder(BorderFactory.createTitledBorder(
                        BorderFactory.createLineBorder(Color.LIGHT_GRAY, 4, true),
                        "Lowest Tone"));
        lowestTonePanel.add(lowestToneBaseName);
        lowestTonePanel.add(lowestToneScaleName);
        lowestTonePanel.add(lowestToneOctave);
        
        final JPanel displayOptionsPanel = new JPanel();
        displayOptionsPanel.setLayout(new BoxLayout(displayOptionsPanel, BoxLayout.Y_AXIS));
        displayOptionsPanel.add(showIpnNameOnKey);
        displayOptionsPanel.add(showMidiNumberAsTooltip);
        displayOptionsPanel.add(colouredOctaves);
        displayOptionsPanel.add(isVertical);
        final JPanel moveLeftOptionsPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        moveLeftOptionsPanel.setBorder(BorderFactory.createTitledBorder("Display Options"));
        moveLeftOptionsPanel.add(displayOptionsPanel);

        final JPanel soundChoice = new JPanel();
        soundChoice.setBorder(BorderFactory.createTitledBorder("Sound"));
        soundChoice.setLayout(new BoxLayout(soundChoice, BoxLayout.Y_AXIS));
        soundChoice.add(radioButtonWave);
        soundChoice.add(radioButtonMidi);
        final JPanel moveLeftSoundPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        moveLeftSoundPanel.add(soundChoice);
        moveLeftSoundPanel.add(waveChoice);
        moveLeftSoundPanel.add(pianoClassChoice);

        this.panel = new JPanel();
        panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
        panel.add(octaves);
        panel.add(lowestTonePanel);
        panel.add(moveLeftOptionsPanel);
        panel.add(blackKeyPixelWidth);
        panel.add(moveLeftSoundPanel);
    }

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
    
    private PianoWithSound.Configuration getPianoConfiguration() {
        return new PianoWithSound.Configuration(
            octaves.getValue(),
            ""+lowestToneBaseName.getSelectedItem()+lowestToneOctave.getValue(),
            isVertical.isEnabled() && isVertical.isSelected(),
            blackKeyPixelWidth.getValue(),
            showIpnNameOnKey.isSelected(),
            showMidiNumberAsTooltip.isSelected(),
            colouredOctaves.isSelected()
        );
    }
    
    private SoundChannel getSoundChannel() {
        final String waveName = (String) waveChoice.getSelectedItem();
        final Class<? extends WaveGenerator> waveClass = WaveNames.getClass(waveName);
        return new GenericWaveSoundChannel(null, waveClass);
    }
    
    
    private void buildPianoConfigurationFields() {
        this.octaves = new JSlider(1, ToneSystem.MAXIMUM_OCTAVES, 5);
        octaves.setBorder(BorderFactory.createTitledBorder("Number of Octaves"));
        octaves.setSnapToTicks(true);
        octaves.setPaintLabels(true);
        octaves.setPaintTicks(true);
        octaves.setMajorTickSpacing(1);
        
        final List<String> ipnBaseNamesList = Stream.of(ToneSystem.IPN_BASE_NAMES)
            .filter(ipnName -> ipnName.contains("#") == false)
            .collect(Collectors.toList());
        final String[] ipnBaseNames = ipnBaseNamesList.toArray(new String[ipnBaseNamesList.size()]);
        this.lowestToneBaseName = new SmartComboBox(ipnBaseNames);
        lowestToneBaseName.setBorder(BorderFactory.createTitledBorder("Name"));
        
        final Set<String> scaleNamesSet = ScaleTypes.scaleToStartNote.sequencedKeySet();
        final String[] scaleNames = scaleNamesSet.toArray(new String[scaleNamesSet.size()]);
        this.lowestToneScaleName = new SmartComboBox(scaleNames);
        lowestToneScaleName.setBorder(BorderFactory.createTitledBorder("Scale"));
        
        lowestToneBaseName.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                final String ipnBaseName = (String) lowestToneBaseName.getSelectedItem();
                final String scaleName = ScaleTypes.scaleName(ipnBaseName);
                lowestToneScaleName.setSelectedItem(scaleName);
            }
        });
        lowestToneScaleName.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                final String scaleName = (String) lowestToneScaleName.getSelectedItem();
                final String ipnBaseName = ScaleTypes.scaleToStartNote.get(scaleName);
                lowestToneBaseName.setSelectedItem(ipnBaseName);
            }
        });
        
        this.lowestToneOctave = new JSlider(ToneSystem.LOWEST_OCTAVE, ToneSystem.MAXIMUM_OCTAVES - 1, 2);
        lowestToneOctave.setBorder(BorderFactory.createTitledBorder("Octave"));
        lowestToneOctave.setSnapToTicks(true);
        lowestToneOctave.setPaintLabels(true);
        lowestToneOctave.setPaintTicks(true);
        lowestToneOctave.setMajorTickSpacing(1);
        
        this.isVertical = new JCheckBox("Vertical Orientation"); // selected = false by default
        isVertical.setToolTipText("Please select 'PianoWithSound' to test this option");
        
        this.blackKeyPixelWidth = new JSlider(4, 60, 16);
        blackKeyPixelWidth.setBorder(BorderFactory.createTitledBorder("Black Key Pixel Width"));
        blackKeyPixelWidth.setPaintLabels(true);
        blackKeyPixelWidth.setPaintTicks(true);
        blackKeyPixelWidth.setMajorTickSpacing(4);
        
        this.showIpnNameOnKey = new JCheckBox("IPN-Names on Keys");
        showIpnNameOnKey.setSelected(true);
        
        this.showMidiNumberAsTooltip = new JCheckBox("MIDI-Numbers as Tooltips on Keys");
        showMidiNumberAsTooltip.setSelected(true);
        
        this.colouredOctaves = new JCheckBox("Octaves with Different Colors");
        colouredOctaves.setSelected(true);
    }
    
    private void buildSoundChoiceFields() {
        this.radioButtonWave = new JRadioButton("Wave Generator", true);
        this.radioButtonMidi = new JRadioButton("MIDI Synthesizer", false);
        
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
        
        final PianoClassChoiceItem[] pianoClasses = new PianoClassChoiceItem[] {
                new PianoClassChoiceItem(DifferenceTonePiano.class),
                new PianoClassChoiceItem(TriadPlayingPiano.class),
                new PianoClassChoiceItem(IntervalPlayingPiano.class),
                new PianoClassChoiceItem(PianoWithHold.class),
                new PianoClassChoiceItem(PianoWithVolume.class),
                new PianoClassChoiceItem(PianoWithSound.class),
            };
        pianoClassChoice = new JComboBox<>(pianoClasses);
        pianoClassChoice.setBorder(BorderFactory.createTitledBorder("Piano Type"));
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