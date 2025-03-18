package fri.music.instrument;

import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
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
import fri.music.instrument.midi.MidiSoundChannel;
import fri.music.instrument.midi.SynthesizerFactory;
import fri.music.wavegenerator.GenericWaveSoundChannel;
import fri.music.wavegenerator.WaveGenerator;
import fri.music.wavegenerator.WaveNames;

public class ConfigurationPanel
{
    public final JPanel panel;
    
    private JSlider octaves;
    private JComboBox<String> lowestToneBaseName;
    private JComboBox<String> lowestToneScaleName;
    private JSlider lowestToneOctave;
    private JCheckBox isVertical;
    private JSlider blackKeyWidthPixels;
    private JCheckBox showIpnNameOnKey;
    private JCheckBox showMidiNumberAsTooltip;
    private JCheckBox colouredOctaves;
    
    private JRadioButton radioButtonMidi;
    private JRadioButton radioButtonSineWave;
    private JComboBox<String> waveChoice;
    
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
        displayOptionsPanel.add(isVertical);
        displayOptionsPanel.add(showIpnNameOnKey);
        displayOptionsPanel.add(showMidiNumberAsTooltip);
        displayOptionsPanel.add(colouredOctaves);
        final JPanel moveLeftOptionsPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        moveLeftOptionsPanel.setBorder(BorderFactory.createTitledBorder("Display Options"));
        moveLeftOptionsPanel.add(displayOptionsPanel);

        final JPanel soundChoice = new JPanel();
        soundChoice.setBorder(BorderFactory.createTitledBorder("Sound"));
        soundChoice.setLayout(new BoxLayout(soundChoice, BoxLayout.Y_AXIS));
        soundChoice.add(radioButtonMidi);
        soundChoice.add(radioButtonSineWave);
        final JPanel moveLeftSoundPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        moveLeftSoundPanel.add(soundChoice);
        moveLeftSoundPanel.add(waveChoice);

        this.panel = new JPanel();
        panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
        panel.add(octaves);
        panel.add(lowestTonePanel);
        panel.add(moveLeftOptionsPanel);
        panel.add(blackKeyWidthPixels);
        panel.add(moveLeftSoundPanel);
    }

    public PianoWithSound.Configuration getPianoConfiguration() {
        return new PianoWithSound.Configuration(
            octaves.getValue(),
            ""+lowestToneBaseName.getSelectedItem()+lowestToneOctave.getValue(),
            isVertical.isSelected(),
            blackKeyWidthPixels.getValue(),
            showIpnNameOnKey.isSelected(),
            showMidiNumberAsTooltip.isSelected(),
            colouredOctaves.isSelected()
        );
    }
    
    public SoundChannel getSoundChannel() {
        if (radioButtonSineWave.isSelected()) {
            final String waveName = (String) waveChoice.getSelectedItem();
            final Class<? extends WaveGenerator> waveClass = WaveNames.getClass(waveName);
            return new GenericWaveSoundChannel(null, waveClass);
        }
        return new MidiSoundChannel(SynthesizerFactory.getMidiChannel());
    }
    
    
    private void buildPianoConfigurationFields() {
        this.octaves = new JSlider(1, ToneSystem.MAXIMUM_OCTAVES, 3);
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
        
        this.lowestToneOctave = new JSlider(ToneSystem.LOWEST_OCTAVE, ToneSystem.MAXIMUM_OCTAVES - 1, 3);
        lowestToneOctave.setBorder(BorderFactory.createTitledBorder("Octave"));
        lowestToneOctave.setSnapToTicks(true);
        lowestToneOctave.setPaintLabels(true);
        lowestToneOctave.setPaintTicks(true);
        lowestToneOctave.setMajorTickSpacing(1);
        
        this.isVertical = new JCheckBox("Vertical"); // selected = false by default
        isVertical.setBorder(BorderFactory.createTitledBorder("Vertical Piano"));
        
        this.blackKeyWidthPixels = new JSlider(4, 60, 16);
        blackKeyWidthPixels.setBorder(BorderFactory.createTitledBorder("Black Key Pixel Width"));
        blackKeyWidthPixels.setPaintLabels(true);
        blackKeyWidthPixels.setPaintTicks(true);
        blackKeyWidthPixels.setMajorTickSpacing(4);
        
        this.showIpnNameOnKey = new JCheckBox("IPN-Names on Keys");
        this.showIpnNameOnKey.setSelected(true);
        
        this.showMidiNumberAsTooltip = new JCheckBox("MIDI-Numbers as Tooltips on Keys");
        this.showMidiNumberAsTooltip.setSelected(true);
        
        this.colouredOctaves = new JCheckBox("Octaves with Different Colors"); // selected = false by default
    }
    
    private void buildSoundChoiceFields() {
        this.radioButtonMidi = new JRadioButton("Java MIDI Synthesizer", true);
        this.radioButtonSineWave = new JRadioButton("Wave Generator", false);
        
        final ButtonGroup radioGroup = new ButtonGroup(); // exclusive selection
        radioGroup.add(radioButtonMidi);
        radioGroup.add(radioButtonSineWave);
        
        final ActionListener soundSelectionListener = new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                waveChoice.setEnabled(radioButtonSineWave.isSelected());
            }
        };
        radioButtonMidi.addActionListener(soundSelectionListener);
        radioButtonSineWave.addActionListener(soundSelectionListener);
        
        this.waveChoice = new JComboBox<>(WaveNames.getNames());
        waveChoice.setBorder(BorderFactory.createTitledBorder("Wavetype"));
        
        soundSelectionListener.actionPerformed(null); // enable choice according to current setting
    }
}