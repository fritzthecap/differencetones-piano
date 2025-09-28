package fri.music.instrument.configuration;

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
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JPanel;
import javax.swing.JSlider;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import fri.music.ScaleTypes;
import fri.music.ToneSystem;
import fri.music.instrument.PianoWithSound;
import fri.music.swingutils.layout.SmartComboBox;

/**
 * A panel that lets configure most options for different
 * kinds of Piano implementations.
 */
public class PianoConfigurationPanel
{
    public final JPanel panel;
    
    protected final JPanel displayOptionsPanel;
    
    private JSlider octaves;
    private JComboBox<String> lowestToneBaseName;
    private JComboBox<String> lowestToneScaleName;
    private JSlider lowestToneOctave;
    private JSlider blackKeyPixelWidth;
    private JCheckBox showIpnNameOnKey;
    private JCheckBox showMidiNumberAsTooltip;
    private JCheckBox colouredOctaves;
    
    public PianoConfigurationPanel() {
        // field construction
        buildPianoConfigurationFields();
        
        // fields layout
        final JPanel lowestTonePanel = new JPanel();
        lowestTonePanel.setLayout(new BoxLayout(lowestTonePanel, BoxLayout.X_AXIS));
        lowestTonePanel.setBorder(BorderFactory.createTitledBorder(
                        BorderFactory.createLineBorder(Color.LIGHT_GRAY, 4, true),
                        "Lowest Tone"));
        lowestTonePanel.add(lowestToneBaseName);
        lowestTonePanel.add(lowestToneScaleName);
        lowestTonePanel.add(lowestToneOctave);
        
        this.displayOptionsPanel = new JPanel();
        displayOptionsPanel.setLayout(new BoxLayout(displayOptionsPanel, BoxLayout.Y_AXIS));
        displayOptionsPanel.add(showIpnNameOnKey);
        displayOptionsPanel.add(showMidiNumberAsTooltip);
        displayOptionsPanel.add(colouredOctaves);
        final JPanel moveLeftOptionsPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        moveLeftOptionsPanel.setBorder(BorderFactory.createTitledBorder("Display Options"));
        moveLeftOptionsPanel.add(displayOptionsPanel);

        this.panel = new JPanel();
        panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
        panel.add(octaves);
        panel.add(lowestTonePanel);
        panel.add(moveLeftOptionsPanel);
        panel.add(blackKeyPixelWidth);
    }

    /** @return a piano configuration built from all UI fields. */
    public PianoWithSound.Configuration getPianoConfiguration() {
        return new PianoWithSound.Configuration(
            octaves.getValue(),
            ""+lowestToneBaseName.getSelectedItem()+lowestToneOctave.getValue(),
            false,
            blackKeyPixelWidth.getValue(),
            showIpnNameOnKey.isSelected(),
            showMidiNumberAsTooltip.isSelected(),
            colouredOctaves.isSelected()
        );
    }
    
    
    private void buildPianoConfigurationFields() {
        final int INITIAL_NUMBER_OF_OCTAVES= 5;
        
        final ChangeListener octavesListener = new ChangeListener() {
            @Override
            public void stateChanged(ChangeEvent e) {
                if (e != null && ((JSlider) e.getSource()).getValueIsAdjusting())
                    return;
                final int startOctave = lowestToneOctave.getValue();
                final int maximumPossibleOctaves = ToneSystem.MAXIMUM_OCTAVES - startOctave;
                final boolean isMaximum = (octaves.getValue() == octaves.getMaximum());
                octaves.setMaximum(maximumPossibleOctaves);
                // try to keep a good value
                if (isMaximum && maximumPossibleOctaves <= INITIAL_NUMBER_OF_OCTAVES)
                    octaves.setValue(maximumPossibleOctaves);
            }
        };
        
        this.octaves = new JSlider(1, ToneSystem.MAXIMUM_OCTAVES, INITIAL_NUMBER_OF_OCTAVES);
        octaves.setBorder(BorderFactory.createTitledBorder("Number of Octaves (Maximum 10)"));
        octaves.setSnapToTicks(true);
        octaves.setPaintLabels(true);
        octaves.setPaintTicks(true);
        octaves.setMajorTickSpacing(1);
        octaves.addChangeListener(octavesListener);
        
        final List<String> ipnBaseNamesList = Stream.of(ToneSystem.IPN_BASE_NAMES)
            .filter(ipnName -> ipnName.indexOf(ToneSystem.SHARP_CHAR) < 0)
            .collect(Collectors.toList());
        final String[] ipnBaseNames = ipnBaseNamesList.toArray(new String[ipnBaseNamesList.size()]);
        this.lowestToneBaseName = new SmartComboBox(ipnBaseNames);
        lowestToneBaseName.setBorder(BorderFactory.createTitledBorder("Key"));
        lowestToneBaseName.setToolTipText("Leftmost (and Rightmost) Tone of the Keyboard");
        
        final Set<String> scaleNamesSet = ScaleTypes.scaleToStartNote.sequencedKeySet();
        final String[] scaleNames = scaleNamesSet.toArray(new String[scaleNamesSet.size()]);
        this.lowestToneScaleName = new SmartComboBox(scaleNames);
        lowestToneScaleName.setBorder(BorderFactory.createTitledBorder("or Scale"));
        lowestToneScaleName.setToolTipText("Modal Scale Type");
        
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
        lowestToneOctave.setBorder(BorderFactory.createTitledBorder("Start Octave"));
        lowestToneOctave.setToolTipText("The Lowest (Leftmost) Octave of the Keyboard");
        lowestToneOctave.setSnapToTicks(true);
        lowestToneOctave.setPaintLabels(true);
        lowestToneOctave.setPaintTicks(true);
        lowestToneOctave.setMajorTickSpacing(1);
        lowestToneOctave.addChangeListener(octavesListener);
        
        this.blackKeyPixelWidth = new JSlider(4, 60, 16);
        blackKeyPixelWidth.setBorder(BorderFactory.createTitledBorder("Black Key Pixel Width"));
        blackKeyPixelWidth.setToolTipText("Bigger or Smaller Keyboard Keys");
        blackKeyPixelWidth.setPaintLabels(true);
        blackKeyPixelWidth.setPaintTicks(true);
        blackKeyPixelWidth.setMajorTickSpacing(4);
        
        this.showIpnNameOnKey = new JCheckBox("IPN-Names on Keys");
        showIpnNameOnKey.setToolTipText("Show Note Names on Piano Keys");
        showIpnNameOnKey.setSelected(true);
        
        this.showMidiNumberAsTooltip = new JCheckBox("MIDI-Numbers as Tooltips");
        showMidiNumberAsTooltip.setToolTipText("Show MIDI-Numbers as Tooltips on Piano Keys (e.g. C4 is 60)");
        showMidiNumberAsTooltip.setSelected(false);
        
        this.colouredOctaves = new JCheckBox("Coloured Octaves");
        colouredOctaves.setToolTipText("Show Every Octave with a Different Color");
        colouredOctaves.setSelected(false);
        
        octavesListener.stateChanged(null);
    }
}