package fri.music.instrument.configuration;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JComboBox;
import javax.swing.JPanel;
import javax.swing.JSlider;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import fri.music.ScaleTypes;
import fri.music.ToneSystem;
import fri.music.swingutils.layout.SmartComboBox;

/**
 * A panel that lets configure some common options for
 * ToneSystem implementations.
 */
public class ToneRangeConfigurationPanel
{
    protected static final int DEFAULT_OCTAVES = 7;
    protected static final int DEFAULT_LOWEST_TONE_OCTAVE = 2;
    protected static final String DEFAULT_LOWEST_TONE_BASENAME = "C";
    protected static final String DEFAULT_LOWEST_TONE_IPNNAME = DEFAULT_LOWEST_TONE_BASENAME + DEFAULT_LOWEST_TONE_OCTAVE;
    
    /** The user-interface panel to add to some frame or dialog. */
    public final JPanel panel;
    
    private JSlider octaves;
    private JComboBox<String> lowestToneBaseName;
    private JComboBox<String> lowestToneScaleName;
    private JSlider lowestToneOctave;
    
    public ToneRangeConfigurationPanel(int octavesParam, String lowestToneBaseNameParam, int lowestToneOctaveParam) {
        this(octavesParam, lowestToneBaseNameParam, lowestToneOctaveParam, true);
    }
    
    public ToneRangeConfigurationPanel(int octavesParam, String lowestToneBaseNameParam, int lowestToneOctaveParam, boolean addScaleNameChoice) {
        // field construction
        buildConfigurationFields(octavesParam, lowestToneBaseNameParam, lowestToneOctaveParam, addScaleNameChoice);
        
        // fields layout
        final JPanel lowestTonePanel = new JPanel();
        lowestTonePanel.setLayout(new BoxLayout(lowestTonePanel, BoxLayout.X_AXIS));
        lowestTonePanel.setBorder(BorderFactory.createTitledBorder(
                        BorderFactory.createLineBorder(Color.LIGHT_GRAY, 4, true),
                        "Lowest Tone"));
        lowestTonePanel.add(lowestToneBaseName);
        if (lowestToneScaleName != null)
            lowestTonePanel.add(lowestToneScaleName);
        lowestTonePanel.add(lowestToneOctave);
        
        this.panel = new JPanel();
        panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
        
        panel.add(octaves);
        panel.add(lowestTonePanel);
    }
    
    public final int getOctaves() {
        return octaves.getValue();
    }
    
    public final String getLowestToneIpnName() {
        return "" + lowestToneBaseName.getSelectedItem() + lowestToneOctave.getValue();
    }
    
    protected final int getLowestToneOctave() {
        return lowestToneOctave.getValue();
    }
    
    protected final String getLowestToneBaseName() {
        return (String) lowestToneBaseName.getSelectedItem();
    }
    
    protected final String[] getToneBaseNames() {
        final List<String> toneBaseNamesList = Stream.of(ToneSystem.IPN_BASE_NAMES)
            .filter(ipnName -> ipnName.indexOf(ToneSystem.SHARP_CHAR) < 0)
            .collect(Collectors.toList());
        return toneBaseNamesList.toArray(new String[toneBaseNamesList.size()]);
    }
    
    /** @return alternative scale name choice for choosing a lowest tone. */
    protected JComboBox<String> buildScaleNameChoice(final JComboBox<String> lowestToneBaseName) {
        final Set<String> scaleNamesSet = ScaleTypes.scaleToStartNote.sequencedKeySet();
        final String[] scaleNames = scaleNamesSet.toArray(new String[scaleNamesSet.size()]);
        
        final JComboBox<String> lowestToneScaleName = new JComboBox<>(scaleNames);
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
        
        return lowestToneScaleName;
    }

    
    private void buildConfigurationFields(
            int octavesParam, 
            String lowestToneBaseNameParam, 
            int lowestToneOctaveParam, 
            boolean addScaleNameChoice)
    {
       final int octavesParamFinal = (octavesParam <= 0) ? DEFAULT_OCTAVES : octavesParam;
        
        if (lowestToneBaseNameParam == null || lowestToneBaseNameParam.length() <= 0)
            lowestToneBaseNameParam = DEFAULT_LOWEST_TONE_BASENAME;
        
        if (lowestToneOctaveParam <= 0)
            lowestToneOctaveParam = DEFAULT_LOWEST_TONE_OCTAVE;
        
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
                if (isMaximum && maximumPossibleOctaves <= octavesParamFinal)
                    octaves.setValue(maximumPossibleOctaves);
            }
        };
        
        this.octaves = new JSlider(1, ToneSystem.MAXIMUM_OCTAVES, octavesParamFinal);
        octaves.setToolTipText("The Number of Octaves above the Starting Octave");
        octaves.setBorder(BorderFactory.createTitledBorder("Number of Octaves (Maximum "+ToneSystem.MAXIMUM_OCTAVES+")"));
        octaves.setSnapToTicks(true);
        octaves.setPaintLabels(true);
        octaves.setPaintTicks(true);
        octaves.setMajorTickSpacing(1);
        octaves.addChangeListener(octavesListener);
        
        this.lowestToneBaseName = new SmartComboBox(getToneBaseNames());
        lowestToneBaseName.setBorder(BorderFactory.createTitledBorder("Key"));
        lowestToneBaseName.setToolTipText("Lowest Tone of the Tonesystem");
        
        if (addScaleNameChoice)
            this.lowestToneScaleName = buildScaleNameChoice(lowestToneBaseName);
        
        lowestToneBaseName.setSelectedItem(lowestToneBaseNameParam);
        
        this.lowestToneOctave = new JSlider(
                ToneSystem.LOWEST_OCTAVE, 
                ToneSystem.MAXIMUM_OCTAVES - 1, 
                lowestToneOctaveParam);
        lowestToneOctave.setBorder(BorderFactory.createTitledBorder("Start Octave"));
        lowestToneOctave.setToolTipText("Lowest Octave of the Tonesystem");
        lowestToneOctave.setSnapToTicks(true);
        lowestToneOctave.setPaintLabels(true);
        lowestToneOctave.setPaintTicks(true);
        lowestToneOctave.setMajorTickSpacing(1);
        lowestToneOctave.addChangeListener(octavesListener);
        
        octavesListener.stateChanged(null);
    }
}