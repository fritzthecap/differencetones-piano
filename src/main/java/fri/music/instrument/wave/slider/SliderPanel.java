package fri.music.instrument.wave.slider;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionAdapter;
import java.util.Dictionary;
import java.util.Hashtable;
import java.util.Map;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSlider;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import fri.music.Tone;
import fri.music.ToneSystem;
import fri.music.Tones;
import fri.music.instrument.swing.slider.SliderMouseEventLocator;
import fri.music.instrument.swing.slider.SliderRightClickMouseAdapter;
import fri.music.wavegenerator.WaveGenerator;

/**
 * One frequency slider and its functionality.
 */
public class SliderPanel
{
    public static final String DEFAULT_INITIAL_VALUE = "C6";
    public static final String START_LABEL = "Play";
    public static final String STOP_LABEL = "Halt";
    public static final Dimension STARTSTOP_BUTTON_SIZE = new Dimension(120, 30);
    
    /** Number of micro-tones within a semi-tone. */
    private static final int MICROTONES = 24;
    
    /** The slider with "Play" button, to add. */
    public final JPanel sliderPanel;
    /** The "Play" button, to click upon. */
    public final JButton startStop;
    
    protected final Tones tones; // final: when tones change, this panel will be abandoned
    
    protected final JSlider frequencySlider;
    
    private final Map<Double,JLabel> sliderLabels = new Hashtable<>();
    private JLabel currentlyMarkedLabel;
    
    private WaveGenerator waveGenerator;
    
    protected final JSlider amplitudeSlider;
    protected final JSlider gainSlider;
    // following are here just for removing listeners on close()
    private final ChangeListener amplitudeChangeListener;
    private final ChangeListener gainChangeListener;
    
    public SliderPanel(
            Tones tones,
            String title, 
            String numberLabel, 
            JSlider amplitudeSlider, 
            JSlider gainSlider, 
            JComboBox<String> waveChoice) 
    {
        this.tones = tones;
        this.amplitudeSlider = amplitudeSlider;
        this.gainSlider = gainSlider;
        
        amplitudeChangeListener = new ChangeListener() {
            @Override
            public void stateChanged(ChangeEvent e) {
                if (waveGenerator != null)
                    waveGenerator.setAmplitude(((JSlider) e.getSource()).getValue());
            }
        };
        amplitudeSlider.addChangeListener(amplitudeChangeListener);
        
        gainChangeListener = new ChangeListener() {
            @Override
            public void stateChanged(ChangeEvent e) {
                if (waveGenerator != null)
                    waveGenerator.setGain(((JSlider) e.getSource()).getValue());
            }
        };
        gainSlider.addChangeListener(gainChangeListener);

        frequencySlider = createFrequencySlider();
        startStop = createStartButton(numberLabel);
        
        final ChangeListener changeListener = new ChangeListener() {
            @Override
            public void stateChanged(ChangeEvent e) {
                final double frequency = getValue();
                // display frequency in titled border
                final String rounded = (frequency > 0.0) ? Tone.frequencyFormat.format(frequency) : "";
                frequencySlider.setBorder(
                        BorderFactory.createTitledBorder(title+" "+numberLabel+": "+(frequency > 0.0 ? rounded+" Hertz" : "")));
                
                if (waveGenerator != null) // change to new frequency
                    waveGenerator.setFrequency(frequency);
                
                if (currentlyMarkedLabel != null) { /// highlight note of frequency
                    currentlyMarkedLabel.setForeground(Color.BLACK);
                    currentlyMarkedLabel = null;
                }
                final JLabel label = sliderLabels.get(frequency);
                if (label != null) {
                    label.setForeground(Color.RED);
                    currentlyMarkedLabel = label;
                }
            }
        };
        changeListener.stateChanged(null); // needs non-null frequencySlider
        addChangeListener(changeListener);
        
        startStop.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent event) {
                final double frequency = getValue();
                if (waveGenerator == null && frequency > ToneSystem.MINIMUM_FREQUENCY) {
                    final String packageName = WaveGenerator.class.getPackageName();
                    final String baseClassName = WaveGenerator.class.getSimpleName();
                    final String generatorName = (String) waveChoice.getSelectedItem();
                    final String className = packageName+"."+generatorName+baseClassName;
                    try {
                        waveGenerator = (WaveGenerator) 
                            Class.forName(className).getDeclaredConstructor().newInstance();
                    }
                    catch (Exception e) {
                        throw new RuntimeException("Wave-generator not implemented: "+className);
                    }
                    waveGenerator.setGain((float) gainSlider.getValue());
                    waveGenerator.start(getValue(), amplitudeSlider.getValue());
                    startStop.setText(STOP_LABEL+" "+numberLabel);
                    startStop.setForeground(Color.RED);
                }
                else {
                    closeWaveGenerator();
                    startStop.setText(START_LABEL+" "+numberLabel);
                    startStop.setForeground(Color.BLUE);
                }
            }
        });
        
        final JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
        buttonPanel.add(startStop); // without this panel, "Play" button would not have STARTSTOP_BUTTON_SIZE
        
        sliderPanel = new JPanel(new BorderLayout());
        sliderPanel.add(frequencySlider, BorderLayout.CENTER);
        sliderPanel.add(buttonPanel, BorderLayout.WEST);
    }
    
    
    /** @return a Hertz frequency from slider. */
    public double getValue() {
        return valueToFrequency(frequencySlider.getValue());
    }
    /** @param value a Hertz frequency to set into slider. */
    public void setValue(double frequency) {
        frequencySlider.setValue(frequencyToValue(frequency));
    }
    
    /** @return the currently active note from slider, or empty when in between notes. */
    public String getNote() {
        final Tone tone = tones.forFrequency(getValue());
        return (tone != null) ? tone.ipnName : "";
    }
    /** @param ipnName the IPN-name of the note to set its frequency into the slider. */
    public void setNote(String ipnName) {
        final Tone tone = tones.forIpnName(ipnName);
        if (tone != null)
            setValue(tone.frequency);
    }
    
    public void addChangeListener(ChangeListener listener) {
        frequencySlider.addChangeListener(listener);
    }
    public void removeChangeListener(ChangeListener listener) {
        frequencySlider.removeChangeListener(listener);
    }
  
    public boolean isPlaying() {
        return waveGenerator != null;
    }
    
    public void close() {
        closeWaveGenerator();
        amplitudeSlider.removeChangeListener(amplitudeChangeListener);
        gainSlider.removeChangeListener(gainChangeListener);
    }
    
    
    private void closeWaveGenerator() {
        if (isPlaying())
            waveGenerator.close();
        waveGenerator = null;
    }
    
    private JButton createStartButton(String numberLabel) {
        final JButton start = new JButton(START_LABEL+" "+numberLabel);
        start.setForeground(Color.BLUE);
        start.setPreferredSize(STARTSTOP_BUTTON_SIZE);
        start.setMaximumSize(STARTSTOP_BUTTON_SIZE);
        return start;
    }
    
    
    private JSlider createFrequencySlider() {
        final JSlider frequencySlider = new JSlider();
        
        frequencySlider.addMouseListener(new SliderRightClickMouseAdapter());
        
        frequencySlider.addMouseMotionListener(new MouseMotionAdapter() {
            /** Sets the slider's tooltip text to the frequency of the note the mouse is over.*/
            @Override
            public void mouseMoved(MouseEvent e) {
                final int sliderValue = new SliderMouseEventLocator(e).sliderValue;
                final int midiNoteNumber = (sliderValue + MICROTONES / 2) / MICROTONES;
                final Tone tone = tones.forMidiNoteNumber(midiNoteNumber);
                if (tone != null)
                    frequencySlider.setToolTipText(tone.formattedFrequency()+" Hertz");
                else
                    frequencySlider.setToolTipText(null);
            }
        });
        
        frequencySlider.setMinimum(frequencyToValue(tones.getLowest().frequency) - 1);
        frequencySlider.setMaximum(frequencyToValue(tones.getHighest().frequency));
        frequencySlider.setValue(frequencyToValue(tones.forIpnName(DEFAULT_INITIAL_VALUE).frequency));
        frequencySlider.setLabelTable(sliderLableTable()); // the IPN note names
        frequencySlider.setMajorTickSpacing(MICROTONES); // a tick every semi-tone
        frequencySlider.setPaintLabels(true);
        frequencySlider.setPaintTicks(true);
        
        return frequencySlider;
    }
    
    private Dictionary<Integer,JLabel> sliderLableTable() {
        final Hashtable<Integer,JLabel> dictionary = new Hashtable<>();
        for (Tone tone : tones.tones)
            dictionary.put(tone.midiNumber * MICROTONES, createLabel(tone));
        return dictionary;
    }

    private JLabel createLabel(Tone tone) {
        final JLabel label = new JLabel(tone.ipnName.contains("#") ? "^" : tone.ipnName) { // \u2022 \u2B27
            /** Keep labels enabled and thus readable. */
            @Override
            public void setEnabled(boolean enabled) {
            }
        };
        sliderLabels.put(tone.frequency, label);
        return label;
    }
    

    /** Conversion from slider int-values to double-frequencies via MIDI-numbers of tones. */
    private double valueToFrequency(int value) {
        int midiNoteNumber = value / MICROTONES;
        int numberOfMicroTones = value % MICROTONES;
        
        final Tone tone = tones.forMidiNoteNumber(midiNoteNumber);
        if (tone == null) // lowest is one below lowest midiNotNumber, the "- 1" in minimum
            return 0.0;
        
        if (numberOfMicroTones == 0)
            return tone.frequency;
        
        final Tone upper = tones.getNextUpper(tone);
        if (upper == null)
            return tones.getHighest().frequency;
        
        final double difference = upper.frequency - tone.frequency;
        final double oneMicrotone = difference / MICROTONES;
        
        return tone.frequency + (oneMicrotone * numberOfMicroTones);
    }

    /** Conversion from double-frequencies to slider int-values via MIDI-numbers of tones. */
    private int frequencyToValue(double frequency) {
        if (frequency <= 0.0) // handles the "- 1" in minimum
            return frequencySlider.getMinimum();
        
        int numberOfMicroTones = 0;
        Tone tone = tones.forFrequency(frequency);
        
        if (tone == null) { // in between micro-tones
            final Tone[] enclosingTones = tones.getEnclosingTones(frequency);
            if (enclosingTones[1] == null) // frequency is below lowest tone
                return frequencySlider.getMinimum();
            if (enclosingTones[0] == null) // frequency is above highest tone
                return frequencySlider.getMaximum();
            
            tone = enclosingTones[0]; // take lower plus micro-tones
            
            final double difference = enclosingTones[1].frequency - tone.frequency;
            final double lowerPart = frequency - tone.frequency;
            final double fraction = lowerPart / difference;
            
            numberOfMicroTones = (int) Math.round(MICROTONES * fraction);
        }
        
        // MIDI-number provides a non-exponential scale for the slider
        return (tone.midiNumber * MICROTONES) + numberOfMicroTones;
    }
}