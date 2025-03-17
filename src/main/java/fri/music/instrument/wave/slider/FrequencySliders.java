package fri.music.instrument.wave.slider;

import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSlider;
import javax.swing.JTextField;
import javax.swing.SwingConstants;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import fri.music.JustIntonation;
import fri.music.MathUtils;
import fri.music.Tone;
import fri.music.ToneSystem;
import fri.music.Tones;
import fri.music.instrument.SmartComboBox;
import fri.music.instrument.wave.TuningComponent;
import fri.music.wavegenerator.SineWaveGenerator;
import fri.music.wavegenerator.WaveGenerator;

public class FrequencySliders
{
    public static final String RECOMMENDED_LOWEST = "C3";
    public static final int RECOMMENDED_OCTAVES = 5;
    
    /** Contains all of the UI. */
    public final JComponent panel;
    
    private SliderPanel frequencyPanel1;
    private SliderPanel frequencyPanel2;
    private DifferenceSliderPanel differenceFrequencyPanel;
    
    private ChangeListener frequencySliderListener;
    
    private Tones tones;
    private JustIntonation.Interval[] justIntervals;

    /** Render tone range C3 - C8. */
    public FrequencySliders() {
        this(new JustIntonation(RECOMMENDED_LOWEST, RECOMMENDED_OCTAVES));
    }
    
    public FrequencySliders(ToneSystem toneSystem) {
        tones = new Tones(toneSystem.tones());
        
        initializeJustIntervals(toneSystem);
        
        final JSlider amplitudeSlider = createAmplitudeSlider();
        final JSlider gainSlider = createGainSlider();
        
        final JComboBox<String> waveChoice = createWaveChoice();
        final JComboBox<String> tuningChoice = createTuningChoice(toneSystem, amplitudeSlider, gainSlider, waveChoice);
        
        createFrequencySliderPanels(amplitudeSlider, gainSlider, waveChoice);
        
        final JComponent infoPanel = createInfoPanel();
        
        panel = new JScrollPane(
                layoutMainPanel(amplitudeSlider, gainSlider, waveChoice, tuningChoice, infoPanel));
    }

    /** Ends playing sounds and releases resources. */
    public void close() {
        frequencyPanel1.close();
        frequencyPanel2.close();
        differenceFrequencyPanel.close();
    }

    private void initializeJustIntervals(ToneSystem toneSystem) {
        final JustIntonation.ChromaticScale chromaticScale = toneSystem instanceof JustIntonation
                ? ((JustIntonation) toneSystem).chromaticScale
                : JustIntonation.ChromaticScales.LIMIT_5_SYMMETRIC_1;
        
        final JustIntonation.Interval[] intervals = chromaticScale.intervals();
        justIntervals = new JustIntonation.Interval[intervals.length + 1];
        justIntervals[0] = JustIntonation.Intervals.UNISON;
        System.arraycopy(intervals, 0, justIntervals, 1, intervals.length);
    }

    private JSlider createAmplitudeSlider() {
        final JSlider amplitudeSlider = new JSlider();
        amplitudeSlider.setPaintLabels(true);
        amplitudeSlider.setPaintTicks(true);
        amplitudeSlider.setMajorTickSpacing(10);
        amplitudeSlider.setMinimum(0);
        amplitudeSlider.setMaximum(127);
        amplitudeSlider.setValue(7);
        final ChangeListener changeListener = new ChangeListener() {
            @Override
            public void stateChanged(ChangeEvent e) {
                amplitudeSlider.setBorder(BorderFactory.createTitledBorder("Amplitude: "+amplitudeSlider.getValue()));
            }
        };
        amplitudeSlider.addChangeListener(changeListener);
        changeListener.stateChanged(null);
        return amplitudeSlider;
    }

    private JSlider createGainSlider() {
        final JSlider gainSlider = new JSlider();
        final WaveGenerator helper = new SineWaveGenerator();
        gainSlider.setPaintLabels(true);
        gainSlider.setPaintTicks(true);
        gainSlider.setMajorTickSpacing(5);
        gainSlider.setMinimum((int) helper.getMinimumGain());
        gainSlider.setMaximum((int) helper.getMaximumGain());
        gainSlider.setValue((int) helper.getGain());
        gainSlider.setPreferredSize(new Dimension(400, 60));
        final ChangeListener changeListener = new ChangeListener() {
            @Override
            public void stateChanged(ChangeEvent e) {
                gainSlider.setBorder(BorderFactory.createTitledBorder("Gain (Decibels): "+gainSlider.getValue()));
            }
        };
        gainSlider.addChangeListener(changeListener);
        changeListener.stateChanged(null);
        return gainSlider;
    }

    private JComboBox<String> createWaveChoice() {
        final String[] items = new String[] { "Sine", /*"Sawtooth", "Square",*/ "Triangle" };
        final JComboBox<String> waveChoice = new SmartComboBox(items);
        waveChoice.setBorder(BorderFactory.createTitledBorder("Wave Form"));
        waveChoice.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                final SliderPanel[] panels = 
                        new SliderPanel[] { frequencyPanel1, frequencyPanel2, differenceFrequencyPanel };
                for (SliderPanel panel : panels) {
                    if (panel.isPlaying()) { // is running
                        panel.startStop.doClick(); // stop
                        panel.startStop.doClick(); // restart with new wave form
                    }
                }
            }
        });
        return waveChoice;
    }

    private JComboBox<String> createTuningChoice(
            ToneSystem toneSystem,
            final JSlider amplitudeSlider, 
            final JSlider gainSlider, 
            final JComboBox<String> waveChoice)
    {
        final TuningComponent.Listener listener = new TuningComponent.Listener() {
            @Override
            public void tuningChanged(ToneSystem toneSystem) {
                tones = new Tones(toneSystem.tones());
                initializeJustIntervals(toneSystem);
                recreateFrequencySliderPanels(amplitudeSlider, gainSlider, waveChoice);
            }
        };
        final TuningComponent tuning = new TuningComponent(
                tones.getLowest().ipnName, 
                tones.getOctaves(), 
                null, 
                listener);
        return tuning.getToneSystemChoice(toneSystem);
    }

    private void createFrequencySliderPanels(JSlider amplitudeSlider, JSlider gainSlider, JComboBox<String> waveChoice) {
        frequencyPanel1 = new SliderPanel(tones, "Frequency", "1", amplitudeSlider, gainSlider, waveChoice);
        frequencyPanel1.setNote("D6"); // initially create a major second to generate C3 as difference tone
        frequencyPanel2 = new SliderPanel(tones, "Frequency", "2", amplitudeSlider, gainSlider, waveChoice);
        //frequencyPanel1.setNote("C6"); // is default
        differenceFrequencyPanel = new DifferenceSliderPanel(tones, "Difference", "1-2", amplitudeSlider, gainSlider, waveChoice);
    }
    
    private JComponent createInfoPanel() {
        final JTextField note1TextField = createTextField(60, "Note 1", true);
        final JTextField note2TextField = createTextField(60, "Note 2", true);
        final JTextField note3TextField = createTextField(70, "Note 1-2");
        final JTextField frequency1TextField = createTextField(110, "Frequency 1");
        final JTextField frequency2TextField = createTextField(110, "Frequency 2");
        final JTextField frequency3TextField = createTextField(110, "Frequency 1-2");
        final JTextField fractionTextField = createTextField(110, "Pure Fraction");
        final JTextField octavesTextField = createTextField(70, "Octaves");
        final JTextField nearestIntervalTextField = createTextField(190, "Nearest Pure Interval");
        nearestIntervalTextField.setHorizontalAlignment(SwingConstants.LEFT);
        final JTextField enterFractionTextField = createTextField(110, "Enter Fraction", true);
        enterFractionTextField.setText("/");
        enterFractionTextField.setToolTipText("Changes Frequency 1 on ENTER-Key");
        
        // handle cursor keys or manual note input
        final KeyListener keyboardListener = new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent e) {
                final boolean down = (e.getKeyCode() == KeyEvent.VK_DOWN);
                final boolean up   = (e.getKeyCode() == KeyEvent.VK_UP);
                if (up || down) {
                    final Tone currentTone = tones.forIpnName(((JTextField) e.getSource()).getText());
                    final Tone nextTone;
                    if (currentTone != null) {
                        nextTone = down ? tones.getNextLower(currentTone) : tones.getNextUpper(currentTone);
                    }
                    else {
                        final Tone[] enclosingTones = tones.getEnclosingTones(findSource(e).getValue());
                        nextTone = down ? enclosingTones[0] : enclosingTones[1];
                    }
                    if (nextTone != null)
                        findSource(e).setValue(nextTone.frequency);
                }
            }
            public void keyReleased(KeyEvent e) {
                final boolean matters = 
                        (e.getKeyCode() >= KeyEvent.VK_A && e.getKeyCode() <= KeyEvent.VK_G) ||
                        (e.getKeyCode() >= KeyEvent.VK_1 && e.getKeyCode() <= KeyEvent.VK_9);
                if (matters) {
                    final Tone tone = tones.forIpnName(((JTextField) e.getSource()).getText());
                    if (tone != null)
                        findSource(e).setValue(tone.frequency);
                }
            }
            private SliderPanel findSource(KeyEvent e) {
                return (e.getSource() == note1TextField) ? frequencyPanel1 : frequencyPanel2;
            }
        };
        note1TextField.addKeyListener(keyboardListener);
        note2TextField.addKeyListener(keyboardListener);
        
        enterFractionTextField.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                final JTextField textField = (JTextField) e.getSource();
                textField.setForeground(Color.BLACK); // could have been in error state
                final String fraction = textField.getText();
                final int breakLine = fraction.indexOf("/");
                if (breakLine < 0)
                    return;
                final String string1 = fraction.substring(0, breakLine).strip();
                final String string2 = fraction.substring(breakLine + 1).strip();
                try {
                    final long dividend = Long.valueOf(string1);
                    final long divisor = Long.valueOf(string2);
                    final double frequency2 = frequencyPanel2.getValue();
                    final double frequency = frequency2 * ((double) dividend / (double) divisor);
                    frequencyPanel1.setValue(frequency);
                }
                catch (NumberFormatException ex) {
                    System.err.println("Please enter a valid fraction! Could not parse '"+fraction+"'");
                    textField.setForeground(Color.RED);
                }
            }
        });
        
        // handle changes in sliders
        frequencySliderListener = new ChangeListener() {
            @Override
            public void stateChanged(ChangeEvent e) {
                final double frequency1 = frequencyPanel1.getValue();
                final double frequency2 = frequencyPanel2.getValue();
                
                differenceFrequencyPanel.setValue(frequency1, frequency2);
                
                note1TextField.setText(frequencyPanel1.getNote());
                note2TextField.setText(frequencyPanel2.getNote());
                note3TextField.setText(differenceFrequencyPanel.getNote());
                
                frequency1TextField.setText(Tone.frequencyFormat.format(frequency1));
                frequency2TextField.setText(Tone.frequencyFormat.format(frequency2));
                final double difference = differenceFrequencyPanel.getValue();
                frequency3TextField.setText(difference > 0.0 ? Tone.frequencyFormat.format(difference) : "");
                
                final String[] intervalInfo = getIntervalInfo();
                fractionTextField.setText(intervalInfo[0]);
                octavesTextField.setText(intervalInfo[1]);
                nearestIntervalTextField.setText(intervalInfo[2]);
                nearestIntervalTextField.setCaretPosition(0);
            }
        };
        frequencySliderListener.stateChanged(null); // display current frequency fraction
        
        frequencyPanel1.addChangeListener(frequencySliderListener);
        frequencyPanel2.addChangeListener(frequencySliderListener);
        
        final JButton startStopBoth = createStartStopBothButton();
        // handle "Play 1+2" button
        startStopBoth.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                final boolean stop = (frequencyPanel1.isPlaying() || frequencyPanel2.isPlaying());
                if (stop) {
                    startStopBoth.setForeground(Color.BLUE);
                    if (frequencyPanel1.isPlaying() == true)
                        frequencyPanel1.startStop.doClick();
                    if (frequencyPanel2.isPlaying() == true)
                        frequencyPanel2.startStop.doClick();
                }
                else if (frequencyPanel1.getValue() > 0.0 && frequencyPanel2.getValue() > 0.0) {
                    startStopBoth.setForeground(Color.RED);
                    frequencyPanel1.startStop.doClick();
                    frequencyPanel2.startStop.doClick();
                }
            }
        });
        
        final JPanel panel = new JPanel();
        panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));
        panel.setBorder(BorderFactory.createEmptyBorder(0, 20, 0, 20));
        
        panel.add(startStopBoth);
        panel.add(Box.createHorizontalGlue());
        panel.add(note1TextField);
        panel.add(note2TextField);
        panel.add(note3TextField);
        panel.add(frequency1TextField);
        panel.add(frequency2TextField);
        panel.add(frequency3TextField);
        panel.add(Box.createHorizontalGlue());
        panel.add(octavesTextField);
        panel.add(nearestIntervalTextField);
        panel.add(fractionTextField);
        panel.add(enterFractionTextField);
        
        return panel;
    }

    private JTextField createTextField(int width, String title) {
        return createTextField(width, title, false);
    }
    
    private JTextField createTextField(int width, String title, boolean editable) {
        final JTextField textField = new JTextField();
        textField.setEditable(editable);
        textField.setFont(new Font(Font.MONOSPACED, Font.BOLD, 16));
        textField.setHorizontalAlignment(SwingConstants.CENTER);
        textField.setBorder(BorderFactory.createTitledBorder(
                BorderFactory.createLineBorder(Color.LIGHT_GRAY, 3, true), 
                title));
        final Dimension size = new Dimension(width, 48);
        textField.setPreferredSize(size);
        textField.setMaximumSize(size);
        return textField;
    }

    private JButton createStartStopBothButton() {
        final JButton startStopBoth = new JButton(SliderPanel.START_LABEL+" / "+SliderPanel.STOP_LABEL+" 1+2");
        startStopBoth.setForeground(Color.BLUE);
        startStopBoth.setPreferredSize(SliderPanel.STARTSTOP_BUTTON_SIZE);
        startStopBoth.setMaximumSize(SliderPanel.STARTSTOP_BUTTON_SIZE);
        return startStopBoth;
    }
    
    
    private JComponent layoutMainPanel(
            JSlider amplitudeSlider, 
            JSlider gainSlider, 
            JComboBox<String> waveChoice, 
            JComboBox<String> tuningChoice, 
            JComponent infoPanel)
    {
        final JPanel frequenciesPanel = new JPanel();
        frequenciesPanel.setBorder(BorderFactory.createCompoundBorder(
                        BorderFactory.createEmptyBorder(10, 10, 10, 10),
                        BorderFactory.createRaisedSoftBevelBorder()));
        frequenciesPanel.setLayout(new BoxLayout(frequenciesPanel, BoxLayout.Y_AXIS));
        frequenciesPanel.add(frequencyPanel1.sliderPanel);
        frequenciesPanel.add(frequencyPanel2.sliderPanel);
        frequenciesPanel.add(differenceFrequencyPanel.sliderPanel);
        
        final JPanel settingsPanel = new JPanel();
        settingsPanel.setLayout(new BoxLayout(settingsPanel, BoxLayout.X_AXIS));
        settingsPanel.add(tuningChoice);
        settingsPanel.add(waveChoice);
        settingsPanel.add(amplitudeSlider);
        settingsPanel.add(gainSlider);

        final JPanel infoAndSettingsPanel = new JPanel();
        infoAndSettingsPanel.setLayout(new BoxLayout(infoAndSettingsPanel, BoxLayout.Y_AXIS));
        infoAndSettingsPanel.add(settingsPanel);
        infoAndSettingsPanel.add(infoPanel);
        
        final JPanel mainPanel = new JPanel();
        mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));
        mainPanel.add(infoAndSettingsPanel);
        mainPanel.add(frequenciesPanel);
        mainPanel.add(Box.createVerticalGlue()); // TODO: doesn't work, frequency-panels still stretch!
        
        return mainPanel;
    }
    
    
    // callback methods
    
    private void recreateFrequencySliderPanels(JSlider amplitudeSlider, JSlider gainSlider, JComboBox<String> waveChoice) {
        final double frequency1 = frequencyPanel1.getValue();
        final double frequency2 = frequencyPanel2.getValue();
        final boolean isPlaying1 = frequencyPanel1.isPlaying();
        final boolean isPlaying2 = frequencyPanel2.isPlaying();
        final boolean isPlaying3 = differenceFrequencyPanel.isPlaying();
        final Container parent = frequencyPanel1.sliderPanel.getParent();
        
        frequencyPanel1.removeChangeListener(frequencySliderListener);
        frequencyPanel2.removeChangeListener(frequencySliderListener);
        parent.remove(frequencyPanel1.sliderPanel);
        parent.remove(frequencyPanel2.sliderPanel);
        parent.remove(differenceFrequencyPanel.sliderPanel);
        frequencyPanel1.close();
        frequencyPanel2.close();
        differenceFrequencyPanel.close(); // could be playing
        
        createFrequencySliderPanels(amplitudeSlider, gainSlider, waveChoice);
        
        parent.add(frequencyPanel1.sliderPanel);
        parent.add(frequencyPanel2.sliderPanel);
        parent.add(differenceFrequencyPanel.sliderPanel);
        
        frequencyPanel1.addChangeListener(frequencySliderListener);
        frequencyPanel2.addChangeListener(frequencySliderListener);
        
        frequencyPanel1.setValue(frequency1);
        frequencyPanel2.setValue(frequency2);
        
        frequencySliderListener.stateChanged(null);
        
        parent.revalidate(); // repaints UI
        
        if (isPlaying1)
            frequencyPanel1.startStop.doClick();
        if (isPlaying2)
            frequencyPanel2.startStop.doClick();
        if (isPlaying3)
            differenceFrequencyPanel.startStop.doClick();
    }
    
    /** @return fraction, octave and nearest interval, on slider move. */
    private String[] getIntervalInfo() {
        double frequency1 = frequencyPanel1.getValue();
        double frequency2 = frequencyPanel2.getValue();
        if (frequency1 <= 0.0 || frequency2 <= 0.0)
            return new String[] { "", "", "" };
        
        if (frequency1 < frequency2) { // 1st slider value is below 2nd slider value
            double swap = frequency1;
            frequency1 = frequency2;
            frequency2 = swap;
        }
        
        final double division = frequency1 / frequency2; // always greater zero
        final long[] fraction = MathUtils.toFraction(division);
        final long octave = (fraction[0] / fraction[1]) - 1;
        final String nearestInterval = nearestInterval(fraction[0], fraction[1], octave);
        
        final boolean isPureInterval = nearestInterval.startsWith(">");
        String fractionDisplay = "";
        if (isPureInterval)
            fractionDisplay = fraction[0]+" / "+fraction[1];
        
        return new String[] { fractionDisplay, ""+octave, nearestInterval };
    }
    
    private String nearestInterval(long dividend, long divisor, long octave) {
        // reduce fraction to range of one octave
        while (dividend > 2 * divisor)
            divisor *= 2;
        
        double centMinimum = Double.MAX_VALUE;
        JustIntonation.Interval nearestInterval = null;
        for (JustIntonation.Interval interval : justIntervals) {
            final double centDistance = centDistance(dividend, divisor, interval);
            if (centDistance < centMinimum) {
                centMinimum = centDistance;
                nearestInterval = interval;
            }
        }
        
        // correct the wrong UNISON when in different octave, caused by divisor *= 2
        if (octave != 0 && nearestInterval == JustIntonation.Intervals.UNISON)
            nearestInterval = JustIntonation.Intervals.OCTAVE;
        
        String intervalName = nearestInterval.name();
        // make interval names more readable
        while (intervalName.endsWith("_") || Character.isDigit(intervalName.charAt(intervalName.length() - 1)))
            intervalName = intervalName.substring(0, intervalName.length() - 1);
        if (intervalName.endsWith("_AUG") || intervalName.endsWith("_DIM"))
            intervalName = intervalName.substring(0, intervalName.length() - "_AUG".length());
        
        // mark the exact center of the interval range
        if (centMinimum == 0.0)
            intervalName = "> "+intervalName+" <";
        else
            intervalName = "  "+intervalName+"  ";
        
        return intervalName;
    }

    private double centDistance(long dividend, long divisor, JustIntonation.Interval interval) {
        final double cent1 = JustIntonation.Interval.cent(dividend, divisor);
        final double cent2 = interval.cent(0);
        return Math.abs(cent1 - cent2);
    }
}