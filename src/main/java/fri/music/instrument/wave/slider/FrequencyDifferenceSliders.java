package fri.music.instrument.wave.slider;

import java.awt.Color;
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
import javax.swing.JSlider;
import javax.swing.JTextField;
import javax.swing.SwingConstants;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import fri.music.JustIntonation;
import fri.music.MathUtils;
import fri.music.Tone;
import fri.music.ToneSystem;
import fri.music.swingutils.ButtonUtil;
import fri.music.swingutils.text.HelpWindowSingleton;

/**
 * Displays two frequency sliders and their difference-tone.
 * Optionally also secondary and tertiary difference-tone can be viewed.
 * Sound can be turned on and off for each slider.
 * The sliders render note names and their frequencies.
 */
public class FrequencyDifferenceSliders extends AbstractFrequencySliders
{
    private SliderPanel frequencyPanel1;
    private SliderPanel frequencyPanel2;
    private DifferenceSliderPanel differenceFrequencyPanel;
    private ChangeListener frequencySliderInfoListener; // stored here for recreation
    private JustIntonation.Interval[] pureIntervals;

    /** Render tone range C3 - C8. */
    public FrequencyDifferenceSliders() {
        this(null);
    }
    
    /** Render given tone range. */
    public FrequencyDifferenceSliders(ToneSystem toneSystem) {
        super(toneSystem);
    }
    
    @Override
    protected void initializeTones(ToneSystem toneSystem) {
        super.initializeTones(toneSystem);
        initializeJustIntervals(toneSystem);
    }
    
    @Override
    protected ActionListener createHelpActionListener() {
        return (ActionEvent event) -> HelpWindowSingleton.start(panel, "Difference-Tone Frequency Sliders", HelpForFrequencyDifferenceSliders.URL);
    }
    
    private void initializeJustIntervals(ToneSystem toneSystem) {
        final JustIntonation.ChromaticScale chromaticScale = toneSystem instanceof JustIntonation
                ? ((JustIntonation) toneSystem).chromaticScale
                : JustIntonation.ChromaticScales.LIMIT_5_SYMMETRIC_1;
        
        final JustIntonation.Interval[] intervals = chromaticScale.intervals();
        pureIntervals = new JustIntonation.Interval[intervals.length + 1];
        pureIntervals[0] = JustIntonation.Intervals.UNISON;
        System.arraycopy(intervals, 0, pureIntervals, 1, intervals.length);
    }
    
    @Override
    protected SliderPanel[] getSliderPanels() {
        return new SliderPanel[] {
            frequencyPanel1,
            frequencyPanel2,
            differenceFrequencyPanel
        };
    }
    
    @Override
    protected JPanel[] createFrequencySliderPanels(JSlider amplitudeSlider, JSlider gainSlider, JComboBox<String> waveChoice) {
        frequencyPanel1 = new SliderPanel(tones(), "Frequency", "1", amplitudeSlider, gainSlider, waveChoice);
        frequencyPanel1.setNote("D6"); // initially create a major second to generate C3 as difference tone
        frequencyPanel2 = new SliderPanel(tones(), "Frequency", "2", amplitudeSlider, gainSlider, waveChoice);
        //frequencyPanel1.setNote("C6"); // is default
        differenceFrequencyPanel = new DifferenceSliderPanel(tones(), "Difference", "1-2", amplitudeSlider, gainSlider, waveChoice);
        
        return new JPanel[] {
            frequencyPanel1.sliderPanel,
            frequencyPanel2.sliderPanel,
            differenceFrequencyPanel.sliderPanel,
        };
    }
    
    private boolean differencePanelIsExpanded;
    
    @Override
    protected void beforeRecreateFrequencySliderPanels() {
        frequencyPanel1.removeChangeListener(frequencySliderInfoListener);
        frequencyPanel2.removeChangeListener(frequencySliderInfoListener);
        
        differencePanelIsExpanded = differenceFrequencyPanel.isExpanded();
    }
    
    @Override
    protected boolean shouldRestoreSliderState(SliderPanel newSliderPanel) {
        return newSliderPanel == frequencyPanel1 || newSliderPanel == frequencyPanel2;
    }
    
    @Override
    protected void afterRecreateFrequencySliderPanels() {
        frequencyPanel1.addChangeListener(frequencySliderInfoListener);
        frequencyPanel2.addChangeListener(frequencySliderInfoListener);
        
        frequencySliderInfoListener.stateChanged(null); // update info display
        
        differenceFrequencyPanel.setExpanded(differencePanelIsExpanded);
    }
    
    @Override
    protected JComponent createInfoPanel() {
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
        final JTextField enterFractionTextField = createTextField(110, "Enter Fraction:", true);
        enterFractionTextField.setText("/");
        enterFractionTextField.setToolTipText("Changes Frequency 1 on ENTER-Key");
        
        // handle cursor keys or manual note input
        final KeyListener keyboardListener = new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent e) {
                final boolean down = (e.getKeyCode() == KeyEvent.VK_DOWN);
                final boolean up   = (e.getKeyCode() == KeyEvent.VK_UP);
                if (up || down) {
                    final Tone currentTone = tones().forIpnName(((JTextField) e.getSource()).getText());
                    final Tone nextTone;
                    if (currentTone != null) {
                        nextTone = down ? tones().getNextLower(currentTone) : tones().getNextUpper(currentTone);
                    }
                    else {
                        final Tone[] enclosingTones = tones().getEnclosingTones(findSource(e).getValue());
                        nextTone = down ? enclosingTones[0] : enclosingTones[1];
                    }
                    if (nextTone != null)
                        findSource(e).setValue(nextTone.frequency);
                }
            }
            @Override
            public void keyReleased(KeyEvent e) {
                final boolean matters = 
                        (e.getKeyCode() >= KeyEvent.VK_A && e.getKeyCode() <= KeyEvent.VK_G) ||
                        (e.getKeyCode() >= KeyEvent.VK_1 && e.getKeyCode() <= KeyEvent.VK_9);
                if (matters) {
                    final Tone tone = tones().forIpnName(((JTextField) e.getSource()).getText());
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
        frequencySliderInfoListener = new ChangeListener() {
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
                
                final String enteredFraction = enterFractionTextField.getText().replace(" ", "");
                final String detectedFraction = fractionTextField.getText().replace(" ", "");
                if (detectedFraction.length() > 0 && enteredFraction.equals(detectedFraction) == false)
                    enterFractionTextField.setText(detectedFraction);
            }
        };
        frequencySliderInfoListener.stateChanged(null); // display current frequency fraction
        
        frequencyPanel1.addChangeListener(frequencySliderInfoListener);
        frequencyPanel2.addChangeListener(frequencySliderInfoListener);
        
        final JButton startStopBoth = createStartStopBothButton();
        // handle "Play 1+2" button
        startStopBoth.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                final boolean stop = (frequencyPanel1.isPlaying() || frequencyPanel2.isPlaying());
                if (stop) {
                    startStopBoth.setForeground(Color.BLUE);
                    if (frequencyPanel1.isPlaying() == true)
                        ButtonUtil.doClick(frequencyPanel1.startStop);
                    if (frequencyPanel2.isPlaying() == true)
                        ButtonUtil.doClick(frequencyPanel2.startStop);
                }
                else if (frequencyPanel1.getValue() > 0.0 && frequencyPanel2.getValue() > 0.0) {
                    startStopBoth.setForeground(Color.RED);
                    ButtonUtil.doClick(frequencyPanel1.startStop);
                    ButtonUtil.doClick(frequencyPanel2.startStop);
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
    
    
    
    // callback methods
    
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
            fractionDisplay = fraction[0]+"/"+fraction[1];
        
        return new String[] { fractionDisplay, ""+octave, nearestInterval };
    }
    
    private String nearestInterval(long dividend, long divisor, long octave) {
        // reduce fraction to range of one octave
        while (dividend > 2 * divisor)
            divisor *= 2;
        
        double centMinimum = Double.MAX_VALUE;
        JustIntonation.Interval nearestInterval = null;
        for (JustIntonation.Interval interval : pureIntervals) {
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