package fri.music.instrument.configuration;

import java.awt.FlowLayout;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JTextField;
import fri.music.Tone;
import fri.music.ToneSystem;
import fri.music.differencetones.DifferenceTones;
import fri.music.instrument.TuningComponent;
import fri.music.instrument.wave.DeviationComponent;
import fri.music.instrument.wave.IntervalRangeComponent;
import fri.music.utils.StringUtil;
import fri.music.utils.swing.BorderUtil;
import fri.music.utils.swing.text.TextFieldUtil;

/**
 * Belongs to TuningsAndPurityCheckLauncher.
 * Helper class that contains the configuration
 * for difference-tone and intervals text display.
 */
class DifferenceTonesConfigurationPanel
{
    private static KeyListener toneNameListener;
    
    public final JPanel panel;
    public final JButton differenceTonesButton;
    public final JButton intervalsButton;
    public final JButton allIntervalsOfScaleButton;
    
    private final TuningComponent tuning;
    private final FrequencyOfA4Component frequencyOfA4;
    private final DeviationComponent deviation;
    
    private final JTextField lowerIntervalTone;
    private final JTextField upperIntervalTone;
    private final JCheckBox onlyPrimaryDifferenceTone;
    
    private final JTextField differenceTone;
    private final IntervalRangeComponent intervalRange;

    public DifferenceTonesConfigurationPanel() {
        // global configuration panel
        final JPanel tuningPanel = new JPanel();
        tuningPanel.setLayout(new BoxLayout(tuningPanel, BoxLayout.Y_AXIS));
        
        this.tuning = new TuningComponent();
        tuningPanel.add(tuning.getLeftAlignedChoice());
        
        this.frequencyOfA4 = new FrequencyOfA4Component();
        tuningPanel.add(frequencyOfA4.frequencySlider);
        
        this.deviation = new DeviationComponent(DifferenceTones.DEFAULT_DEVIATION, false);
        tuningPanel.add(deviation.deviationSlider);
        
        // upper panel
        final JPanel differenceTonesPanel = new JPanel();
        differenceTonesPanel.setLayout(new BoxLayout(differenceTonesPanel, BoxLayout.Y_AXIS));
        differenceTonesPanel.setBorder(BorderUtil.titledBorder("Difference-Tones of Interval", 4f, 3));
        
        this.lowerIntervalTone = configureToneField("Tone 1", "C6");
        this.upperIntervalTone = configureToneField("Tone 2", "D6");
        final JPanel textFieldsPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
        textFieldsPanel.add(lowerIntervalTone);
        textFieldsPanel.add(upperIntervalTone);
        differenceTonesPanel.add(textFieldsPanel);
        
        this.onlyPrimaryDifferenceTone = new JCheckBox("Display Only Primary", true);
        onlyPrimaryDifferenceTone.setToolTipText("Do Not Show Secondary and Tertiary Difference-Tones");
        onlyPrimaryDifferenceTone.setAlignmentX(JComponent.CENTER_ALIGNMENT);
        
        this.differenceTonesButton = new JButton("Display Difference-Tone(s)");
        differenceTonesButton.setAlignmentX(JComponent.CENTER_ALIGNMENT);
        
        final JPanel differenceToneButtonPanel = new JPanel();
        differenceToneButtonPanel.setLayout(new BoxLayout(differenceToneButtonPanel, BoxLayout.Y_AXIS));
        differenceToneButtonPanel.add(onlyPrimaryDifferenceTone);
        differenceToneButtonPanel.add(differenceTonesButton);
        
        differenceTonesPanel.add(differenceToneButtonPanel);
        differenceTonesPanel.add(Box.createVerticalGlue());
        
        // lower panel
        final JPanel intervalsPanel = new JPanel();
        intervalsPanel.setLayout(new BoxLayout(intervalsPanel, BoxLayout.Y_AXIS));
        intervalsPanel.setBorder(BorderUtil.titledBorder("Intervals of Difference-Tone", 4f, 3));
        
        this.differenceTone = configureToneField("Difference-Tone", "C3", 120);
        differenceTone.setAlignmentX(JComponent.CENTER_ALIGNMENT);
        intervalsPanel.add(differenceTone);
        
        this.intervalRange = new IntervalRangeComponent();
        final JPanel choicePanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
        intervalRange.getNarrowestChoice().setSelectedItem(ToneSystem.MAJOR_SECOND);
        intervalRange.getWidestChoice().setSelectedItem(ToneSystem.MINOR_SEVENTH);
        choicePanel.add(intervalRange.getNarrowestChoice());
        choicePanel.add(intervalRange.getWidestChoice());
        
        this.intervalsButton = new JButton("Display Intervals");
        this.allIntervalsOfScaleButton = new JButton("All of Scale");
        final JPanel intervalsButtonPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
        intervalsButtonPanel.add(intervalsButton);
        intervalsButtonPanel.add(allIntervalsOfScaleButton);
        
        intervalsPanel.add(choicePanel);
        intervalsPanel.add(intervalsButtonPanel);
        intervalsPanel.add(Box.createVerticalGlue()); // after last vertical component
        
        // build together all
        this.panel = new JPanel();
        this.panel.setLayout(new BoxLayout(this.panel, BoxLayout.Y_AXIS));
        this.panel.add(tuningPanel);
        this.panel.add(Box.createVerticalGlue());
        this.panel.add(differenceTonesPanel);
        this.panel.add(intervalsPanel);
    }
    
    // global methods
    
    public ToneSystem getToneSystem() {
        tuning.setFrequencyOfA4(frequencyOfA4.getValue());
        return tuning.getTuning();
    }
    public double getDeviation() {
        return deviation.getDeviation();
    }
    public int getDeviationPercent() {
        return deviation.deviationSlider.getValue();
    }
    
    // difference tone methods
    
    public String getDifferenceTonesTitle() {
        return getLowerToneIpnName()+"-"+getUpperToneIpnName()+
                " Difference-Tone"+(showOnlyPrimaryDifferenceTone() ? "" : "s");
    }
    public String getDifferenceTonesHeadingLines(Tone lowerTone, Tone upperTone) {
        return getGlobalHeadingLines()+
            "Interval:   "+lowerTone.ipnName+" ("+lowerTone.formattedFrequency()+") - "
                          +upperTone.ipnName+" ("+upperTone.formattedFrequency()+")"+StringUtil.NEWLINE+
            "Difference: "+(Tone.frequencyFormat.format(Math.abs(upperTone.frequency - lowerTone.frequency)))+StringUtil.NEWLINE;
    }
    public boolean showOnlyPrimaryDifferenceTone() {
        return onlyPrimaryDifferenceTone.isSelected();
    }
    public String getUpperToneIpnName() {
        return upperIntervalTone.getText().toUpperCase();
    }
    public String getLowerToneIpnName() {
        return lowerIntervalTone.getText().toUpperCase();
    }
    
    // intervals methods
    
    public String getIntervalsTitle() {
        return getDifferenceToneIpnName()+" Intervals";
    }
    public String getIntervalHeadingLines(Tone differenceTone) {
        return getGlobalHeadingLines()+
            "Tone:       "+differenceTone.ipnName+" ("+differenceTone.formattedFrequency()+")"+StringUtil.NEWLINE;
    }
    public String getDifferenceToneIpnName() {
        return differenceTone.getText().toUpperCase();
    }
    public String getNarrowestInterval() {
        return (String) intervalRange.getNarrowestChoice().getSelectedItem();
    }
    public String getWidestInterval() {
        return (String) intervalRange.getWidestChoice().getSelectedItem();
    }
    public String getGlobalHeadingLines() {
        return 
            "Tuning:     "+tuning.getChoice(null).getSelectedItem()+StringUtil.NEWLINE+
            "A4:         "+frequencyOfA4.getValue()+" Hertz"+StringUtil.NEWLINE+
            "Deviation:  "+deviation.deviationSlider.getValue()+" %"+StringUtil.NEWLINE;
    }
    
    // privates
    
    private JTextField configureToneField(String title, String content) {
        return configureToneField(title, content, 90);
    }
    private JTextField configureToneField(String title, String content, int pixelWidth) {
        final JTextField toneField = TextFieldUtil.sizedField(pixelWidth, title, true);
        toneField.setText(content);
        toneField.addKeyListener(keyListener());
        toneField.setToolTipText("Use CURSOR-UP and CURSOR-DOWN Keys to Scroll to the Next Available Tone");
        return toneField;
    }

    private static KeyListener keyListener() {
        if (toneNameListener != null)
            return toneNameListener; // is reusable for multiple instances
        
        return toneNameListener = new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent e) {
                final boolean down = (e.getKeyCode() == KeyEvent.VK_DOWN);
                if (down || e.getKeyCode() == KeyEvent.VK_UP) {
                    final String ipnNameWithOctave = textField(e).getText().toUpperCase();
                    final String ipnBaseName = StringUtil.getUntilFirstNumber(ipnNameWithOctave);
                    int octave = StringUtil.getFirstNumber(ipnNameWithOctave);
                    
                    if (ipnBaseName.length() > 0 && octave > -1) {
                        final String nextIpnBaseName = getNext(ipnBaseName, down);
                        
                        if (down == false && nextIpnBaseName.equals(ToneSystem.IPN_BASE_NAMES[0]))
                            octave++;
                        else if (down == true && ipnBaseName.equals(ToneSystem.IPN_BASE_NAMES[0]))
                            octave--;
                        
                        if (octave >= ToneSystem.LOWEST_OCTAVE && 
                                (octave < ToneSystem.MAXIMUM_OCTAVES || nextIpnBaseName.equals(ToneSystem.IPN_BASE_NAMES[0])))
                            textField(e).setText(nextIpnBaseName + octave);
                    }
                }
            }
            
            @Override
            public void keyTyped(KeyEvent e) {
                final String text = textField(e).getText();
                final boolean matters = 
                        text.length() <= 2 && // new character not yet added, maximum is "C10", there is no "C#10"
                            ((e.getKeyChar() >= 'a' && e.getKeyChar() <= 'g') ||
                            (e.getKeyChar() >= 'A' && e.getKeyChar() <= 'G') ||
                            e.getKeyChar() == '#' ||
                            (e.getKeyChar() >= '0' && e.getKeyChar() <= '9'));
                if (matters == false)
                    e.consume();
            }
            
            private JTextField textField(KeyEvent event) {
                return (JTextField) event.getSource();
            }
            
            private String getNext(String ipnBaseName, boolean down) {
                for (int i = 0; i < ToneSystem.IPN_BASE_NAMES.length; i++) {
                    if (ToneSystem.IPN_BASE_NAMES[i].equals(ipnBaseName)) {
                        int index = down ? (i - 1) : (i + 1);
                        if (index < 0)
                            index = ToneSystem.IPN_BASE_NAMES.length - 1;
                        else if (index >= ToneSystem.IPN_BASE_NAMES.length)
                            index = 0;
                        return ToneSystem.IPN_BASE_NAMES[index];
                    }
                }
                throw new IllegalArgumentException("Unknown note: "+ipnBaseName);
            }
        };  // end KeyListener
    }
}   // end class DifferenceTonesConfiguration