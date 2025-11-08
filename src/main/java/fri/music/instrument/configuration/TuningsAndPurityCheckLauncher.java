package fri.music.instrument.configuration;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import fri.music.AbstractJustIntonation.JustTone;
import fri.music.Tone;
import fri.music.ToneSystem;
import fri.music.differencetones.DifferenceToneInversions;
import fri.music.differencetones.DifferenceToneInversions.TonePair;
import fri.music.differencetones.DifferenceTones;
import fri.music.justintonation.swing.HelpForJustIntonationChecker;
import fri.music.justintonation.swing.PurityCheckConfigurationPanel;
import fri.music.utils.StringUtil;
import fri.music.utils.swing.layout.ToolBarUtil;
import fri.music.utils.swing.text.HelpWindowSingleton;
import fri.music.utils.swing.text.TextAreaUtil;

/**
 * Configuration and display launcher for tunings, 
 * difference-tones and their intervals,
 * purity checks of fraction-based tunings.
 */
public class TuningsAndPurityCheckLauncher
{
    public JComponent panel;
    
    private AllIntervalsOfScaleConfigurationPanel allIntervalsOfScaleConfiguration;
    
    public TuningsAndPurityCheckLauncher() {
        final JPanel tuningsPanel = new JPanel(new BorderLayout());
        buildTuningsPanel(tuningsPanel);
        
        final JPanel differenceTonesPanel = new JPanel(new BorderLayout());
        buildDifferenceTonesPanel(differenceTonesPanel);
        
        final JPanel checkerPanel = new JPanel(new BorderLayout());
        buildCheckerPanel(checkerPanel);
        
        final JTabbedPane tabbedPane = new JTabbedPane() {
            @Override
            public void setSelectedIndex(int index) {
                if (index == 3) // "Help" tab
                    HelpWindowSingleton.start(
                            panel, 
                            HelpForTextDisplay.TITLE, 
                            HelpForTextDisplay.URL);
                else
                    super.setSelectedIndex(index);
                    
            }
        };
        final Font font = tabbedPane.getFont(); // bigger tab titles
        tabbedPane.setFont(font.deriveFont(font.getSize2D() + 4f));
        
        tabbedPane.addTab("Tunings", tuningsPanel);
        tabbedPane.setToolTipTextAt(0, "Frequencies and Cents for Different Tunings");
        tabbedPane.addTab("Difference-Tones", differenceTonesPanel);
        tabbedPane.setToolTipTextAt(1, "Difference Tones and Intervals");
        tabbedPane.addTab("Purity Check", checkerPanel);
        tabbedPane.setToolTipTextAt(2, "Check Fraction-Based Tunings for Purity");
        tabbedPane.addTab("Help", new JLabel()); // dummy tab component
        
        this.panel = tabbedPane;
    }

    
    private void buildTuningsPanel(final JPanel tuningsPanel) {
        final ToneSystemConfigurationPanel tuningsConfigurationPanel = new ToneSystemConfigurationPanel();
        tuningsPanel.add(tuningsConfigurationPanel.panel, BorderLayout.NORTH);
        
        final ColumnDisplayConfigurationPanel displayConfiguration = new ColumnDisplayConfigurationPanel();
        final JPanel nonStretchingPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        nonStretchingPanel.add(displayConfiguration.panel);
        tuningsPanel.add(nonStretchingPanel, BorderLayout.CENTER);
        
        final JButton displayTuningButton = new JButton("Display Scale");
        displayTuningButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent event) {
                try {
                    final String title = tuningsConfigurationPanel.getToneSystem().name();
                    final String result = buildTuningDisplayText(tuningsConfigurationPanel, displayConfiguration);
                    TextAreaUtil.showResultInTextDialog(tuningsPanel, title, result, Font.MONOSPACED);
                }
                catch (Exception e) {
                    JOptionPane.showMessageDialog(tuningsPanel, e.getMessage());
                }
            }
        });
        
        final JButton help = new JButton("Help");
        help.addActionListener(event -> HelpWindowSingleton.start(
                tuningsPanel, 
                HelpForTuningsParameter.TITLE, 
                HelpForTuningsParameter.URL));
        
        final JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
        buttonPanel.add(displayTuningButton);
        buttonPanel.add(ToolBarUtil.getHelpButtonLookWrapper(help));
        tuningsPanel.add(buttonPanel, BorderLayout.SOUTH);
    }

    private void buildDifferenceTonesPanel(final JPanel differenceTonesPanel) {
        final DifferenceTonesConfigurationPanel configurationPanel = new DifferenceTonesConfigurationPanel();
        differenceTonesPanel.add(configurationPanel.panel, BorderLayout.CENTER);
        
        final ActionListener actionListener = new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent event) {
                final boolean isDifferenceTonesButton = (event.getSource() == configurationPanel.differenceTonesButton);
                try {
                    final String title = isDifferenceTonesButton
                            ? configurationPanel.getDifferenceTonesTitle()
                            : configurationPanel.getIntervalsTitle();
                    final String result = isDifferenceTonesButton
                            ? buildDifferenceTonesDisplayText(configurationPanel)
                            : buildIntervalsDisplayText(configurationPanel);
                    TextAreaUtil.showResultInTextDialog(differenceTonesPanel, title, result, Font.MONOSPACED);
                }
                catch (Exception e) {
                    JOptionPane.showMessageDialog(differenceTonesPanel, e.getMessage());
                }
            }
        };
        configurationPanel.differenceTonesButton.addActionListener(actionListener);
        configurationPanel.intervalsButton.addActionListener(actionListener);
        
        configurationPanel.allIntervalsOfScaleButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                if (allIntervalsOfScaleConfiguration == null)
                    allIntervalsOfScaleConfiguration = new AllIntervalsOfScaleConfigurationPanel(configurationPanel);
                allIntervalsOfScaleConfiguration.startDialog();
            }
        });
    }

    private void buildCheckerPanel(JComponent checkerPanel) {
        final PurityCheckConfigurationPanel purityCheckConfiguration = new PurityCheckConfigurationPanel();
        
        checkerPanel.add(purityCheckConfiguration.panel, BorderLayout.CENTER);
        
        final JButton showDiagnosisButton = new JButton("Check Purity");
        showDiagnosisButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                final String[] titleAndResult = purityCheckConfiguration.getTitleAndResult();
                if (titleAndResult == null) {
                    JOptionPane.showMessageDialog(checkerPanel, "Please choose at least one just-intonation tuning!");
                    return;
                }
                TextAreaUtil.showResultInTextDialog(checkerPanel, titleAndResult[0], titleAndResult[1], null);
            }
        });
        
        final JButton help = new JButton("Help");
        help.addActionListener(event -> HelpWindowSingleton.start(
                purityCheckConfiguration.panel, 
                HelpForJustIntonationChecker.TITLE, 
                HelpForJustIntonationChecker.URL));
        
        final JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
        buttonPanel.add(showDiagnosisButton);
        buttonPanel.add(ToolBarUtil.getHelpButtonLookWrapper(help));
        
        checkerPanel.add(buttonPanel, BorderLayout.SOUTH);
    }
    
    
    // "Tunings" panel text builder
    
    private String buildTuningDisplayText(
            ToneSystemConfigurationPanel toneSystemConfiguration, 
            ColumnDisplayConfigurationPanel displayConfiguration)
    {
        final ToneSystem toneSystem = toneSystemConfiguration.getToneSystem();
        final StringBuilder sb = new StringBuilder();
        
        if (displayConfiguration.title.isSelected())
            StringUtil.appendLine(toneSystem.toString(), sb);
        
        for (Tone tone : toneSystem.tones())
            displayConfiguration.append(tone, sb);
        
        return StringUtil.removeTrailingLineBlanks(sb);
    }

    
    // "Difference Tones" panel text builder
    
    private String buildDifferenceTonesDisplayText(DifferenceTonesConfigurationPanel configurationPanel) {
        final String lowerToneIpnName = configurationPanel.getLowerToneIpnName();
        final String upperToneIpnName = configurationPanel.getUpperToneIpnName();
        if (lowerToneIpnName.trim().length() <= 0 || upperToneIpnName.trim().length() <= 0)
            throw new IllegalArgumentException("Please enter two valid IPN tone names!");
        
        final boolean onlyPrimaryDifferenceTones = configurationPanel.showOnlyPrimaryDifferenceTone();
        final DifferenceTones differenceTones = new DifferenceTones(
                configurationPanel.getToneSystem().tones(), 
                configurationPanel.getDeviation(), 
                onlyPrimaryDifferenceTones);
        
        final Tone lowerTone = differenceTones.forIpnName(lowerToneIpnName);
        final Tone upperTone = differenceTones.forIpnName(upperToneIpnName);
        if (lowerTone == null)
            throw new IllegalArgumentException("An invalid IPN tone name was entered: "+lowerToneIpnName);
        if (upperTone == null)
            throw new IllegalArgumentException("An invalid IPN tone name was entered: "+upperToneIpnName);
        
        final StringBuilder sb = new StringBuilder();
        StringUtil.appendLine(configurationPanel.getDifferenceTonesHeadingLines(lowerTone, upperTone), sb);
        
        final Tone[] tartiniTones = differenceTones.findDifferenceTones(lowerTone, upperTone);
        for (int i = 0; i < tartiniTones.length; i++) {
            final Tone tone = tartiniTones[i];
            final String prefix = (onlyPrimaryDifferenceTones ? "" : (i + 1)+": ");
            sb.append(prefix);
            
            final String name = (tone != null) ? tone.ipnName : "(None)";
            final String frequency = (tone != null) ? tone.formattedFrequency() : "";
            StringUtil.appendColumn(name, 3, sb);
            StringUtil.appendColumn(frequency, 9, sb);
            if (tone instanceof JustTone) { // null would give false
                final JustTone justTone = (JustTone) tone;
                StringUtil.appendColumn(justTone.centDeviationString(), 5, sb);
                StringUtil.appendColumn(justTone.interval.ratioString(0), 7, sb);
            }
            sb.append(StringUtil.NEWLINE);
        }
        return StringUtil.removeTrailingLineBlanks(sb);
    }
    
    private String buildIntervalsDisplayText(DifferenceTonesConfigurationPanel configurationPanel) {
        final String differenceToneIpnName = configurationPanel.getDifferenceToneIpnName();
        if (differenceToneIpnName.length() <= 0)
            throw new IllegalArgumentException("Please enter a valid IPN tone name!");
        
        final DifferenceToneInversions differenceToneInversions = new DifferenceToneInversions(
                new DifferenceToneInversions.Configuration(
                        configurationPanel.getToneSystem().tones(),
                    ToneSystem.semitoneSteps(configurationPanel.getNarrowestInterval()),
                    ToneSystem.semitoneSteps(configurationPanel.getWidestInterval()),
                    configurationPanel.getDeviation())
            );
        differenceToneInversions.removeDissonant(false);

        final Tone differenceTone = differenceToneInversions.forIpnName(differenceToneIpnName);
        if (differenceTone == null)
            throw new IllegalArgumentException("An invalid IPN tone name was entered: "+differenceToneIpnName);
        
        final List<TonePair> intervals = differenceToneInversions.getIntervalsGenerating(differenceTone);
        
        final String headingLines = configurationPanel.getIntervalHeadingLines(differenceTone);
        final StringBuilder sb = new StringBuilder(headingLines + StringUtil.NEWLINE);
        if (intervals != null) {
            for (TonePair interval : intervals) {
                StringUtil.appendColumn(interval.intervalName(), 13, sb); // maximum "MAJOR_SEVENTH"
                StringUtil.appendColumn(interval.lowerTone().ipnName, 3, sb); // maximum "C10"
                StringUtil.appendColumn(interval.upperTone().ipnName, 3, sb);
                StringUtil.appendColumn(interval.lowerTone().formattedFrequency(), 9, sb);
                StringUtil.appendColumn(interval.upperTone().formattedFrequency(), 9, sb);
                sb.append(StringUtil.NEWLINE);
            }
        }
        else {
            StringUtil.appendColumn("(None)", 13, sb); // empty indicator
        }
        
        return sb.toString();
    }
}