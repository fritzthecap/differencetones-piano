package fri.music.justintonation;

import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;
import java.util.Set;
import java.util.stream.Stream;
import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ListSelectionModel;
import fri.music.JustIntonation;
import fri.music.ScaleTypes;

/**
 * Lets configure the <code>JustIntonationChecker</code> class
 * to see data about just-intonation scales and their harmony issues.
 */
public class ConfigurationPanel
{
    public final JPanel panel;
    
    private JCheckBox showNotScalesOnly;
    private JCheckBox showUnjustOnly;
    private JCheckBox checkAgainst5LimitIntervals;
    private JCheckBox alsoCheckMajorSecondAndMinorSeventh;
    private JList<String> checkedScaleNames;
    private JCheckBox checkWhiteKeyScalesOnly;
    private JCheckBox showTriadsOnly;
    private JCheckBox considerDifferenceTones;
    private JCheckBox checkChromaticScaleDifferenceTones;
    
    private JList<String> checkedJustIntonationTunings;

    public ConfigurationPanel() {
        buildCheckerConfigurationFields();
        buildJustIntonationSelection();
        
        this.panel = new JPanel();
        panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
        
        final JPanel configOptionsUpper = new JPanel();
        configOptionsUpper.setLayout(new BoxLayout(configOptionsUpper, BoxLayout.Y_AXIS));
        configOptionsUpper.add(showTriadsOnly);
        configOptionsUpper.add(showNotScalesOnly);
        configOptionsUpper.add(showUnjustOnly);
        configOptionsUpper.add(alsoCheckMajorSecondAndMinorSeventh);
        //configOptionsUpper.add(checkWhiteKeyScalesOnly); // does this option make sense?
        configOptionsUpper.add(checkAgainst5LimitIntervals); // does this option make sense?
        final JPanel moveLeftUpperPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        moveLeftUpperPanel.setBorder(BorderFactory.createTitledBorder("Report Scope"));
        moveLeftUpperPanel.add(configOptionsUpper);
        
        final JPanel configOptionsLower = new JPanel();
        configOptionsLower.setLayout(new BoxLayout(configOptionsLower, BoxLayout.Y_AXIS));
        configOptionsLower.add(considerDifferenceTones);
        configOptionsLower.add(checkChromaticScaleDifferenceTones);
        final JPanel moveLeftLowerPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        moveLeftLowerPanel.setBorder(BorderFactory.createTitledBorder("Difference Tone Integration"));
        moveLeftLowerPanel.add(configOptionsLower);
        
        final JComponent checkedScaleNamesScrollPane = new JScrollPane(checkedScaleNames);
        checkedScaleNames.setVisibleRowCount(7);
        checkedScaleNamesScrollPane.setBorder(BorderFactory.createTitledBorder("Scale(s)"));
        moveLeftUpperPanel.add(checkedScaleNamesScrollPane);
        
        final JComponent checkedJustIntonationTuningsScrollPane = new JScrollPane(checkedJustIntonationTunings);
        checkedJustIntonationTunings.setVisibleRowCount(5);
        checkedJustIntonationTuningsScrollPane.setBorder(BorderFactory.createTitledBorder("Just-Intonation Tuning(s) to check"));
        
        panel.add(moveLeftUpperPanel);
        panel.add(moveLeftLowerPanel);
        //panel.add(checkedScaleNamesScrollPane);
        panel.add(checkedJustIntonationTuningsScrollPane);
    }

    
    /**
     * @return null when no just-intonation tuning was selected,
     *      else the title and result of the configured diagnosis.
     */
    public String[] getTitleAndResult() {
        final List<String> selectedValues = checkedJustIntonationTunings.getSelectedValuesList();
        if (selectedValues.size() <= 0)
            return null;
        
        final StringBuilder titleText = new StringBuilder();
        final StringBuilder resultText = new StringBuilder();
        final JustIntonationChecker.Configuration configuration = getCheckerConfiguration();
        
        int i = 0;
        for (String justIntonationTuningName : selectedValues) {
            titleText.append(justIntonationTuningName);
            i++;
            if (i < selectedValues.size())
                titleText.append(", ");
            
            final JustIntonation.ChromaticScales chromaticScale = JustIntonation.ChromaticScales.valueOf(justIntonationTuningName);
            final JustIntonationChecker.Result result = new JustIntonationChecker(configuration).check(chromaticScale);
            resultText.append(result.toString());
            if (i < selectedValues.size())
                resultText.append(JustIntonationChecker.newline);
        }
        
        return new String[] { titleText.toString(), resultText.toString() };
    }
    
    private JustIntonationChecker.Configuration getCheckerConfiguration() {
        return new JustIntonationChecker.Configuration(
                showNotScalesOnly.isSelected() == false,
                showUnjustOnly.isSelected(),
                checkAgainst5LimitIntervals.isSelected(),
                alsoCheckMajorSecondAndMinorSeventh.isSelected(),
                checkedScaleNames.getSelectedValuesList(),
                checkWhiteKeyScalesOnly.isSelected(),
                showTriadsOnly.isSelected(),
                considerDifferenceTones.isSelected(),
                checkChromaticScaleDifferenceTones.isSelected()
            );
    }


    private void buildCheckerConfigurationFields() {
        showNotScalesOnly = new JCheckBox("Show details of diagnoses", true);
        showNotScalesOnly.setToolTipText("Beside scale diagnosis, also show 'justness' details of its intervals and triads");
        
        showUnjustOnly = new JCheckBox("Show 'unjust' details only", true);
        showUnjustOnly.setToolTipText("Leave out 'just' intervals and triads");
        
        showNotScalesOnly.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                showUnjustOnly.setEnabled(showNotScalesOnly.isSelected()); 
            }
        });
        
        checkAgainst5LimitIntervals = new JCheckBox("Always compare with 5-limit interval ratios", false);
        checkAgainst5LimitIntervals.setToolTipText("When false, compare any interval with the scale's first-found interval of that kind (check the scale's self-coherence)");
        
        alsoCheckMajorSecondAndMinorSeventh = new JCheckBox("Also check major-second and minor-seventh", false);
        alsoCheckMajorSecondAndMinorSeventh.setToolTipText("Also check the mostly 'unjust' major second an minor seventh");
        
        final Set<String> scaleTypeNameSet = ScaleTypes.scaleToStartNote.keySet();
        final String[] scaleTypeNames = scaleTypeNameSet.toArray(new String[scaleTypeNameSet.size()]);
        checkedScaleNames = new JList<>(scaleTypeNames);
        checkedScaleNames.setToolTipText("Select 1-n diatonic scale types you want to examine. Use Ctrl- and Shift-Click to select several!");
        checkedScaleNames.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
        checkedScaleNames.setSelectedIndices(new int[] { 0, /*5*/ }); // IONIAN, AEOLIAN
        
        checkWhiteKeyScalesOnly = new JCheckBox("Check scales built on white piano-keys only", true);
        checkWhiteKeyScalesOnly.setToolTipText("Do not check scales that use black piano-keys (e.g. AEOLIAN scale based on C)");
        
        showTriadsOnly = new JCheckBox("Show triad diagnoses only, no intervals", false);
        showTriadsOnly.setToolTipText("Do not display interval diagnoses, just triad results");
        
        considerDifferenceTones = new JCheckBox("Consider interval as 'unjust' when its difference tone is not in scale", true);
        considerDifferenceTones.setToolTipText("Check whether difference tones, resulting from a diatonic scale's intervals, are scale tones");
        
        checkChromaticScaleDifferenceTones = new JCheckBox("Check difference tones of all intervals being in scale", false);
        checkChromaticScaleDifferenceTones.setToolTipText("Check all intervals for its difference-tone being in scale, widening the interval chromatically");
    }
    
    private void buildJustIntonationSelection() {
        final JustIntonation.ChromaticScales[] justIntonations = JustIntonation.ChromaticScales.values();
        final String[] justIntonationNames = Stream.of(justIntonations).map(ji -> ji.name()).toArray(String[]::new);
        checkedJustIntonationTunings = new JList<>(justIntonationNames);
        checkedJustIntonationTunings.setToolTipText("Select the just-intonation tuning(s) you want to examine. Use Ctrl- and Shift-Click to select!");
        checkedJustIntonationTunings.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
        checkedJustIntonationTunings.setSelectedIndices(new int[] { 0 }); // LIMIT_5_SYMMETRIC_1
    }
}