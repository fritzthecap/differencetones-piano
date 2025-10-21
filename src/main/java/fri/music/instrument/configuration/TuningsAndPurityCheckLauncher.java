package fri.music.instrument.configuration;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import fri.music.AbstractJustIntonation.JustTone;
import fri.music.Tone;
import fri.music.ToneSystem;
import fri.music.differencetones.DifferenceToneInversions;
import fri.music.differencetones.DifferenceToneInversions.TonePair;
import fri.music.differencetones.DifferenceTones;
import fri.music.instrument.TuningComponent;
import fri.music.instrument.wave.DeviationComponent;
import fri.music.instrument.wave.IntervalRangeComponent;
import fri.music.justintonation.swing.HelpForJustIntonationChecker;
import fri.music.justintonation.swing.PurityCheckConfigurationPanel;
import fri.music.utils.StringUtil;
import fri.music.utils.swing.BorderUtil;
import fri.music.utils.swing.layout.SmartComboBox;
import fri.music.utils.swing.layout.ToolBarUtil;
import fri.music.utils.swing.text.HelpWindowSingleton;
import fri.music.utils.swing.text.TextAreaActions;
import fri.music.utils.swing.text.TextFieldUtil;
import fri.music.utils.swing.window.DialogStarter;

/**
 * UI for checking the purity of fraction-based tunings
 * with ConfigurationPanel.
 * <p/>
 * TODO: this class goes beyond justintonation and 
 *  thus is in the wrong package. Mind that you
 *  have to move also HelpForTextDisplay.java and
 *  HelpForTextDisplay.html to its new package, and then
 *  adjust all "justintonation/swing/HelpForTextDisplay.html" 
 *  relative paths in all HTML documentations!
 */
public class TuningsAndPurityCheckLauncher
{
    public JComponent panel;
    
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
        
        final ColumnDisplayConfiguration displayConfiguration = new ColumnDisplayConfiguration();
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
                    showResultInTextDialog(tuningsPanel, title, result, Font.MONOSPACED);
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
        final DifferenceTonesConfiguration configurationPanel = new DifferenceTonesConfiguration();
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
                    showResultInTextDialog(differenceTonesPanel, title, result, Font.MONOSPACED);
                }
                catch (Exception e) {
                    JOptionPane.showMessageDialog(differenceTonesPanel, e.getMessage());
                }
            }
        };
        configurationPanel.differenceTonesButton.addActionListener(actionListener);
        configurationPanel.intervalsButton.addActionListener(actionListener);
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
                showResultInTextDialog(checkerPanel, titleAndResult[0], titleAndResult[1], null);
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
    
    private void showResultInTextDialog(JComponent parent, String title, String text, String font) {
        final JTextArea textArea = new JTextArea(text);
        textArea.setTabSize(2);
        textArea.setEditable(false);
        if (font != null)
            textArea.setFont(Font.decode(font).deriveFont(Font.BOLD, 14f));
        else
            textArea.setFont(textArea.getFont().deriveFont(Font.BOLD, 14f));
        
        final TextAreaActions fontActions = new TextAreaActions(textArea); // adds context menu
        
        DialogStarter.start(title, parent, new JScrollPane(textArea), null, true);
    }

    
    private String buildTuningDisplayText(
            ToneSystemConfigurationPanel toneSystemConfiguration, 
            ColumnDisplayConfiguration displayConfiguration)
    {
        final ToneSystem toneSystem = toneSystemConfiguration.getToneSystem();
        final StringBuilder sb = new StringBuilder();
        
        if (displayConfiguration.title.isSelected())
            appendLine(toneSystem.toString(), sb);
        
        for (Tone tone : toneSystem.tones())
            displayConfiguration.append(tone, sb);
        
        return StringUtil.removeTrailingLineBlanks(sb);
    }

    
    private String buildDifferenceTonesDisplayText(DifferenceTonesConfiguration configurationPanel) {
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
        appendLine(configurationPanel.getDifferenceTonesHeadingLines(lowerTone, upperTone), sb);
        
        final Tone[] tartiniTones = differenceTones.findDifferenceTones(lowerTone, upperTone);
        for (int i = 0; i < tartiniTones.length; i++) {
            final Tone tone = tartiniTones[i];
            final String prefix = (onlyPrimaryDifferenceTones ? "" : (i + 1)+": ");
            sb.append(prefix);
            
            final String name = (tone != null) ? tone.ipnName : "(None)";
            final String frequency = (tone != null) ? tone.formattedFrequency() : "";
            appendColumn(name, 3, sb);
            appendColumn(frequency, 9, sb);
            if (tone instanceof JustTone) { // null would give false
                final JustTone justTone = (JustTone) tone;
                appendColumn(justTone.centDeviationString(), 5, sb);
                appendColumn(justTone.interval.ratioString(0), 7, sb);
            }
            sb.append(StringUtil.NEWLINE);
        }
        return StringUtil.removeTrailingLineBlanks(sb);
    }

    
    private String buildIntervalsDisplayText(DifferenceTonesConfiguration configurationPanel) {
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
                appendColumn(interval.intervalName(), 13, sb); // maximum "MAJOR_SEVENTH"
                appendColumn(interval.lowerTone().ipnName, 3, sb); // maximum "C10"
                appendColumn(interval.upperTone().ipnName, 3, sb);
                appendColumn(interval.lowerTone().formattedFrequency(), 9, sb);
                appendColumn(interval.upperTone().formattedFrequency(), 9, sb);
                sb.append(StringUtil.NEWLINE);
            }
        }
        else {
            appendColumn("(None)", 13, sb); // empty indicator
        }
        
        return sb.toString();
    }

    
    private void appendLine(String line, StringBuilder sb) {
        sb.append(line);
        sb.append(StringUtil.NEWLINE);
    }
    
    
    private static final int columnSeparation = 2; // blanks
    
    /** Right-aligned columns with separator-blanks. */
    private static void appendColumn(String content, int maxLength, StringBuilder sb) {
        appendColumn(content, maxLength, columnSeparation, sb);
    }
    private static void appendColumn(String content, int maxLength, int blanksToAppend, StringBuilder sb) {
        final int contentLength = content.length();
        final int lessThanMaxLength = maxLength - contentLength;
        for (int i = 0; i < lessThanMaxLength; i++)
            sb.append(' '); // align any content to right
        
        sb.append(content);
        
        if (blanksToAppend > 0) { // not end of columns
            if (lessThanMaxLength < 0) // subtract from blanks what is too much in content
                blanksToAppend += lessThanMaxLength;

            for (int i = 0; i < blanksToAppend; i++)
                sb.append(' ');
        }
    }
    

    /**
     * Helper class that contains the UI column configuration
     * and can format text according to it.
     */
    private static class ColumnDisplayConfiguration
    {
        /** The column-position choice. */
        private static class PositionChoice extends SmartComboBox
        {
            public PositionChoice(int position) {
                setToolTipText("Column Position");
                
                for (int i = 1; i <= NUMBER_OF_COLUMNS; i++)
                    addItem(""+i);
                setSelectedItem(""+position);
            }
            
            public Integer getPosition() {
                final String item = (String) getSelectedItem();
                return Integer.valueOf(item);
            }
        }   // end class PositionChoice
        
        
        /** The column-activation checkbox and its position chooser. */
        private static class Column extends JPanel implements Comparable<Column>
        {
            public final Function<Tone,String> content;
            public final int maxLength;
            public final boolean forJustTone;
            
            public Column(int position, boolean forJustTone, JCheckBox checkBox, Function<Tone,String> content, int maxLength) {
                super(new BorderLayout());
                
                checkBox.setToolTipText("Click to Add or Remove Column from Display");
                checkBox.addActionListener(event -> checkboxChanged());
                checkBox.setBorder(BorderFactory.createEmptyBorder(0, 10, 0, 20));
                
                this.forJustTone = forJustTone;
                this.content = content;
                this.maxLength = maxLength;
                
                add(new PositionChoice(position), BorderLayout.WEST);
                add(checkBox, BorderLayout.CENTER);
                
                checkboxChanged();
            }
            
            @Override
            public int compareTo(Column other) {
                return positionChoice().getPosition() - other.positionChoice().getPosition();
            }
            
            @Override
            public String toString() {
                return getClass().getSimpleName()+" "+positionChoice().getPosition()+" '"+checkBox().getText()+"'";
            }
            
            boolean isSelected() {
                return checkBox().isSelected();
            }
            PositionChoice positionChoice() {
                return (PositionChoice) getComponent(0);
            }
            
            private JCheckBox checkBox() {
                return (JCheckBox) getComponent(1);
            }
            private void checkboxChanged() {
                positionChoice().setEnabled(isSelected());                
            }
        }   // end class Column
        
        
        /** Reorders columns when a position-choice gets changed by the user. */
        private class PositionChangeItemListener implements ItemListener
        {
            private Integer oldValue;
            private List<Column> oldColumns;
            private boolean working;
            
            @Override
            public void itemStateChanged(ItemEvent event) {
                if (working) // ignore events coming from own setSelectedItem() call
                    return;
                
                // order of arriving events is not specified!
                if (event.getStateChange() == ItemEvent.DESELECTED)
                    moveOrRememberOldValue(event);
                else if (event.getStateChange() == ItemEvent.SELECTED)
                    moveOrRememberOldValue(event);
            }

            private void moveOrRememberOldValue(ItemEvent event) {
                if (oldValue == null)
                    rememberState(event);
                else
                    move(event);
            }

            private void rememberState(ItemEvent event) {
                oldValue = itemValue(event);
                oldColumns = sortedColumnsClone();
            }

            private Integer itemValue(ItemEvent event) {
                return Integer.valueOf((String) event.getItem());
            }

            private void move(ItemEvent event) {
                working = true;
                try {
                    final PositionChoice positionChoice = (PositionChoice) event.getSource();
                    move(oldValue, itemValue(event), positionChoice);
                }
                finally {
                    oldValue = null;
                    oldColumns = null;
                    working = false;
                }
            }

            private void move(final Integer oldValue, final Integer newValue, PositionChoice positionChoice) {
                final boolean movingLeft = (oldValue > newValue);
                // change all position choice values except event source
                for (Column column : oldColumns) { // use old sort order!
                    final PositionChoice columnPositionChoice = column.positionChoice();
                    
                    if (columnPositionChoice != positionChoice) { // do not touch the event source
                        final Integer position = columnPositionChoice.getPosition();
                        
                        if (movingLeft == true && position < oldValue && position >= newValue)
                            column.positionChoice().setSelectedItem(""+(position + 1));
                        else if (movingLeft == false && position > oldValue && position <= newValue)
                            column.positionChoice().setSelectedItem(""+(position - 1));
                    }
                }
                // finished re-indexing, reorder and repaint
                sortCheckboxesVisually();
            }
        };  // end class PositionChangeItemListener
        
        
        // start class ColumnDisplayConfiguration
        
        private static final int NUMBER_OF_COLUMNS = 6;
        
        public final JPanel panel;
        
        private final JCheckBox title = new JCheckBox("Title", false);
        private List<Column> columns = List.of(
            new Column(
                    1, false, new JCheckBox("IPN Note Name", true),       t -> t.ipnName, 3), // max "C10"
            new Column(
                    2, false, new JCheckBox("MIDI-Number", false),   t -> ""+t.midiNumber, 3), // max "132"
            new Column(
                    3, false, new JCheckBox("Frequency in Hertz", true), t -> t.formattedFrequency(), 9), // max "16803.840"
            new Column(
                    4, false, new JCheckBox("Cents", false),         t -> ""+t.cent+" ¢", 7), // max "12000 c"
            new Column(
                    5, true, new JCheckBox("Cent Deviation from 12-ET", true),  t -> ((JustTone) t).centDeviationString(), 5), // max "+12 ¢""
            new Column(
                    6, true, new JCheckBox("Fraction", true),        t -> ((JustTone) t).interval.ratioString(0), 7) // max "729/512"
        );
        private final ItemListener positionChangeListener = new PositionChangeItemListener();
        private JComponent checkboxTable;
        
        ColumnDisplayConfiguration() {
            if (columns.size() != NUMBER_OF_COLUMNS)
                throw new RuntimeException("Please adapt columns list to have size "+NUMBER_OF_COLUMNS);
            
            this.panel = new JPanel();
            panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
            panel.setBorder(BorderFactory.createTitledBorder("Displayed Columns and Order"));
            
            final JPanel titleLayoutPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
            titleLayoutPanel.add(title);
            panel.add(titleLayoutPanel);
            title.setToolTipText("General Tuning Information as Header");
            
            sortCheckboxesVisually();
            
            for (Column column : columns)
                column.positionChoice().addItemListener(positionChangeListener);
        }

        public void append(Tone tone, StringBuilder sb) {
            final boolean isJustTone = (tone instanceof JustTone);
            
            for (Column column : sortedColumnsClone())
                if (column.isSelected() && (isJustTone || column.forJustTone == isJustTone))
                    appendColumn(column.content.apply(tone), column.maxLength, sb);
            
            sb.append(StringUtil.NEWLINE);
        }

        void sortCheckboxesVisually() {
            final List<Column> columns = sortedColumnsClone();
            
            final JPanel leftPanel = new JPanel();
            leftPanel.setLayout(new BoxLayout(leftPanel, BoxLayout.Y_AXIS));
            for (int i = 0; i < columns.size() / 2; i++)
                leftPanel.add(columns.get(i));
            
            final JPanel rightPanel = new JPanel();
            rightPanel.setLayout(new BoxLayout(rightPanel, BoxLayout.Y_AXIS));
            for (int i = columns.size() / 2; i < columns.size(); i++)
                rightPanel.add(columns.get(i));
            
            final JPanel table = new JPanel();
            table.setLayout(new BoxLayout(table, BoxLayout.X_AXIS));
            table.add(leftPanel);
            table.add(rightPanel);
            
            addOrReplaceCheckboxTable(table);
        }
        
        private void addOrReplaceCheckboxTable(JPanel table) {
            final boolean revalidation = (this.checkboxTable != null);
            final int addIndex;
            if (revalidation) {
                addIndex = Arrays.asList(panel.getComponents()).indexOf(this.checkboxTable);
                panel.remove(this.checkboxTable);
            }
            else {
                addIndex = -1; // initially append to end
            }

            panel.add(this.checkboxTable = table, addIndex);
            
            if (revalidation) {
                panel.revalidate();
                panel.repaint();
            }
        }
        
        private List<Column> sortedColumnsClone() {
            final List<Column> sortedColumns = new ArrayList<>(columns);
            Collections.sort(sortedColumns);
            return sortedColumns;
        }
    }   // end class ColumnDisplayConfiguration
    
    
    
    private static class DifferenceTonesConfiguration
    {
        private static KeyListener toneNameListener;
        
        public final JPanel panel;
        public final JButton differenceTonesButton;
        public final JButton intervalsButton;
        
        private final TuningComponent tuning;
        private final FrequencyOfA4Component frequencyOfA4;
        private final DeviationComponent deviation;
        
        private final JTextField lowerIntervalTone;
        private final JTextField upperIntervalTone;
        private final JCheckBox onlyPrimaryDifferenceTone;
        
        private final JTextField differenceTone;
        private final IntervalRangeComponent intervalRange;
     
        public DifferenceTonesConfiguration() {
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
            final JPanel bottomCenterPanel1 = new JPanel();
            bottomCenterPanel1.setLayout(new BoxLayout(bottomCenterPanel1, BoxLayout.Y_AXIS));
            bottomCenterPanel1.add(onlyPrimaryDifferenceTone);
            
            this.differenceTonesButton = new JButton("Display Difference-Tone(s)");
            differenceTonesButton.setAlignmentX(JComponent.CENTER_ALIGNMENT);
            bottomCenterPanel1.add(differenceTonesButton);
            
            differenceTonesPanel.add(bottomCenterPanel1);
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
            intervalsButton.setAlignmentX(JComponent.CENTER_ALIGNMENT);
            
            intervalsPanel.add(choicePanel);
            intervalsPanel.add(intervalsButton);
            intervalsPanel.add(Box.createVerticalGlue());
            
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
        
        // privates
        
        private String getGlobalHeadingLines() {
            return 
                "Tuning:     "+tuning.getChoice(null).getSelectedItem()+StringUtil.NEWLINE+
                "A4:         "+frequencyOfA4.getValue()+" Hertz"+StringUtil.NEWLINE+
                "Deviation:  "+deviation.deviationSlider.getValue()+" %"+StringUtil.NEWLINE;
        }
        
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
}