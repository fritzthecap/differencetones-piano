package fri.music.justintonation.swing;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;
import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTextArea;
import fri.music.AbstractJustIntonation.JustTone;
import fri.music.Tone;
import fri.music.ToneSystem;
import fri.music.instrument.configuration.ToneSystemConfigurationPanel;
import fri.music.swingutils.layout.SmartComboBox;
import fri.music.swingutils.layout.ToolBarUtil;
import fri.music.swingutils.text.HelpWindowSingleton;
import fri.music.swingutils.text.TextAreaActions;
import fri.music.swingutils.window.DialogStarter;
import fri.music.utils.StringUtil;

/**
 * UI for checking the purity of fraction-based tunings
 * with ConfigurationPanel.
 */
public class TuningsAndPurityCheckLauncher
{
    public JComponent panel;
    
    public TuningsAndPurityCheckLauncher() {
        final JPanel tuningsPanel = new JPanel(new BorderLayout());
        buildTuningsPanel(tuningsPanel);
        tuningsPanel.setBorder(BorderFactory.createTitledBorder(
                BorderFactory.createLineBorder(Color.LIGHT_GRAY, 4, true),
                "Tunings Display"));
        
        final JPanel checkerPanel = new JPanel(new BorderLayout());
        buildCheckerPanel(checkerPanel);
        checkerPanel.setBorder(BorderFactory.createTitledBorder(
                BorderFactory.createLineBorder(Color.LIGHT_GRAY, 4, true),
                "Purity Check"));
        
        final JSplitPane splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
        splitPane.setLeftComponent(tuningsPanel);
        splitPane.setRightComponent(checkerPanel);
        splitPane.setResizeWeight(0.5);
        
        this.panel = splitPane;
    }

    private void buildTuningsPanel(JPanel tuningsPanel) {
        final ToneSystemConfigurationPanel tuningsConfigurationPanel = new ToneSystemConfigurationPanel();
        tuningsPanel.add(tuningsConfigurationPanel.panel, BorderLayout.NORTH);
        
        final ColumnDisplayConfiguration displayConfiguration = new ColumnDisplayConfiguration();
        tuningsPanel.add(displayConfiguration.panel, BorderLayout.CENTER);
        
        final JButton displayTuningButton = new JButton("Display Tuning");
        displayTuningButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent event) {
                try {
                    final String title = tuningsConfigurationPanel.getToneSystem().name();
                    final String result = buildTuningDisplayText(tuningsConfigurationPanel, displayConfiguration);
                    showResultInTextDialog(tuningsPanel, title, result, Font.MONOSPACED);
                }
                catch (IllegalArgumentException e) {
                    JOptionPane.showMessageDialog(tuningsPanel, e.getMessage());
                }
            }
        });
        
        final JButton help = new JButton("Help");
        help.addActionListener(event -> HelpWindowSingleton.start(
                tuningsConfigurationPanel.panel, 
                HelpForTuningsParameter.TITLE, 
                HelpForTuningsParameter.URL));
        
        final JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
        buttonPanel.add(displayTuningButton);
        buttonPanel.add(ToolBarUtil.getHelpButtonLookWrapper(help));
        
        tuningsPanel.add(buttonPanel, BorderLayout.SOUTH);
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
        textArea.setEditable(false);
        
        if (font != null)
            textArea.setFont(Font.decode(font).deriveFont(Font.BOLD, 14f));
        else
            textArea.setFont(textArea.getFont().deriveFont(Font.BOLD, 14f));

        textArea.setTabSize(4);
        textArea.setRows(32);
        
        final TextAreaActions fontActions = new TextAreaActions(textArea);
        
        DialogStarter.start(
                title, 
                parent, 
                new JScrollPane(textArea), 
                null, 
                true);
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
        
        return sb.toString();
    }

    private void appendLine(String line, StringBuilder sb) {
        sb.append(line);
        sb.append(StringUtil.NEWLINE);
    }
    

    
    private static class ColumnDisplayConfiguration
    {
        private static class PositionChoice extends SmartComboBox
        {
            public PositionChoice(int position) {
                setToolTipText("Column Sort Order Number");
                
                for (int i = 1; i <= NUMBER_OF_COLUMNS; i++)
                    addItem(""+i);
                setSelectedItem(""+position);
            }
            
            public int getPosition() {
                final String item = (String) getSelectedItem();
                return Integer.valueOf(item);
            }
        }
        
        private static class Column extends JPanel implements Comparable<Column>
        {
            public final Function<Tone,String> content;
            public final int maxLength;
            public final boolean forJustTone;
            
            public Column(int position, boolean forJustTone, JCheckBox checkBox, Function<Tone,String> content, int maxLength) {
                super(new BorderLayout());
                
                checkBox.setToolTipText("Add or Remove Column from Display");
                checkBox.addActionListener(event -> checkboxChanged());
                
                this.forJustTone = forJustTone;
                this.content = content;
                this.maxLength = maxLength;
                
                add(new PositionChoice(position), BorderLayout.WEST);
                add(checkBox, BorderLayout.CENTER);
                
                checkboxChanged();
            }
            
            public boolean isSelected() {
                return checkBox().isSelected();
            }
            
            private JCheckBox checkBox() {
                return (JCheckBox) getComponent(1);
            }
            
            private PositionChoice positionChoice() {
                return (PositionChoice) getComponent(0);
            }
            
            private void checkboxChanged() {
                positionChoice().setEnabled(checkBox().isSelected());                
            }
            
            @Override
            public int compareTo(Column other) {
                return positionChoice().getPosition() - ((Column) other).positionChoice().getPosition();
            }
        }
        
        private static final int NUMBER_OF_COLUMNS = 6;
        
        private static final int columnSeparation = 4; // blanks
        
        private final JPanel panel;
        
        private final JCheckBox title = new JCheckBox("Title", false);
        
        private List<Column> columns = List.of(
            new Column(
                    1, false, new JCheckBox("IPN-Name", true),      t -> t.ipnName, 3), // max "C10"
            new Column(
                    2, false, new JCheckBox("MIDI-Number", false),  t -> ""+t.midiNumber, 3), // max "132"
            new Column(
                    3, false, new JCheckBox("Frequency", true),     t -> t.formattedFrequency(), 9), // max "16803.840"
            new Column(
                    4, false, new JCheckBox("Cents", false),        t -> ""+t.cent+" ¢", 7), // max "12000 c"
            new Column(
                    5, true, new JCheckBox("Fraction", true),       t -> ((JustTone) t).interval.ratioString(0), 7), // max "729/512"
            new Column(
                    6, true, new JCheckBox("Cent Deviation", true), t -> ((JustTone) t).centDeviationString(), 3) // max "+12"
        );
        
        /** Swaps position with peer column when a position changes. */
        private final ItemListener positionChangeListener = new ItemListener() {
            private int oldValue;
            private boolean working;
            
            @Override
            public void itemStateChanged(ItemEvent event) {
                if (working) // ignore events coming from own setSelectedItem() call
                    return;
                
                if (event.getStateChange() == ItemEvent.DESELECTED)
                    oldValue = Integer.valueOf((String) event.getItem()).intValue();
                else if (event.getStateChange() == ItemEvent.SELECTED)
                    try {
                        working = true;
                        final int newValue = Integer.valueOf((String) event.getItem()).intValue();
                        final PositionChoice positionChoice = (PositionChoice) event.getSource();
                        final Column columnWithSamePosition = columns.stream()
                            .filter(c -> c.positionChoice().getPosition() == newValue && c.positionChoice() != positionChoice)
                            .findFirst()
                            .get();
                        columnWithSamePosition.positionChoice().setSelectedItem(""+oldValue);
                    }
                    finally {
                        working = false;
                    }
            }
        };
        
        
        ColumnDisplayConfiguration() {
            if (columns.size() != NUMBER_OF_COLUMNS)
                throw new RuntimeException("Please adapt columns list to have size "+NUMBER_OF_COLUMNS);
            
            this.panel = new JPanel();
            panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
            panel.setBorder(BorderFactory.createTitledBorder("Displayed Tone Columns"));
            
            final JPanel checkBoxLayoutPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
            checkBoxLayoutPanel.add(title);
            panel.add(checkBoxLayoutPanel);
            title.setToolTipText("General Tuning Information as Header");
            
            final JPanel leftPanel = new JPanel();
            leftPanel.setLayout(new BoxLayout(leftPanel, BoxLayout.Y_AXIS));
            for (int i = 0; i < columns.size() / 2; i++) {
                final Column column = columns.get(i);
                leftPanel.add(column);
            }
            
            final JPanel rightPanel = new JPanel();
            rightPanel.setLayout(new BoxLayout(rightPanel, BoxLayout.Y_AXIS));
            for (int i = columns.size() / 2; i < columns.size(); i++) {
                final Column column = columns.get(i);
                rightPanel.add(column);
            }
            
            final JPanel table = new JPanel();
            table.setLayout(new BoxLayout(table, BoxLayout.X_AXIS));
            table.add(leftPanel);
            table.add(rightPanel);
            
            panel.add(table);
            
            for (Column column : columns)
                column.positionChoice().addItemListener(positionChangeListener);
        }
        
        void append(Tone tone, StringBuilder sb) {
            final boolean isJustTone = (tone instanceof JustTone);
            
            final List<Column> sortedColumns = new ArrayList<>(columns);
            Collections.sort(sortedColumns);
            
            for (Column column : sortedColumns)
                if (column.isSelected() && (isJustTone || column.forJustTone == isJustTone))
                    appendColumn(column.content.apply(tone), column.maxLength, columnSeparation, sb);
            
            sb.append(StringUtil.NEWLINE);
        }
        
        private void appendColumn(String content, int maxLength, int blanksToAppend, StringBuilder sb) {
            sb.append(content);
            
            if (blanksToAppend > 0) { // not end of columns
                final int contentLength = content.length();
                int lessThanMaxLength = maxLength - contentLength;
                if (lessThanMaxLength < 0) {
                    blanksToAppend += lessThanMaxLength;
                    lessThanMaxLength = 0;
                }
                for (int i = 0; i < lessThanMaxLength + blanksToAppend; i++)
                    sb.append(' ');
            }
        }
    }
}