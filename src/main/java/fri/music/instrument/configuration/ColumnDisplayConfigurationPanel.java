package fri.music.instrument.configuration;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;
import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JPanel;
import fri.music.Tone;
import fri.music.AbstractJustIntonation.JustTone;
import fri.music.utils.StringUtil;
import fri.music.utils.swing.layout.SmartComboBox;

/**
 * Belongs to TuningsAndPurityCheckLauncher.
 * Helper class that contains the UI column configuration
 * and can format text according to it.
 */
class ColumnDisplayConfigurationPanel
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
    private static class Column extends JPanel implements Comparable<ColumnDisplayConfigurationPanel.Column>
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
        public int compareTo(ColumnDisplayConfigurationPanel.Column other) {
            return positionChoice().getPosition() - other.positionChoice().getPosition();
        }
        
        @Override
        public String toString() {
            return getClass().getSimpleName()+" "+positionChoice().getPosition()+" '"+checkBox().getText()+"'";
        }
        
        boolean isSelected() {
            return checkBox().isSelected();
        }
        ColumnDisplayConfigurationPanel.PositionChoice positionChoice() {
            return (ColumnDisplayConfigurationPanel.PositionChoice) getComponent(0);
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
        private List<ColumnDisplayConfigurationPanel.Column> oldColumns;
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
                final ColumnDisplayConfigurationPanel.PositionChoice positionChoice = (ColumnDisplayConfigurationPanel.PositionChoice) event.getSource();
                move(oldValue, itemValue(event), positionChoice);
            }
            finally {
                oldValue = null;
                oldColumns = null;
                working = false;
            }
        }

        private void move(final Integer oldValue, final Integer newValue, ColumnDisplayConfigurationPanel.PositionChoice positionChoice) {
            final boolean movingLeft = (oldValue > newValue);
            // change all position choice values except event source
            for (ColumnDisplayConfigurationPanel.Column column : oldColumns) { // use old sort order!
                final ColumnDisplayConfigurationPanel.PositionChoice columnPositionChoice = column.positionChoice();
                
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
    
    final JPanel panel;
    
    final JCheckBox title = new JCheckBox("Title", false);
    
    private List<ColumnDisplayConfigurationPanel.Column> columns = List.of(
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
    
    ColumnDisplayConfigurationPanel() {
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
        
        for (ColumnDisplayConfigurationPanel.Column column : columns)
            column.positionChoice().addItemListener(positionChangeListener);
    }

    /** Appends given tone to given string-builder according to column configuration, including trailing newline. */
    void append(Tone tone, StringBuilder sb) {
        final boolean isJustTone = (tone instanceof JustTone);
        
        for (ColumnDisplayConfigurationPanel.Column column : sortedColumnsClone())
            if (column.isSelected() && (isJustTone || column.forJustTone == isJustTone))
                StringUtil.appendColumn(column.content.apply(tone), column.maxLength, sb);
        
        sb.append(StringUtil.NEWLINE);
    }

    private void sortCheckboxesVisually() {
        final List<ColumnDisplayConfigurationPanel.Column> columns = sortedColumnsClone();
        
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
    
    private List<ColumnDisplayConfigurationPanel.Column> sortedColumnsClone() {
        final List<ColumnDisplayConfigurationPanel.Column> sortedColumns = new ArrayList<>(columns);
        Collections.sort(sortedColumns);
        return sortedColumns;
    }
}   // end class ColumnDisplayConfiguration