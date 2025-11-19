package fri.music.instrument.configuration;

import java.awt.FlowLayout;
import java.awt.Font;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JPanel;
import fri.music.AbstractToneSystem;
import fri.music.Tone;
import fri.music.ToneSystem;
import fri.music.Tones;
import fri.music.differencetones.DifferenceToneInversions;
import fri.music.instrument.wave.DifferenceToneUtil;
import fri.music.utils.StringUtil;
import fri.music.utils.swing.text.TextAreaUtil;
import fri.music.utils.swing.window.DialogStarter;

/**
 * Lets configure octaves and lowest tone of a scale.
 */
class AllIntervalsOfScaleConfigurationPanel extends ToneRangeConfigurationPanel
{
    private final DifferenceTonesConfigurationPanel differenceTonesConfigurationPanel;
    private JDialog dialog;
    
    public AllIntervalsOfScaleConfigurationPanel(DifferenceTonesConfigurationPanel differenceTonesConfigurationPanel) {
        super(
                4, // number of octaves 
                StringUtil.getUntilFirstNumber(differenceTonesConfigurationPanel.getDifferenceToneIpnName()), 
                StringUtil.getFirstNumber(differenceTonesConfigurationPanel.getDifferenceToneIpnName()), 
                true,  // show modal scale choice
                false); // octaves not on top
        this.differenceTonesConfigurationPanel = differenceTonesConfigurationPanel;
    }

    @Override
    protected String lowestTonePanelTitle() {
        return "Lowest Tone";
    }
    
    @Override
    protected String lowestToneBaseNameTooltip() {
        return "Lowest Tone of the Tonesystem";
    }
    
    @Override
    protected String lowestToneOctaveTooltip() {
        return "Octave of the Lowest Tone";
    }
    
    public void startDialog() {
        if (dialog != null) {
            dialog.setVisible(true);
        }
        else {
            final JButton showButton = new JButton("Display Intervals for All");
            showButton.addActionListener(event -> showResultInTextDialog());
            
            final JPanel dialogPanel = new JPanel();
            dialogPanel.setLayout(new BoxLayout(dialogPanel, BoxLayout.Y_AXIS));
            dialogPanel.add(this.panel);
            final JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
            buttonPanel.add(showButton);
            dialogPanel.add(buttonPanel);
            
            dialog = DialogStarter.start(
                    "Tone Range of Melody and Intervals",
                    differenceTonesConfigurationPanel.panel,
                    dialogPanel,
                    true);
        }
    }

    private void showResultInTextDialog() {
        final String lowestToneIpnName = getLowestToneIpnName();
        final int octaves = getOctaves();
        final ToneSystem toneSystem = differenceTonesConfigurationPanel.getToneSystem();
        final DifferenceToneInversions differenceToneInversions = differenceToneInversions(toneSystem, lowestToneIpnName, octaves);
        
        // get possible difference-tones
        final List<Tone> differenceTones = differenceToneInversions.differenceTones();
        // inversions were constructed with one below lowest tone, to find enclosing tones
        // also for lowest tone, but we don't want to see the one below here:
        differenceTones.remove(0);
        
        // prepare table column structure as sorted list of interval-names
        final List<String> tableColumns = intervalColumns(differenceTones, differenceToneInversions);
        // fill table with values
        final List<List<String>> tableCells = intervalTableCells(tableColumns, differenceTones, differenceToneInversions);
        
        // write text
        final StringBuilder sb = new StringBuilder();
        // write header informations
        final String titleLine = 
                differenceTonesConfigurationPanel.getGlobalHeadingLines()+
                "Base Tone:  "+lowestToneIpnName+StringUtil.NEWLINE+
                "Octaves:    "+octaves+StringUtil.NEWLINE;
        StringUtil.appendLine(titleLine, sb);
        // write table cells
        writeText(differenceTones, tableColumns, tableCells, sb);
        
        TextAreaUtil.showResultInTextDialog(
                differenceTonesConfigurationPanel.panel,
                "Difference-Tone Intervals for "+toneSystem.name(),
                sb.toString(),
                Font.MONOSPACED);
    }

    private DifferenceToneInversions differenceToneInversions(ToneSystem toneSystem, String lowestToneIpnName, int octaves) {
        final double deviation = differenceTonesConfigurationPanel.getDeviation();
        final String narrowestInterval = differenceTonesConfigurationPanel.getNarrowestInterval();
        final String widestInterval = differenceTonesConfigurationPanel.getWidestInterval();
        
        final Tone[] toneStock = toneSystem.tones();
        final Tone lowestTone = new Tones(toneStock).forIpnName(lowestToneIpnName);
        final Tone oneBelow = DifferenceToneUtil.oneToneBelow(toneStock, lowestTone);
        final Tone[] tones = AbstractToneSystem.tones(
                toneStock,
                (oneBelow != null) ? oneBelow.ipnName : lowestToneIpnName,
                octaves);
        
        final DifferenceToneInversions differenceToneInversions = new DifferenceToneInversions(
                new DifferenceToneInversions.Configuration(
                    tones,
                    ToneSystem.semitoneSteps(narrowestInterval),
                    ToneSystem.semitoneSteps(widestInterval),
                    deviation)
            );
        differenceToneInversions.removeDissonant(false);
        
        return differenceToneInversions;
    }
    
    private List<String> intervalColumns(List<Tone> differenceTones, DifferenceToneInversions differenceToneInversions) {
        final List<String> intervalColumns = new ArrayList<>();
        for (int i = 1; i < ToneSystem.INTERVAL_NAMES.length - 1; i++) // leave out UNISON and OCTAVE
            intervalColumns.add(ToneSystem.INTERVAL_NAMES[i]);
        
        // find all actually occurring interval-names
        final Set<String> occurringIntervalColumns = new HashSet<>();
        for (Tone tone : differenceTones)
            for (DifferenceToneInversions.TonePair tonePair : differenceToneInversions.getIntervalsGenerating(tone))
                occurringIntervalColumns.add(tonePair.intervalName());
        
        // remove all interval-names that do not occur in this differenceToneInversions
        for (final Iterator<String> iterator = intervalColumns.iterator(); iterator.hasNext(); ) {
            final String intervalName = iterator.next();
            if (occurringIntervalColumns.contains(intervalName) == false)
                iterator.remove();
        }
        
        return Collections.unmodifiableList(intervalColumns);
    }
    
    private List<List<String>> intervalTableCells(List<String> tableColumns, List<Tone> differenceTones, DifferenceToneInversions differenceToneInversions) {
        final List<List<String>> tableCells = new ArrayList<>(differenceTones.size());
        for (Tone tone : differenceTones) {
            // prepare row with empty values
            final List<String> row = new ArrayList<>(tableColumns.size());
            for (int i = 0; i < tableColumns.size(); i++)
                row.add("");
            tableCells.add(row);
            
            // set existing values into row
            for (DifferenceToneInversions.TonePair interval : differenceToneInversions.getIntervalsGenerating(tone)) {
                final int columnIndex = tableColumns.indexOf(interval.intervalName());
                final String intervalTones = interval.lowerTone().ipnName+"-"+interval.upperTone().ipnName;
                row.set(columnIndex, intervalTones);
            }
        }
        return Collections.unmodifiableList(tableCells);
    }

    private void writeText(List<Tone> differenceTones, List<String> tableColumns, List<List<String>> tableCells, StringBuilder sb) {
        // write table column header
        final int COLUMN_WIDTH = 13;
        sb.append("       "); // 7: leave out first two columns with diff-tone name and number of intervals
        for (String intervalName : tableColumns)
            StringUtil.appendColumn(intervalName, COLUMN_WIDTH, 1, sb);
        sb.append(StringUtil.NEWLINE);
        
        // write table
        for (int i = 0; i < differenceTones.size(); i++) {
            final Tone tone = differenceTones.get(i);
            StringUtil.appendColumn(tone.ipnName, 3, 1, sb);
            
            final List<String> row = tableCells.get(i);
            StringUtil.appendColumn(""+row.stream().filter(s -> s.isEmpty() == false).count(), 2, 1, sb);
            
            for (final String intervalTones : row)
                StringUtil.appendColumn(intervalTones, COLUMN_WIDTH, 1, sb);

            sb.append(StringUtil.NEWLINE);
        }
    }
}