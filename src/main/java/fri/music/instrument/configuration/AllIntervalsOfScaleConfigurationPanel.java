package fri.music.instrument.configuration;

import java.awt.Font;
import java.util.List;
import javax.swing.BoxLayout;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import fri.music.AbstractToneSystem;
import fri.music.Tone;
import fri.music.ToneSystem;
import fri.music.differencetones.DifferenceToneInversions;
import fri.music.utils.StringUtil;
import fri.music.utils.swing.text.TextAreaUtil;

/**
 * Lets configure octaves and lowest tone of a scale.
 */
class AllIntervalsOfScaleConfigurationPanel extends ToneRangeConfigurationPanel
{
    private final DifferenceTonesConfigurationPanel differenceTonesConfigurationPanel;
    
    public AllIntervalsOfScaleConfigurationPanel(DifferenceTonesConfigurationPanel differenceTonesConfigurationPanel) {
        super(4, "C", 2, false);
        this.differenceTonesConfigurationPanel = differenceTonesConfigurationPanel;
    }

    public void startDialog() {
        final JPanel dialogPanel = new JPanel();
        dialogPanel.setLayout(new BoxLayout(dialogPanel, BoxLayout.Y_AXIS));
        dialogPanel.add(this.panel);
        
        final int answer = JOptionPane.showConfirmDialog(
                differenceTonesConfigurationPanel.panel, 
                dialogPanel,
                "Configure Tone Range", 
                JOptionPane.OK_CANCEL_OPTION,
                JOptionPane.PLAIN_MESSAGE);
        
        if (answer == JOptionPane.OK_OPTION)
            showResultInTextDialog();
    }

    private void showResultInTextDialog() {
        final ToneSystem toneSystem = differenceTonesConfigurationPanel.getToneSystem();
        final double deviation = differenceTonesConfigurationPanel.getDeviation();
        final String narrowestInterval = differenceTonesConfigurationPanel.getNarrowestInterval();
        final String widestInterval = differenceTonesConfigurationPanel.getWidestInterval();
        
        final int octaves = getOctaves();
        final String lowestToneIpnName = getLowestToneIpnName();
        final Tone[] tones = AbstractToneSystem.tones(
                toneSystem.tones(),
                lowestToneIpnName,
                toneSystem.baseToneIpnName(),
                octaves);
        
        final DifferenceToneInversions differenceToneInversions = new DifferenceToneInversions(
                new DifferenceToneInversions.Configuration(
                    tones,
                    ToneSystem.semitoneSteps(narrowestInterval),
                    ToneSystem.semitoneSteps(widestInterval),
                    deviation)
            );
        differenceToneInversions.removeDissonant(false);
        
        final StringBuilder sb = new StringBuilder();
        final String titleLine = 
                differenceTonesConfigurationPanel.getGlobalHeadingLines()+
                "Base Tone:  "+lowestToneIpnName+StringUtil.NEWLINE+
                "Octave(s):  "+octaves+StringUtil.NEWLINE;
        StringUtil.appendLine(titleLine, sb);
        
        differenceToneInversions.differenceTones().stream().forEach(tone -> {
            final List<DifferenceToneInversions.TonePair> intervals = differenceToneInversions.getIntervalsGenerating(tone);
            StringUtil.appendColumn(tone.ipnName, 3, sb);
            StringUtil.appendColumn(""+intervals.size(), 2, sb);
            StringUtil.appendLine(""+intervals, sb);
        });
        
        TextAreaUtil.showResultInTextDialog(
                differenceTonesConfigurationPanel.panel,
                "All Difference-Tone Intervals for "+toneSystem.name(),
                sb.toString(),
                Font.MONOSPACED);
    }
}