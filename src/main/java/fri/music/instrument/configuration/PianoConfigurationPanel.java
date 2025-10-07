package fri.music.instrument.configuration;

import java.awt.FlowLayout;
import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JCheckBox;
import javax.swing.JPanel;
import javax.swing.JSlider;
import fri.music.instrument.PianoWithSound;
import fri.music.utils.StringUtil;

/**
 * A panel that lets configure most options for different
 * kinds of Piano implementations.
 */
public class PianoConfigurationPanel extends ToneRangeConfigurationPanel
{
    protected final JPanel displayOptionsPanel;
    
    private JSlider blackKeyPixelWidth;
    private JCheckBox showIpnNameOnKey;
    private JCheckBox showMidiNumberAsTooltip;
    private JCheckBox colouredOctaves;
    
    public PianoConfigurationPanel(PianoWithSound.Configuration configuration) {
        super(
                (configuration != null) ? configuration.octaves : 0,
                (configuration != null) ? StringUtil.getUntilFirstNumber(configuration.lowestToneIpnName) : null,
                (configuration != null) ? configuration.lowestToneOctaveBasedOnC : 0
            );

        // field construction
        buildPianoConfigurationFields(configuration);
        
        this.displayOptionsPanel = new JPanel();
        displayOptionsPanel.setLayout(new BoxLayout(displayOptionsPanel, BoxLayout.Y_AXIS));
        displayOptionsPanel.add(showIpnNameOnKey);
        displayOptionsPanel.add(showMidiNumberAsTooltip);
        displayOptionsPanel.add(colouredOctaves);
        final JPanel moveLeftOptionsPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        moveLeftOptionsPanel.setBorder(BorderFactory.createTitledBorder("Display Options"));
        moveLeftOptionsPanel.add(displayOptionsPanel);

        panel.add(moveLeftOptionsPanel);
        panel.add(blackKeyPixelWidth);
    }

    /** @return a piano configuration built from all UI fields. */
    public PianoWithSound.Configuration getPianoConfiguration() {
        return new PianoWithSound.Configuration(
            getOctaves(),
            getLowestToneIpnName(),
            false,
            blackKeyPixelWidth.getValue(),
            showIpnNameOnKey.isSelected(),
            showMidiNumberAsTooltip.isSelected(),
            colouredOctaves.isSelected()
        );
    }
    
    
    private void buildPianoConfigurationFields(PianoWithSound.Configuration configurationParam) {
        final PianoWithSound.Configuration configuration;
        if (configurationParam == null)
            configuration = new PianoWithSound.Configuration(DEFAULT_OCTAVES, DEFAULT_LOWEST_TONE_IPNNAME);
        else
            configuration = configurationParam;
        
        this.blackKeyPixelWidth = new JSlider(4, 60, configuration.blackKeyWidth);
        blackKeyPixelWidth.setBorder(BorderFactory.createTitledBorder("Black Key Pixel Width"));
        blackKeyPixelWidth.setToolTipText("Bigger or Smaller Keyboard Keys");
        blackKeyPixelWidth.setPaintLabels(true);
        blackKeyPixelWidth.setPaintTicks(true);
        blackKeyPixelWidth.setMajorTickSpacing(4);
        
        this.showIpnNameOnKey = new JCheckBox("IPN-Names on Keys");
        showIpnNameOnKey.setToolTipText("Show Note Names on Piano Keys");
        showIpnNameOnKey.setSelected(configuration.showIpnNameOnKey);
        
        this.showMidiNumberAsTooltip = new JCheckBox("MIDI-Numbers as Tooltips");
        showMidiNumberAsTooltip.setToolTipText("Show MIDI-Numbers as Tooltips on Piano Keys (e.g. C4 is 60)");
        showMidiNumberAsTooltip.setSelected(configuration.showMidiNumberAsTooltip);
        
        this.colouredOctaves = new JCheckBox("Coloured Octaves");
        colouredOctaves.setToolTipText("Show Every Octave with a Different Color");
        colouredOctaves.setSelected(configuration.colouredOctaves);
    }
}