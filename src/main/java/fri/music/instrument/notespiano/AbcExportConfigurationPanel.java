package fri.music.instrument.notespiano;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.JTextField;
import javax.swing.SpinnerModel;
import javax.swing.SpinnerNumberModel;
import fri.music.player.notelanguage.abc.AbcExport;

/**
 * Contains ABC property fields for conversion.
 */
public class AbcExportConfigurationPanel
{
    /** The UI with configuration fields. */
    public final JPanel topPanel;
    public final JPanel fieldsPanel;
    
    private JSpinner songNumberField;
    private JTextField titleField;
    private JTextField subTitleField;
    private JTextField authorField;
    private JTextField dateField;
    private JTextField keyAndClefField;
    private JSpinner numberOfBarsPerLineField;

    /** Get UI by using the public panel of this object. */
    public AbcExportConfigurationPanel() {
        this.fieldsPanel = new JPanel();
        fieldsPanel.setLayout(new BoxLayout(fieldsPanel, BoxLayout.Y_AXIS));
        buildFields(fieldsPanel);
        
        final JPanel layoutPanel = new JPanel(new BorderLayout());
        layoutPanel.add(fieldsPanel, BorderLayout.CENTER);
        
        this.topPanel = layoutPanel;
    }

    /**
     * Result of this panel is a ABC export configuration.
     * @return a configuration object for ABC export.
     */
    public AbcExport.Configuration getExportToAbcConfiguration() {
        return new AbcExport.Configuration(
                (Integer) songNumberField.getValue(),
                titleField.getText(),
                subTitleField.getText(),
                authorField.getText(),
                dateField.getText(),
                keyAndClefField.getText(),
                (Integer) numberOfBarsPerLineField.getValue()
            );
    }
    
    
    private void buildFields(JPanel panel) {
        this.songNumberField = buildNumberField("The ABC 'X' Header Field", 1, 0, 100, 1);
        this.titleField = buildTextField("Title", "The ABC 'T' Header Field");
        this.subTitleField = buildTextField("Subtitle", "The 2nd ABC 'T' Header Field");
        this.authorField = buildTextField("Composer", "The ABC 'C' Header Field");
        this.dateField = buildTextField("Date", "Will be added to Composer");
        dateField.setText("{date}");
        this.keyAndClefField = buildTextField("Key", "The ABC 'K' Header Field, e.g. 'G bass' (Clef is Optional)");
        this.numberOfBarsPerLineField = buildNumberField("Number of Bars per ABC Note Line", 4, 1, 16, 1);
        
        panel.add(buildNumberFieldLayout("Song Number", songNumberField));
        panel.add(titleField);
        panel.add(subTitleField);
        panel.add(authorField);
        panel.add(dateField);
        panel.add(keyAndClefField); 
        panel.add(buildNumberFieldLayout("Number of Bars per Line", numberOfBarsPerLineField));
    }

    private JTextField buildTextField(String title, String tooltip) {
        final JTextField field = new JTextField();
        field.setToolTipText(tooltip);
        field.setBorder(BorderFactory.createTitledBorder(title));
        return field;
    }
    
    private JSpinner buildNumberField(String tooltip, int initial, int min, int max, int step) {
        final SpinnerModel numberModel = new SpinnerNumberModel(initial, min, max, step);
        final JSpinner numberField = new JSpinner(numberModel);
        numberField.setToolTipText(tooltip);
        return numberField;
    }
    
    private JComponent buildNumberFieldLayout(String title, JSpinner field) {
        final JPanel layoutPanel = new JPanel(new FlowLayout(FlowLayout.LEADING));
        layoutPanel.add(field);
        layoutPanel.setBorder(BorderFactory.createTitledBorder(title));
        return layoutPanel;
    }
}