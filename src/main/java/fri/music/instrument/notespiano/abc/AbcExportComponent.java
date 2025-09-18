package fri.music.instrument.notespiano.abc;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.Toolkit;
import java.awt.datatransfer.StringSelection;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Objects;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTextArea;
import fri.music.player.notelanguage.MelodyFactory;
import fri.music.player.notelanguage.abc.AbcExport;
import fri.music.swingutils.text.TextAreaActions;

/**
 * Provides ABC header fields and a text-area rendering the export result.
 */
public class AbcExportComponent extends JSplitPane
{
    private static final String HELP_TEXT = """
            <html><body>
            <ol>
            <li>Select an ABC converter address</li>
            <li>Open address in web-browser</li>
            <li>Fill in fields below</li>
            <li>Click "Translate to ABC"<br/>
                (this automatically copies ABC text)</li>
            <li>Paste into web-browser field (Ctrl-V)</li>
            </ol>
            </body></html>""";

    private static final String[] ABC_CONVERTER_URLS = new String[] {
            "https://www.abcjs.net/abcjs-editor",
            "https://michaeleskin.com/abctools/abctools.html",
            "https://notabc.app/abc-converter/",
            "https://editor.drawthedots.com/",
            "https://abc.hieuthi.com/",
            "https://abc.rectanglered.com/",
            "https://www.abctransposer.de/",
            "https://www.maztr.com/sheetmusiceditor",
            "https://spuds.thursdaycontra.com/SPUDSConverter.html"
        };

    protected final MelodyFactory melodyFactory;
    protected final JCheckBox includeMelody;
    
    public AbcExportComponent(final String notesText, MelodyFactory melodyFactory, boolean includeMelodyOption) {
        super(JSplitPane.HORIZONTAL_SPLIT);
        
        this.melodyFactory = Objects.requireNonNull(melodyFactory);
        
        final int ROWS = 12;
        final int COLUMNS = 24;

        final JTextArea abcTextarea = new JTextArea(ROWS, COLUMNS);
        abcTextarea.setToolTipText("The ABC Text Generated from Notes");
        new TextAreaActions(abcTextarea); // adds itself as mouse-listener to textarea
        
        final JScrollPane abcTextScrollPane = new JScrollPane(abcTextarea);
        abcTextScrollPane.setBorder(BorderFactory.createTitledBorder("ABC Text"));
        abcTextScrollPane.setAlignmentX(JComponent.CENTER_ALIGNMENT);
        abcTextScrollPane.setAlignmentY(JComponent.CENTER_ALIGNMENT);
        
        final AbcExportConfigurationPanel configuration = new AbcExportConfigurationPanel();
        
        final JLabel help = new JLabel(HELP_TEXT);
        final JPanel helpPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        helpPanel.add(help);
        configuration.fieldsPanel.add(helpPanel, 0); // put help text on top of fields
        
        if (includeMelodyOption) {
            includeMelody = new JCheckBox("Include Melody", true);
            includeMelody.setToolTipText("Include the Notes the Intervals Were Generated For");
        }
        else {
            includeMelody = null;
        }
        
        final JButton exportButton = new JButton("Translate to ABC");
        exportButton.setToolTipText("Generate ABC Notes into Right-Side Textarea");
        exportButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                writeExportToTextarea(notesText, abcTextarea, configuration);
            }
        });
        final JPanel exportButtonPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
        exportButtonPanel.add(exportButton);
        if (includeMelody != null)
            exportButtonPanel.add(includeMelody);
        
        final JPanel configPanel = new JPanel(new BorderLayout());
        configPanel.add(configuration.topPanel, BorderLayout.NORTH);
        configPanel.add(exportButtonPanel, BorderLayout.SOUTH);
        
        final JComboBox<String> abcUrls = new JComboBox<>(ABC_CONVERTER_URLS);
        abcUrls.setBorder(BorderFactory.createTitledBorder("ABC Converters - Selection Copies Address to System Clipboard"));
        abcUrls.setToolTipText("Use Ctrl-V to Paste Selected URL in your Web-Browser's Addressline");
        abcUrls.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                final String selectedUrl = (String) abcUrls.getSelectedItem();
                StringSelection stringSelection = new StringSelection(selectedUrl);
                Toolkit
                    .getDefaultToolkit()
                    .getSystemClipboard()
                    .setContents(stringSelection, null);
            }
        });
        
        final JPanel textAreaAndUrlSelection = new JPanel(new BorderLayout());
        textAreaAndUrlSelection.add(abcTextScrollPane, BorderLayout.CENTER);
        textAreaAndUrlSelection.add(abcUrls, BorderLayout.SOUTH);
                
        setLeftComponent(configPanel);
        setRightComponent(textAreaAndUrlSelection);
        
        // initially show text converted to ABC
        writeExportToTextarea(notesText, abcTextarea, configuration);
        
        setResizeWeight(0.28);
    }
    
    private void writeExportToTextarea(String notesText, JTextArea abcText, AbcExportConfigurationPanel configuration) {
        final String abc = export(configuration.getExportToAbcConfiguration(), notesText, false);
        abcText.setText(abc);
        abcText.setCaretPosition(0);
        // select all and copy text to clipboard
        abcText.selectAll();
        abcText.copy();
    }

    /**
     * Performs the export. To be overridden for combination with melody.
     * @param configuration the ABC header data.
     * @param notesText the notes to convert to ABC.
     * @return the ABC text to put into local text-area.
     */
    protected String export(AbcExport.Configuration configuration, String notesText, boolean includeTuning) {
        final AbcExport abcExport = new AbcExport(
                melodyFactory.translate(notesText), 
                includeTuning ? melodyFactory.tuning : null,
                melodyFactory.getBeatsPerMinute(),
                melodyFactory.getBeatsPerBar(),
                melodyFactory.getBeatType());
        return abcExport.export(configuration);
    }
}