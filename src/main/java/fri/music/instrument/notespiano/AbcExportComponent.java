package fri.music.instrument.notespiano;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.Toolkit;
import java.awt.datatransfer.StringSelection;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTextArea;
import fri.music.player.Note;
import fri.music.player.notelanguage.MelodyFactory;
import fri.music.player.notelanguage.abc.AbcExport;
import fri.music.swingutils.TextAreaActions;

/**
 * Provides ABC header fields and a text-area rendering the export result.
 */
public class AbcExportComponent extends JSplitPane
{
    private static final String HELP_TEXT = """
            <html><body>
            <ol>
            <li>Fill in fields below</li>
            <li>Click "Translate to ABC"</li>
            <li>Select an ABC converter address</li>
            <li>Open address in web-browser</li>
            <li>Copy ABC text (Ctrl-A, Ctrl-C)</li>
            <li>Paste text into web-browser field</li>
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

    public AbcExportComponent(final String notesText) {
        super(JSplitPane.HORIZONTAL_SPLIT);
        
        final int ROWS = 12;
        final int COLUMNS = 24;

        final JTextArea abcTextarea = new JTextArea(ROWS, COLUMNS);
        new TextAreaActions(abcTextarea); // adds itself as mouse-listener to textarea
        abcTextarea.setToolTipText("The ABC Text Generated from Notes");
        
        final JScrollPane abcTextScrollPane = new JScrollPane(abcTextarea);
        abcTextScrollPane.setBorder(BorderFactory.createTitledBorder("ABC Text"));
        abcTextScrollPane.setAlignmentX(JComponent.CENTER_ALIGNMENT);
        abcTextScrollPane.setAlignmentY(JComponent.CENTER_ALIGNMENT);
        
        final AbcExportConfigurationPanel configuration = new AbcExportConfigurationPanel();
        
        final JLabel help = new JLabel(HELP_TEXT);
        final JPanel helpPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        helpPanel.add(help);
        configuration.fieldsPanel.add(helpPanel, 0); // put help text on top of fields
        
        final JButton exportButton = new JButton("Translate to ABC");
        exportButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                writeExportToTextarea(notesText, abcTextarea, configuration);
            }
        });
        final JPanel exportButtonPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
        exportButtonPanel.add(exportButton);
        
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
    
    private void writeExportToTextarea(
            String notesText, 
            JTextArea abcText,
            AbcExportConfigurationPanel configuration)
    {
        final MelodyFactory melodyFactory = new MelodyFactory();
        final Note[][] notes = melodyFactory.translate(notesText);
        final AbcExport exportToAbc = new AbcExport(notes);
        final String abc = exportToAbc.export(configuration.getExportToAbcConfiguration());
        abcText.setText(abc);
    }
}