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
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTextArea;
import fri.music.player.Note;
import fri.music.player.notelanguage.AbcExport;
import fri.music.player.notelanguage.MelodyFactory;
import fri.music.swingutils.TextAreaActions;

/**
 * Provides ABC header fields and a text-area rendering the export result.
 */
public class AbcExportComponent extends JSplitPane
{
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
        //abcText.setEditable(false);
        abcTextarea.addMouseListener(new TextAreaActions(abcTextarea));
        
        final JScrollPane abcTextScrollPane = new JScrollPane(abcTextarea);
        abcTextScrollPane.setBorder(BorderFactory.createTitledBorder("ABC Text"));
        abcTextScrollPane.setAlignmentX(JComponent.CENTER_ALIGNMENT);
        abcTextScrollPane.setAlignmentY(JComponent.CENTER_ALIGNMENT);
        
        final AbcExportConfigurationPanel configuration = new AbcExportConfigurationPanel();
        
        final JButton exportButton = new JButton("Export");
        exportButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                writeExportToTextarea(notesText, abcTextarea, configuration);
            }
        });
        final JPanel exportButtonPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
        exportButtonPanel.add(exportButton);
        
        final JPanel configPanel = new JPanel(new BorderLayout());
        configPanel.add(configuration.panel, BorderLayout.NORTH);
        configPanel.add(exportButtonPanel, BorderLayout.SOUTH);
        
        final JComboBox<String> abcUrls = new JComboBox<>(ABC_CONVERTER_URLS);
        abcUrls.setBorder(BorderFactory.createTitledBorder("Selecting an ABC Converter Copies its URL to System Clipboard"));
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
        
        writeExportToTextarea(notesText, abcTextarea, configuration);
        
        setResizeWeight(0.3);
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
    
    
    public static void main(String[] args) {
        final String TEST_TEXT = "C4/4 D4/4 E4/4 F4/4 G4/4 A4/4 B4/4 C5/4";
        final AbcExportComponent exportPanel = new AbcExportComponent(TEST_TEXT);
        final javax.swing.JFrame frame = new javax.swing.JFrame("Export to ABC Notation");
        frame.setDefaultCloseOperation(javax.swing.JFrame.EXIT_ON_CLOSE);
        frame.getContentPane().add(exportPanel);
        frame.pack();
        frame.setLocationRelativeTo(null);
        frame.setVisible(true);
    }
}