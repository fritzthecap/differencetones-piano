package fri.music.demos;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTextArea;
import fri.music.instrument.notespiano.AbcExportConfigurationPanel;
import fri.music.player.Note;
import fri.music.player.notelanguage.MelodyFactory;
import fri.music.player.notelanguage.abc.AbcExport;

public class ExportToAbcDemo
{
    public static void main(String[] args) {
        final int ROWS = 12;
        final int COLUMNS = 24;
        final JTextArea notesText = new JTextArea(ROWS, COLUMNS);
        notesText.setText(NoteExamples.TUBULAR_BELLS);
        final JScrollPane notesTextScrollPane = new JScrollPane(notesText);
        notesTextScrollPane.setBorder(BorderFactory.createTitledBorder("Notes Text"));
        
        final JTextArea abcText = new JTextArea(ROWS, COLUMNS);
        abcText.setEditable(false);
        final JScrollPane abcTextScrollPane = new JScrollPane(abcText);
        abcTextScrollPane.setBorder(BorderFactory.createTitledBorder("ABC Text"));
        
        final JSplitPane textSplitPanel = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
        textSplitPanel.setLeftComponent(notesTextScrollPane);
        textSplitPanel.setRightComponent(abcTextScrollPane);
        
        final AbcExportConfigurationPanel configuration = new AbcExportConfigurationPanel();
        
        final JButton exportButton = new JButton("Export");
        exportButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                final MelodyFactory melodyFactory = new MelodyFactory();
                final Note[][] notes = melodyFactory.translate(notesText.getText());
                final AbcExport exportToAbc = new AbcExport(notes);
                final String abc = exportToAbc.export(configuration.getExportToAbcConfiguration());
                abcText.setText(abc);
            }
        });
        final JPanel exportButtonPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
        exportButtonPanel.add(exportButton);
        
        final JPanel configPanel = new JPanel(new BorderLayout());
        configPanel.add(configuration.topPanel, BorderLayout.NORTH);
        configPanel.add(exportButtonPanel, BorderLayout.SOUTH);
                
        final JPanel mainPanel = new JPanel();
        final BoxLayout mainLayout = new BoxLayout(mainPanel, BoxLayout.X_AXIS);
        mainPanel.setLayout(mainLayout);
        mainPanel.add(configPanel);
        textSplitPanel.setAlignmentX(JComponent.CENTER_ALIGNMENT);
        textSplitPanel.setAlignmentY(JComponent.CENTER_ALIGNMENT);
        mainPanel.add(textSplitPanel);
        
        final JFrame frame = new JFrame("Export to ABC Notation");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.getContentPane().add(mainPanel);
        frame.pack();
        frame.setLocationRelativeTo(null);
        frame.setVisible(true);
    }
}