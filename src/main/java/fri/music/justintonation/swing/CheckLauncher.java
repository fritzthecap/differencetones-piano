package fri.music.justintonation.swing;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import fri.music.swingutils.DialogUtil;
import fri.music.swingutils.text.TextAreaActions;

public class CheckLauncher
{
    public JComponent panel;
    
    public CheckLauncher(final String dialogTitle) {
        final ConfigurationPanel configurationPanel = new ConfigurationPanel();
        
        final JButton showDiagnosisButton = new JButton("Check Purity");
        
        final JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
        buttonPanel.setBorder(BorderFactory.createLineBorder(Color.LIGHT_GRAY, 4, true));
        buttonPanel.add(showDiagnosisButton);
        
        this.panel = new JPanel(new BorderLayout());
        panel.add(configurationPanel.panel, BorderLayout.CENTER);
        panel.add(buttonPanel, BorderLayout.SOUTH);
        
        showDiagnosisButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                final String[] result = configurationPanel.getTitleAndResult();
                if (result == null) {
                    JOptionPane.showMessageDialog(panel, "Please choose at least one just-intonation tuning!");
                    return;
                }
                
                final JTextArea textArea = new JTextArea(result[1]);
                textArea.setTabSize(4);
                textArea.setRows(32);
                //textArea.setColumns(120);
                
                TextAreaActions fontActions = new TextAreaActions(textArea);
                // make font bigger
                fontActions.magnifyFont(true, textArea);
                fontActions.magnifyFont(true, textArea);
                
                DialogUtil.showModelessDialog(
                        dialogTitle, 
                        panel, 
                        new JScrollPane(textArea), 
                        null, 
                        null, 
                        true);
            }
        });
    }
}
