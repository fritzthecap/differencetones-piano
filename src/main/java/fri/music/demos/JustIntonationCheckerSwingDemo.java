package fri.music.demos;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.WindowConstants;
import fri.music.justintonation.ConfigurationPanel;
import fri.music.justintonation.JustIntonationChecker;

public class JustIntonationCheckerSwingDemo
{
    private static int count = 1;
    
    public static void main(String[] args) {
        final ConfigurationPanel configurationPanel = new ConfigurationPanel();
        
        final JButton showDiagnosisButton = new JButton("Show Diagnosis");
        
        final JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
        buttonPanel.setBorder(BorderFactory.createLineBorder(Color.LIGHT_GRAY, 4, true));
        buttonPanel.add(showDiagnosisButton);
        
        final JPanel mainPanel = new JPanel(new BorderLayout());
        mainPanel.add(configurationPanel.panel, BorderLayout.CENTER);
        mainPanel.add(buttonPanel, BorderLayout.SOUTH);
        
        startFrame(JustIntonationChecker.class.getSimpleName()+" Demo", mainPanel, WindowConstants.EXIT_ON_CLOSE);

        showDiagnosisButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                final String[] result = configurationPanel.getTitleAndResult();
                if (result == null) {
                    JOptionPane.showMessageDialog(mainPanel, "Please choose at least one just-intonation tuning!");
                    return;
                }
                
                final JTextArea textArea = new JTextArea(result[1]);
                textArea.setTabSize(4);
                textArea.setFont(new Font("Monospaced", Font.BOLD, 16));
                textArea.setRows(32);
                textArea.setColumns(120);
                
                startFrame(
                        result[0]+" (Window "+count+")", 
                        new JScrollPane(textArea), 
                        WindowConstants.DISPOSE_ON_CLOSE);
                count++;
            }
        });
    }
    
    private static JFrame startFrame(String title, JComponent content, int actionOnClose) {
        final JFrame frame = new JFrame(title);
        frame.setDefaultCloseOperation(actionOnClose);
        frame.add(content);
        frame.pack();
        frame.setLocationRelativeTo(null);
        frame.setVisible(true);
        return frame;
    }
}