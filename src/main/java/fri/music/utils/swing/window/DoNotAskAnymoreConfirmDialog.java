package fri.music.utils.swing.window;

import java.awt.BorderLayout;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;

public class DoNotAskAnymoreConfirmDialog
{
    private final String rememberAnswerCheckboxText;
    private Boolean answer;
    
    public DoNotAskAnymoreConfirmDialog() {
        this(null);
    }
    
    public DoNotAskAnymoreConfirmDialog(String rememberAnswerCheckboxText) {
        this.rememberAnswerCheckboxText = (rememberAnswerCheckboxText != null)
                ? rememberAnswerCheckboxText
                : "Remember That and Don't Ask Any More";
    }
    
    public boolean show(JComponent parent, String messageText) {
        if (answer != null) // rememberAnswer checkbox has been selected once
            return answer.booleanValue();
        
        final String htmlMessageText = 
                "<html>"+
                "<p style='width: 240;'>"+messageText+"</p>"+
                "<hr/>"+ // visual separation to checkbox
                "</html>";
        
        final JLabel messageLabel = new JLabel(htmlMessageText);
        final JCheckBox rememberAnswer = new JCheckBox(rememberAnswerCheckboxText, false);
        final JPanel panel = new JPanel(new BorderLayout());
        panel.add(messageLabel, BorderLayout.CENTER);
        panel.add(rememberAnswer, BorderLayout.SOUTH);
        
        final boolean dialogAnswer =
            (JOptionPane.YES_OPTION == JOptionPane.showConfirmDialog(
                parent, 
                panel, 
                "Confirm Erase Intervals Text", 
                JOptionPane.YES_NO_OPTION, 
                JOptionPane.WARNING_MESSAGE)
            );
        
        if (rememberAnswer.isSelected())
            answer = Boolean.valueOf(dialogAnswer);
        
        return dialogAnswer;
    }
}