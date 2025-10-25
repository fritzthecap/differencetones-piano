package fri.music.utils.swing.window;

import java.awt.BorderLayout;
import java.awt.Component;
import java.util.Objects;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;

/**
 * A modal confirm-dialog that may not show but deliver a persistent
 * answer when the user once activated its "Remember" checkbox.
 */
public class DoNotAskAnymoreConfirmDialog
{
    private final String title;
    private final String message;
    private final String rememberAnswerCheckboxText;
    
    private Boolean answer;
    
    public DoNotAskAnymoreConfirmDialog(String title, String message) {
        this(title, message, null);
    }
    public DoNotAskAnymoreConfirmDialog(String title, String message, String rememberAnswerCheckboxText) {
        this.title = Objects.requireNonNull(title);
        this.message = Objects.requireNonNull(message);
        this.rememberAnswerCheckboxText = (rememberAnswerCheckboxText != null)
                ? rememberAnswerCheckboxText
                : "Remember That and Don't Ask Anymore";
    }
    
    public boolean answer(Component parent) {
        if (answer != null) // rememberAnswer checkbox has been selected once
            return answer.booleanValue();
        
        final String htmlMessageText = 
                "<html>"+
                "<p style='width: 240;'>"+message+"</p>"+
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
                title, 
                JOptionPane.YES_NO_OPTION, 
                JOptionPane.WARNING_MESSAGE)
            );
        
        if (rememberAnswer.isSelected())
            answer = Boolean.valueOf(dialogAnswer);
        
        return dialogAnswer;
    }
}