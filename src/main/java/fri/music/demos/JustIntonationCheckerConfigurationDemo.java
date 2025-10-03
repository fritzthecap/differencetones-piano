package fri.music.demos;

import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;
import fri.music.justintonation.swing.TuningsCheckLauncher;

public class JustIntonationCheckerConfigurationDemo
{
    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            startFrame(
                    "Check Just-Intonation Tunings for Purity",
                    new TuningsCheckLauncher("Check Just-Intonation Tunings for Purity").panel,
                    WindowConstants.EXIT_ON_CLOSE);
        });
    }
    
    private static JFrame startFrame(String title, JComponent content, int actionOnClose) {
        final JFrame frame = new JFrame(title);
        frame.setDefaultCloseOperation(actionOnClose);
        frame.getContentPane().add(content);
        frame.pack();
        frame.setLocationRelativeTo(null);
        frame.setVisible(true);
        return frame;
    }
}