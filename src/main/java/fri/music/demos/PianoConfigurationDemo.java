package fri.music.demos;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.ArrayList;
import java.util.List;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;
import fri.music.instrument.ConfigurationPanel;
import fri.music.instrument.PianoWithSound;

public class PianoConfigurationDemo
{
    private static List<JFrame> frames = new ArrayList<>();
    
    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            final ConfigurationPanel configurationPanel = new ConfigurationPanel();
            
            final JButton showPianoButton = new JButton("Show Piano");
            showPianoButton.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    final PianoWithSound piano = configurationPanel.getPiano();
                    final JComponent pianoPanel = piano.getKeyboard();
                    
                    final JFrame frame = startFrame(
                            piano.getClass().getSimpleName()+" "+(frames.size() + 1), 
                            pianoPanel, 
                            WindowConstants.DISPOSE_ON_CLOSE);
                    frame.addWindowListener(piano.getWindowClosingListener());
                    frames.add(frame);
                }
            });
            
            final JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
            buttonPanel.setBorder(BorderFactory.createLineBorder(Color.LIGHT_GRAY, 4, true));
            buttonPanel.add(showPianoButton);
            
            final JPanel mainPanel = new JPanel(new BorderLayout());
            mainPanel.add(configurationPanel.panel, BorderLayout.CENTER);
            mainPanel.add(buttonPanel, BorderLayout.SOUTH);
            
            final JFrame mainFrame = startFrame("Piano Configuration Showcase", mainPanel, WindowConstants.EXIT_ON_CLOSE);
            // MUST release wave sound generator resources for every displaying piano!
            mainFrame.addWindowListener(new WindowAdapter() {
                @Override
                public void windowClosing(WindowEvent e) {
                    for (JFrame frame : frames)
                        if (frame.isVisible())
                            frame.dispatchEvent(new WindowEvent(frame, WindowEvent.WINDOW_CLOSING));
                }
            });
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