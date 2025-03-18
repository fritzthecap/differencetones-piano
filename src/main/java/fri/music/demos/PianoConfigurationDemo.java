package fri.music.demos;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.WindowConstants;
import fri.music.SoundChannel;
import fri.music.instrument.ConfigurationPanel;
import fri.music.instrument.PianoWithSound;

public class PianoConfigurationDemo
{
    private static int instance = 0;
    
    public static void main(String[] args) {
        final ConfigurationPanel configurationPanel = new ConfigurationPanel();
        
        final JButton showPianoButton = new JButton("Show Piano");
        showPianoButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                final SoundChannel soundChannel = configurationPanel.getSoundChannel();
                final PianoWithSound piano = new PianoWithSound(
                        configurationPanel.getPianoConfiguration(), 
                        soundChannel);
                final JComponent pianoPanel = piano.getKeyboard();
                
                instance++;
                final JFrame frame = startFrame("Piano "+instance, pianoPanel, WindowConstants.DISPOSE_ON_CLOSE);
                frame.addWindowListener(new WindowAdapter() {
                    @Override
                    public void windowClosing(WindowEvent windowEvent) {
                        piano.destroyKeyboard(pianoPanel);
                    }
                });
            }
        });
        
        final JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
        buttonPanel.setBorder(BorderFactory.createLineBorder(Color.LIGHT_GRAY, 4, true));
        buttonPanel.add(showPianoButton);
        
        final JPanel mainPanel = new JPanel(new BorderLayout());
        mainPanel.add(configurationPanel.panel, BorderLayout.CENTER);
        mainPanel.add(buttonPanel, BorderLayout.SOUTH);
        
        startFrame("Piano Configuration Demo", mainPanel, WindowConstants.EXIT_ON_CLOSE);
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