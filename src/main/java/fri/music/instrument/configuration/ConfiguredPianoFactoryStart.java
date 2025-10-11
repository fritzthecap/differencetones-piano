package fri.music.instrument.configuration;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JPanel;
import fri.music.instrument.PianoWithSound;
import fri.music.swingutils.window.DialogStarter;
import fri.music.swingutils.window.FrameStarter;
import fri.music.swingutils.window.WindowClosingManager;

/**
 * The piano configuration API showcase.
 */
public class ConfiguredPianoFactoryStart
{
    public final JFrame frame;
    
    private final WindowClosingManager closeListener;
    private int numberOfPianos;
    
    public ConfiguredPianoFactoryStart(JFrame parentFrame) {
        final ConfiguredPianoFactory configuredPianoFactory = new ConfiguredPianoFactory();
        
        this.closeListener = new WindowClosingManager();
        
        final JButton showPianoButton = new JButton("Show Piano");
        showPianoButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                final PianoWithSound piano = configuredPianoFactory.getPiano();
                final JComponent pianoPanel = piano.getKeyboard();
                
                numberOfPianos++;
                final String title = piano.getClass().getSimpleName()+" "+numberOfPianos;
                
                final JDialog dialog = DialogStarter.start(
                        title,
                        frame,
                        pianoPanel,
                        true);
                closeListener.add(dialog, piano.getWindowClosingListener());
            }
        });
        
        final JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
        buttonPanel.add(showPianoButton);
        
        final JPanel mainPanel = new JPanel(new BorderLayout());
        mainPanel.add(configuredPianoFactory.panel, BorderLayout.CENTER);
        mainPanel.add(buttonPanel, BorderLayout.SOUTH);
        
        this.frame = FrameStarter.start(parentFrame, "Piano Configuration Showcase", mainPanel, closeListener);
    }
}