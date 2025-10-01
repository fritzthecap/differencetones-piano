package fri.music.demos;

import javax.swing.JFrame;
import javax.swing.SwingUtilities;
import fri.music.instrument.configuration.ConfiguredPianoFactoryStart;

public class ConfiguredPianoFactoryDemo
{
    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            final ConfiguredPianoFactoryStart configuredPianoFactoryStart = new ConfiguredPianoFactoryStart(null);
            configuredPianoFactoryStart.frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE); // this is a demo
        });
    }
}