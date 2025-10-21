package fri.music.demos;

import javax.swing.JFrame;
import javax.swing.SwingUtilities;
import fri.music.instrument.configuration.ConfiguredPianoFactoryLauncher;

public class ConfiguredPianoFactoryDemo
{
    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            final ConfiguredPianoFactoryLauncher configuredPianoFactoryStart = new ConfiguredPianoFactoryLauncher(null);
            configuredPianoFactoryStart.frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE); // this is a demo
        });
    }
}