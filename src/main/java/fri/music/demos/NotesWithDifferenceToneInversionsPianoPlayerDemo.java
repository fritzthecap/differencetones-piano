package fri.music.demos;

import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.SwingUtilities;
import fri.music.instrument.PianoWithSound;
import fri.music.instrument.notespiano.wave.NotesWithDifferenceToneInversionsPianoPlayer;
import fri.music.instrument.wave.DifferenceToneInversionsPiano;
import fri.music.wavegenerator.SineWaveSoundChannel;
import fri.music.wavegenerator.WaveSoundChannel;

/**
 * Supports composing difference-tone intervals for melodies.
 */
public class NotesWithDifferenceToneInversionsPianoPlayerDemo
{
    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            final int octaves = 7;
            final String lowestToneIpnName = "C2";
            
            final JFrame frame = new JFrame("Compose Difference-Tone Intervals for a Melody");
            frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            
            final PianoWithSound.Configuration config = new PianoWithSound.Configuration(octaves, lowestToneIpnName);
            final WaveSoundChannel soundChannel = new SineWaveSoundChannel(null);
            final NotesWithDifferenceToneInversionsPianoPlayer player = 
                    new NotesWithDifferenceToneInversionsPianoPlayer(
                            new DifferenceToneInversionsPiano(config, soundChannel));
            //final JComponent playerPanel = player.getPlayer(NoteExamples.WHEN_THE_SAINTS_GO_MARCHING);
            final JComponent playerPanel = player.getPlayer(null);
            
            frame.addWindowListener(player.getWindowClosingListener());
            frame.getContentPane().add(playerPanel);
            frame.pack();
            frame.setLocationRelativeTo(null);
            frame.setVisible(true);
        });
    }
}