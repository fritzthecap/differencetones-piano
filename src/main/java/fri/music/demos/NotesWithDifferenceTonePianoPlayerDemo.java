package fri.music.demos;

import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.SwingUtilities;
import fri.music.instrument.PianoWithSound;
import fri.music.instrument.notespiano.NoteExamples;
import fri.music.instrument.notespiano.NotesPianoPlayer;
import fri.music.instrument.notespiano.wave.NotesWithDifferenceTonePianoPlayer;
import fri.music.instrument.wave.DifferenceToneForNotesPiano;
import fri.music.wavegenerator.SineWaveSoundChannel;
import fri.music.wavegenerator.WaveSoundChannel;

public class NotesWithDifferenceTonePianoPlayerDemo
{
    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            final int octaves = 7;
            final String lowestToneIpnName = "C2";
            
            final JFrame frame = new JFrame("Auto-Compose Difference-Tone Intervals for Written Notes");
            frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            
            final PianoWithSound.Configuration config = new PianoWithSound.Configuration(octaves, lowestToneIpnName);
            final WaveSoundChannel soundChannel = new SineWaveSoundChannel(null);
            final NotesPianoPlayer player = new NotesWithDifferenceTonePianoPlayer(new DifferenceToneForNotesPiano(config, soundChannel));
            final JComponent playerPanel = player.getPlayer(NoteExamples.AUGUSTIN.notes());
            
            frame.addWindowListener(player.getWindowClosingListener());
            frame.getContentPane().add(playerPanel);
            frame.pack();
            frame.setLocationRelativeTo(null);
            frame.setVisible(true);
        });
    }
}