package fri.music.demos;

import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.SwingUtilities;
import fri.music.SoundChannel;
import fri.music.instrument.PianoWithSound;
import fri.music.instrument.PianoWithVolume;
import fri.music.instrument.notespiano.NotesPianoPlayer;
import fri.music.wavegenerator.SineWaveSoundChannel;

public class NotesPianoPlayerDemo
{
    private static final String TUBULAR_BELLS = """
155
4/4
-/2. -/8 e5/8 
a5/8 e5/8 b5/8 e5/8 {g5/8 a5/8} e5/8 c6/8 
3/4
e5/8 d6/8 e5/8 {b5/8 c6/8} e5/8
4/4
a5/8 e5/8 b5/8 e5/8 {g5/8 a5/8} e5/8 c6/8 
e5/8 d6/8 e5/8 {b5/8 c6/8} e5/8 b5/8 e5/8
""";
    
    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            final int octaves = 7;
            final String lowestToneIpnName = "C2";
            
            final JFrame frame = new JFrame("Record Notes with Piano and Play Them");
            frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            
            final PianoWithSound.Configuration config = new PianoWithSound.Configuration(
                    octaves, 
                    lowestToneIpnName,
                    //true, // vertical
                    13);
            final SoundChannel soundChannel = new SineWaveSoundChannel(null);
            final NotesPianoPlayer player = new NotesPianoPlayer(new PianoWithVolume(config, soundChannel));
            final JComponent playerPanel = player.getPlayer(TUBULAR_BELLS);
            
            frame.addWindowListener(player.getWindowClosingListener());
            frame.getContentPane().add(playerPanel);
            frame.pack();
            frame.setLocationRelativeTo(null);
            frame.setVisible(true);
        });
    }
}