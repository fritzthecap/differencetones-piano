package fri.music.demos;

import javax.swing.JComponent;
import javax.swing.JFrame;
import fri.music.SoundChannel;
import fri.music.instrument.PianoWithSound;
import fri.music.instrument.PianoWithVolume;
import fri.music.instrument.notespiano.NotesPianoPlayer;
import fri.music.wavegenerator.SineWaveSoundChannel;

public class NotesPianoPlayerDemo
{
    public static void main(String[] args) {
        final int octaves = 7;
        final String lowestToneIpnName = "C2";
        
        final JFrame frame = new JFrame("Write Notes with Piano and Let Play Them ("+octaves+" Octaves)");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        
        final PianoWithSound.Configuration config = new PianoWithSound.Configuration(
                octaves, 
                lowestToneIpnName,
                //true, // vertical
                13);
        final SoundChannel soundChannel = new SineWaveSoundChannel(null);
        final NotesPianoPlayer player = new NotesPianoPlayer(new PianoWithVolume(config, soundChannel));
        final JComponent playerPanel = player.getPlayer(NoteExamples.TUBULAR_BELLS);
        
        frame.addWindowListener(player.getWindowClosingListener());
        frame.add(playerPanel);
        frame.pack();
        frame.setLocationRelativeTo(null);
        frame.setVisible(true);
    }
}