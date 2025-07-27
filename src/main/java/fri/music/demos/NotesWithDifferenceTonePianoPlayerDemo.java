package fri.music.demos;

import javax.swing.JComponent;
import javax.swing.JFrame;
import fri.music.ScaleTypes;
import fri.music.instrument.PianoWithSound;
import fri.music.instrument.notespiano.NotesPianoPlayer;
import fri.music.instrument.notespiano.wave.DifferenceToneForNotesPiano;
import fri.music.instrument.notespiano.wave.NotesWithDifferenceTonePianoPlayer;
import fri.music.wavegenerator.SineWaveSoundChannel;
import fri.music.wavegenerator.WaveSoundChannel;

public class NotesWithDifferenceTonePianoPlayerDemo
{
    public static void main(String[] args) {
        final int octaves = 7;
        final String lowestToneIpnName = "C2";
        
        final String scale = ScaleTypes.scaleName(lowestToneIpnName);
        final JFrame frame = new JFrame("NotesWithDifferenceTonePianoPlayer ("+scale+" "+octaves+" Octaves)");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        
        final PianoWithSound.Configuration config = new PianoWithSound.Configuration(
                octaves, 
                lowestToneIpnName,
                //true, // vertical: IMPOSSIBLE here due to wide tuning-choice
                13);
        final WaveSoundChannel soundChannel = new SineWaveSoundChannel(null);
        final NotesPianoPlayer player = new NotesWithDifferenceTonePianoPlayer(new DifferenceToneForNotesPiano(config, soundChannel));
        //final JComponent playerPanel = player.getPlayer(null);
        final JComponent playerPanel = player.getPlayer(NotesPianoPlayerDemo.AUGUSTIN);
        //final JComponent playerPanel = player.getPlayer(NotesPianoPlayerDemo.TUBULAR_BELLS);
        
        frame.addWindowListener(player.getWindowClosingListener());
        frame.add(playerPanel);
        frame.pack();
        frame.setLocationRelativeTo(null);
        frame.setVisible(true);
    }
}