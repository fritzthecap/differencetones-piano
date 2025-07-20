package fri.music.demos;

import javax.swing.JComponent;
import javax.swing.JFrame;
import fri.music.ScaleTypes;
import fri.music.instrument.PianoWithSound;
import fri.music.instrument.notespiano.NotesDifferenceTonesPiano;
import fri.music.instrument.notespiano.NotesPiano;
import fri.music.instrument.wave.DifferenceTonePiano;
import fri.music.wavegenerator.SineWaveSoundChannel;
import fri.music.wavegenerator.WaveSoundChannel;

public class NotesDifferenceTonesPianoDemo
{
    public static void main(String[] args) {
        final int octaves = 7;
        final String lowestToneIpnName = "C2";
        
        final String scale = ScaleTypes.scaleName(lowestToneIpnName);
        final JFrame frame = new JFrame("NotesDifferenceTonesPiano ("+scale+" "+octaves+" Octaves)");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        
        final PianoWithSound.Configuration config = new PianoWithSound.Configuration(
                octaves, 
                lowestToneIpnName,
                //true, // vertical
                13);
        final WaveSoundChannel soundChannel = new SineWaveSoundChannel(null);
        final NotesPiano player = new NotesDifferenceTonesPiano(new DifferenceTonePiano(config, soundChannel));
        final JComponent playerPanel = player.getPlayer(NotesPianoDemo.AUGUSTIN);
        //final JComponent playerPanel = player.getPlayer(NotesPianoDemo.TUBULAR_BELLS);
        
        frame.addWindowListener(player.getWindowClosingListener());
        frame.add(playerPanel);
        frame.pack();
        frame.setLocationRelativeTo(null);
        frame.setVisible(true);
    }
}