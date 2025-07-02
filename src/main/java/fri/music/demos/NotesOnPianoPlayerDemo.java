package fri.music.demos;

import javax.swing.JComponent;
import javax.swing.JFrame;
import fri.music.ScaleTypes;
import fri.music.instrument.NotesOnPianoPlayer;
import fri.music.instrument.PianoWithSound;
import fri.music.wavegenerator.SineWaveSoundChannel;

public class NotesOnPianoPlayerDemo
{
    private static final String AUGUSTIN = 
        "3/4 \n"+
        "G4/4. A4/8 G4/8 F4/8 E4/4 C4/4 C4/4 \n"+
        "D4/4 G3/4 G3/4 E4/4 C4/4 C4/4 \n"+
        "G4/4. A4/8 G4/8 F4/8 E4/4 C4/4 C4/4 \n"+
        "D4/4 G3/4 G3/4 C4/2.";

    public static void main(String[] args) {
        final int octaves = 7;
        final String lowestToneIpnName = "C2";
        
        final String scale = ScaleTypes.scaleName(lowestToneIpnName);
        final JFrame frame = new JFrame("NotesOnPianoPlayer ("+scale+ +octaves+" Octaves)");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        
        final PianoWithSound.Configuration config = new PianoWithSound.Configuration(
                octaves, 
                lowestToneIpnName,
                //true, // vertical
                13);
        final NotesOnPianoPlayer player = new NotesOnPianoPlayer(config, new SineWaveSoundChannel(null), AUGUSTIN);
        final JComponent playerPanel = player.getPlayer();
        
        frame.addWindowListener(player.getWindowClosingListener());
        frame.add(playerPanel);
        frame.pack();
        frame.setLocationRelativeTo(null);
        frame.setVisible(true);
    }
}