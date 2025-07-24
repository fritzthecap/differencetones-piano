package fri.music.demos;

import javax.swing.JComponent;
import javax.swing.JFrame;
import fri.music.ScaleTypes;
import fri.music.SoundChannel;
import fri.music.instrument.PianoWithSound;
import fri.music.instrument.PianoWithVolume;
import fri.music.instrument.notespiano.NotesPianoPlayer;
import fri.music.wavegenerator.SineWaveSoundChannel;

public class NotesPianoPlayerDemo
{
    static final String AUGUSTIN = 
        """
3/4
G4/4. A4/8 G4/8 F4/8 E4/4 C4/4 C4/4
D4/4 G3/4 G3/4 E4/4 C4/4 C4/4
G4/4. A4/8 G4/8 F4/8 E4/4 C4/4 C4/4
D4/4 G3/4 G3/4 C4/2.
""";
    
    static final String ENTLEIN = 
            """
C4/8 D4/8 E4/8 F4/8 G4/4 G4/4
A4/8 A4/8 A4/8 A4/8 G4/2
A4/8 A4/8 A4/8 A4/8 G4/2
F4/8 F4/8 F4/8 F4/8 E4/4 E4/4
D4/8 D4/8 D4/8 D4/8 C4/2
""";
    
    static final String HAENSEL_GRETEL = 
            """
G4/4 E4/8 F4/8 G4/4 E4/8 C4/8
D4/8 D4/8 D4/8 E4/8 C4/2
G4/4 E4/8 F4/8 G4/4 E4/8 C4/8
D4/8 D4/8 D4/8 E4/8 C4/4. C4/8
D4/8 D4/8 D4/8 E4/8 F4/4 D4/8 D4/8
E4/8 D4/8 E4/8 F4/8 G4/2
G4/4 E4/8 F4/8 G4/4 E4/8 C4/8
D4/8 D4/8 D4/8 E4/8 C4/2
""";

    static final String TUBULAR_BELLS = 
        """
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
        final int octaves = 7;
        final String lowestToneIpnName = "C2";
        
        final String scale = ScaleTypes.scaleName(lowestToneIpnName);
        final JFrame frame = new JFrame("NotesPianoPlayer ("+scale+" "+octaves+" Octaves)");
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
        frame.add(playerPanel);
        frame.pack();
        frame.setLocationRelativeTo(null);
        frame.setVisible(true);
    }
}