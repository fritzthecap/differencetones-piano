package fri.music.demos;

import javax.swing.JComponent;
import javax.swing.JFrame;
import fri.music.ScaleTypes;
import fri.music.instrument.Piano;

public class PianoDemo
{
    public static void main(String[] args) {
        final int octaves = 2;
        final String lowestToneIpnName = "C3";
        
        final String scale = ScaleTypes.scaleName(lowestToneIpnName);
        final JFrame frame = new JFrame(Piano.class.getSimpleName()+" ("+scale+", "+octaves+" Octaves)");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        
        final Piano.Configuration config = new Piano.Configuration(octaves, lowestToneIpnName);
        final Piano piano = new Piano(config);
        final JComponent pianoPanel = piano.getKeyboard();
        
        frame.add(pianoPanel);
        frame.pack();
        frame.setLocationRelativeTo(null);
        frame.setVisible(true);
    }
}