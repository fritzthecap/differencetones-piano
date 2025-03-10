package fri.music.demos;

import fri.music.EqualTemperament;
import fri.music.player.Note;
import fri.music.player.Player;
import fri.music.wavegenerator.SineWaveSoundChannel;

public class PlayerDemo
{
    /** 
     * @param args optional IPN-names of tones to play, something like "C4".
     */
    public static void main(String[] args) {
        final Player player = new Player(new SineWaveSoundChannel(new EqualTemperament().tones()));
        if (args.length <= 0) { // no arguments were given on command line
            final Note[][] chords = new Note[][] {
                new Note[] { new Note("G4", 600), new Note("E4", 600) },
                new Note[] { new Note("A4", 200) },
                new Note[] { new Note("G4", 200) },
                new Note[] { new Note("F4", 200) },
                new Note[] { new Note("E4", 400), new Note("C4", 400) },
                new Note[] { new Note("C4", 400) },
                new Note[] { new Note("C4", 400), new Note("G3", 400) },
                new Note[] { new Note("D4", 400), new Note("G3", 400) },
                new Note[] { new Note("G3", 400) },
                new Note[] { new Note("G3", 400), new Note("D3", 400) },
                new Note[] { new Note("E4", 400), new Note("C4", 400) },
                new Note[] { new Note("C4", 400) },
                new Note[] { new Note("C4", 400), new Note("G3", 400) },
            };
            player.playInRow(chords);
        }
        else {
            for (String ipnName : args) {
                final Note note = new Note(ipnName, 500);
                player.play(note);
            }
        }
    }

}
