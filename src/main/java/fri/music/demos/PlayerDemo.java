package fri.music.demos;

import fri.music.EqualTemperament;
import fri.music.Tone;
import fri.music.ToneSystem;
import fri.music.Tones;
import fri.music.player.Note;
import fri.music.player.Player;
import fri.music.wavegenerator.SineWaveSoundChannel;

public class PlayerDemo
{
    /** 
     * @param args optional IPN-names of tones to play, something like "C4".
     */
    public static void main(String[] args) {
        final ToneSystem toneSystem = new EqualTemperament();
        final Tone[] tonesArray = toneSystem.tones();
        final Tones tones = new Tones(tonesArray);
        final Player player = new Player(new SineWaveSoundChannel(tonesArray));
        if (args.length <= 0) { // no arguments were given on command line
            final Note[][] chords = new Note[][] {
                new Note[] { new Note(tones, "G4", 600), new Note(tones, "E4", 600) },
                new Note[] { new Note(tones, "A4", 200) },
                new Note[] { new Note(tones, "G4", 200) },
                new Note[] { new Note(tones, "F4", 200) },
                new Note[] { new Note(tones, "E4", 400), new Note(tones, "C4", 400) },
                new Note[] { new Note(tones, "C4", 400) },
                new Note[] { new Note(tones, "C4", 400), new Note(tones, "G3", 400) },
                new Note[] { new Note(tones, "D4", 400), new Note(tones, "G3", 400) },
                new Note[] { new Note(tones, "G3", 400) },
                new Note[] { new Note(tones, "G3", 400), new Note(tones, "D3", 400) },
                new Note[] { new Note(tones, "E4", 400), new Note(tones, "C4", 400) },
                new Note[] { new Note(tones, "C4", 400) },
                new Note[] { new Note(tones, "C4", 400), new Note(tones, "G3", 400) },
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
