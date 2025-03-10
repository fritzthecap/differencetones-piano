package fri.music.demos;

import fri.music.EqualTemperament;
import fri.music.Tone;
import fri.music.ToneSystem;

public class EqualTemperamentDemo
{
    /** Demo main. */
    public static void main(String[] args) {
        final ToneSystem toneSystem = new EqualTemperament();
        System.out.println(toneSystem);
        
        final Tone[] tones = toneSystem.tones();
        for (int i = 0; i < tones.length; i++)
            System.out.println(tones[i]);
    }

}
