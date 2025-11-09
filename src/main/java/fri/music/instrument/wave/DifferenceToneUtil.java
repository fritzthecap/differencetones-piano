package fri.music.instrument.wave;

import java.util.Arrays;
import java.util.List;
import fri.music.Tone;
import fri.music.differencetones.DifferenceTones;
import fri.music.instrument.PianoWithSound;

/**
 * Parameterized algorithms that can not be shared through inheritance.
 */
public class DifferenceToneUtil
{
    /**
     * Returns the piano-key of the difference-tone for given MIDI numbers, or null when out of range.
     * @param tones the tuning-tones from which to derive the difference tone.
     * @param deviation allowed deviation when calculating difference.
     * @param piano the piano holding keys to select the difference-tone key from.
     * @param midiNoteNumber1 the generating interval's first tone.
     * @param midiNoteNumber2 the generating interval's second tone.
     * @return the piano-key of the difference-tone for given MIDI numbers, or null when out of range.
     */
    public static PianoWithSound.Keyboard.Key getDifferenceToneKey(
            Tone[] tones, 
            double deviation,
            PianoWithSound piano,
            int midiNoteNumber1,
            int midiNoteNumber2)
    {
        final DifferenceTones differenceTones = new DifferenceTones(tones, deviation, true); // true: find primary difference tone only
        try {
            final Tone[] allDifferenceTones = differenceTones.findDifferenceTones(midiNoteNumber1, midiNoteNumber2);
            
            if (allDifferenceTones[0] != null) { // difference-tone is in tones range
                final Tone primaryDifferenceTone = allDifferenceTones[0];

                final int index = piano.findKeyboardIndex(primaryDifferenceTone.midiNumber);
                final List<PianoWithSound.Keyboard.Key> keys = piano.getKeys();
                if (index >= 0 && index < keys.size())
                    return keys.get(index);
            }
        }
        catch (IllegalArgumentException e) { // comes from findDifferenceTones when interval too big
            System.err.println(e.toString());
        }
        return null;
    }

    /**
     * When searching difference-tone intervals for a tone, this is done through <code>Tones.getEnclosingTones()</code>.
     * The lowest tone though would not let find all possible intervals, because there is no enclosing tone below it.
     * For that reason it is recommended to search in a tone-stock that starts one below given lowest tone. 
     * @param toneStock the tones array containing hopefully one below given lowest tone.
     * @param lowest the tone to search the tone one below it.
     * @return null when no lower tone in tone-stock, else the tone one below given lowest tone.
     */
    public static Tone oneToneBelow(Tone[] toneStock, Tone lowest) {
        // we need the tone below lowest melody note to make deviation work also for bottom
        int lowestToneIndex = Arrays.binarySearch(toneStock, lowest, (t1, t2) -> t1.midiNumber - t2.midiNumber);
        lowestToneIndex--; // go one deeper
        return (lowestToneIndex >= 0) ? toneStock[lowestToneIndex] : null;
    }
}