package fri.music.demos;

import fri.music.JustIntonation;
import fri.music.JustIntonation.ChromaticScales;
import fri.music.AbstractJustIntonation.JustTone;
import fri.music.Tone;
import fri.music.ToneSystem;
import fri.music.Tones;

public class JustIntonationDemo
{
    /**
     * Shows distances between tones given on command-line.
     * @param args tuples of notes, e.g. 
     *      A4 B4 B4 C5 C5 D5 D5 E5 E5 F5 F5 G5 G5 A5 A5 B5 B5 C6
     */
    public static void main(String[] args) {
        if (args.length > 0 && args.length % 2 == 0) { // displaying differences between tones
            final String baseTone = ToneSystem.DEFAULT_BASETONE_IPN_NAME;
            final ToneSystem toneSystem = new JustIntonation(
                    baseTone, 
                    ToneSystem.MAXIMUM_OCTAVES,
                    ChromaticScales.LIMIT_5_SYMMETRIC_1);
                    //ChromaticScale.LIMIT_5_SYMMETRIC_2);
            
            System.out.println(toneSystem);
            
            final Tones tones = new Tones(toneSystem.tones());
            
            for (int i = 0; i < args.length; i += 2) {
                final String firstTone = args[i];
                final String secondTone = args[i + 1];
                final JustTone tone1 = (JustTone) tones.forIpnName(firstTone);
                final JustTone tone2 = (JustTone) tones.forIpnName(secondTone);
                
                final long[] distance = tone1.distance(tone2);
                
                System.out.println("Distance between "+tone1.ipnName+" and "+tone2.ipnName+" = "+(distance[0]+"/"+distance[1]));
            }
        }
        else { // display a scale
            final ToneSystem toneSystem = new JustIntonation(
                    ToneSystem.DEFAULT_REFERENCE_FREQUENCY, // 440 
                    ToneSystem.DEFAULT_BASETONE_IPN_NAME, // C0
                    ToneSystem.MAXIMUM_OCTAVES); // 10
            System.out.println(toneSystem);
            System.out.println("IPN-Name (MIDI)\tFreq.\tRatio\tCent\tCent-Deviation");
            
            final Tone[] tones = toneSystem.tones();
            for (int i = 0; i < tones.length; i++)
                System.out.println(tones[i]);
        }
    }

}
