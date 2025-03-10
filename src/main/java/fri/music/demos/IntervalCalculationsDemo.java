package fri.music.demos;

import java.text.DecimalFormat;
import java.text.NumberFormat;

public class IntervalCalculationsDemo
{
    public static void main(String[] args) {
        System.out.println("======= Pythagorean Comma");
        NumberFormat format = new DecimalFormat("0.0000");
        // 12 fifths are 7 octaves
        // C - G - D - A - E - H - F# - C# - Ab - Eb - Bb - F - C
        // C - C - C - C - C- C - C - C
        double twelveFifths = Math.pow(3.0 / 2.0, 12.0); // fifth = 3/2
        double sevenOctaves = Math.pow(2.0,        7.0); // octave = 2/1
        System.out.println("12 fifths = "+format.format(twelveFifths)+", 7 oktaves = "+format.format(sevenOctaves)+", difference (comma): "+format.format(twelveFifths / sevenOctaves));
        double sevenOctavesCent = 1200.0 * 7;
        double log2ofTwelveFifths = 12.0 * Math.log(3.0 / 2.0) / Math.log(2.0);
        double twelveFifthsCent = 1200.0 * log2ofTwelveFifths;
        System.out.println("In cent: "+format.format(twelveFifthsCent - sevenOctavesCent)+" (1 semitone = 100 cent)");
        
        System.out.println("======= Diesis");
        // Diësis: 3 major thirds are 1 octave
        // C - E - G# - C
        // C - C
        double threeThirds = Math.pow(5.0 / 4.0, 3.0); // major third: 5/4
        double oneOctave =   Math.pow(2.0,       1.0);
        double threeThirdsCent = 1200.0 * 3.0 * Math.log(5.0 / 4.0) / Math.log(2.0);
        double oneOctaveCent = 1200.0;
        double diesisCent = 1200.0 * Math.log(128.0 / 125.0) / Math.log(2.0);
        System.out.println("3 major thirds = "+format.format(threeThirds)+", 1 octave = "+oneOctave+", difference (diesis): "+format.format(oneOctave - threeThirds)+", in cent: "+format.format(oneOctaveCent - threeThirdsCent)+", or 128/125 = "+format.format(diesisCent)+" cent");
        
        System.out.println("======= Interval Addition");
        // major third + minor third = 1 fifth
        // C - E - G
        // C - G
        double majorThirdAndMinorThird = (5.0 / 4.0) * (6.0 / 5.0); // major third: 5/4, minor third: 6/5
        double oneFifth = 3.0 / 2.0;
        System.out.println("Major third 5/4 plus minor third 6/5 = "+majorThirdAndMinorThird+", fifth 3/2 = "+oneFifth);
        
        System.out.println("======= Cent from Ratio Calculation");
        // 1 octave = 1200 cent
        // cent = 1200 * log2(ratio)
        double fifthRatio = 3.0 / 2.0;
        double fifthCent = 1200.0 * Math.log(fifthRatio) / Math.log(2.0);
        System.out.println("Cent for fifth ("+fifthRatio+"): "+format.format(fifthCent));
        
        System.out.println("======= Ratio from Cent Calculation");
        // ratio = 2 pow cents/1200
        double ratio = Math.pow(2, fifthCent / 1200.0);
        System.out.println("Ratio for fifth's ("+format.format(fifthCent)+" cent) = "+ratio);
        
        System.out.println("======= Syntonic Comma");
        double syntonicComma = (9.0 / 8.0) / (10.0 / 9.0); // = 81/80
        double syntonicCent = 1200.0 * Math.log(syntonicComma) / Math.log(2.0);
        System.out.println("Syntonic comma (big major second 9/8 - small major second 10/9): "+format.format(syntonicComma)+", in cent: "+format.format(syntonicCent));
        
        System.out.println("======= Adding different Seconds");
        // 12 minor seconds are 1 oktave
        // C - C# - D - D# - E - F - F# - G - G# - A - A# - H - C
        // C - C
        double twelveSmallSeconds;
        twelveSmallSeconds = Math.pow( 16.0 / 15.0,  12.0); // diatonische kleine Sekund: 16/15
        System.out.println("Twelve 16/15 = "+format.format(twelveSmallSeconds)+", difference to octave: "+format.format(oneOctave - twelveSmallSeconds));
        twelveSmallSeconds = Math.pow(135.0 / 128.0, 12.0); // chromatische kleine Sekund: 135/128
        System.out.println("Twelve 135/128 = "+format.format(twelveSmallSeconds)+", difference to octave: "+format.format(oneOctave - twelveSmallSeconds));
        twelveSmallSeconds = Math.pow( 25.0 / 24.0,  12.0); // chromatische kleine Sekund: 25/24
        System.out.println("Twelve 25/24 = "+format.format(twelveSmallSeconds)+", difference to octave: "+format.format(oneOctave - twelveSmallSeconds));
        twelveSmallSeconds = Math.pow(Math.pow(2, 1.0 / 12.0),  12.0); // gleichstufige kleine Sekund
        System.out.println("Twelve equal-tempered seconds = "+format.format(twelveSmallSeconds)+", difference to octave: "+format.format(oneOctave - twelveSmallSeconds));
    }

}
