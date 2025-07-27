package fri.music.demos;

import java.util.List;
import fri.music.EqualTemperament;
import fri.music.JustIntonation;
import fri.music.JustIntonation.ChromaticScales;
import fri.music.ToneSystem;
import fri.music.differencetones.DifferenceToneInversions;
import fri.music.differencetones.DifferenceToneInversions.TonePair;
import fri.music.differencetones.DifferenceTones;

/**
 * Displays difference tone inversion intervals.
 */
public class DifferenceToneInversionsDemo
{
    public static void main(String[] args) {
        final String BASE_TONE = "C2";
        final int octaves = 4; // C2 - C6
        /**
         * 4 octaves cover 1.3 octaves (C2-E3) for a a melody made of difference tones
         * where all harmonious intervals are available for it.
         * Harmonious intervals: minor and major thirds and sixths, fourth, fifth, tritone.
         */
        display(
                new EqualTemperament(BASE_TONE, octaves), 
                DifferenceTones.TOLERANT_DEVIATION_EDO_12);
        /**
         * 4 octaves cover 1.5 octaves (C2-F#3) for a melody made of difference tones
         * where many harmonious intervals (except tritone) are available for it.
         * Harmonious intervals: minor and major thirds and sixths, fourth, fifth.
         */
        display(
                new JustIntonation(BASE_TONE, octaves, ChromaticScales.LIMIT_5_SYMMETRIC_1), 
                DifferenceTones.PRECISE_DEVIATION_JI);
        display(
                new JustIntonation(BASE_TONE, octaves, ChromaticScales.LIMIT_5_SYMMETRIC_2), 
                DifferenceTones.PRECISE_DEVIATION_JI);
        display(
                new JustIntonation(BASE_TONE, octaves, ChromaticScales.LIMIT_5_ASYMMETRIC), 
                DifferenceTones.PRECISE_DEVIATION_JI);
        /**
         * 4 octaves cover 1.5 octaves (C2-F#3) for a melody made of difference tones.
         * The tones F and D# (IONIAN C-scale) are not present at all!
         */
        display(
                new JustIntonation(BASE_TONE, octaves, ChromaticScales.HARMONIC_SERIES), 
                DifferenceTones.TOLERANT_DEVIATION_JI);
    }
    
    private static void display(ToneSystem toneSystem, double deviationTolerance) {
        final int SMALLEST_SEMITONE_STEPS = ToneSystem.semitoneSteps(ToneSystem.MAJOR_SECOND);
        final int BIGGEST_SEMITONE_STEPS  = ToneSystem.semitoneSteps(ToneSystem.MAJOR_SIXTH); // the MINOR_SEVENTH difference tone is very hard to hear!

        final DifferenceToneInversions differenceToneInversions = new DifferenceToneInversions(
                new DifferenceToneInversions.Configuration(
                    toneSystem.tones(),
                    SMALLEST_SEMITONE_STEPS,
                    BIGGEST_SEMITONE_STEPS,
                    deviationTolerance)
            );
        differenceToneInversions.removeDissonant(false);
        
        System.out.println(toneSystem);
        System.out.println("Difference tone deviation tolerance = "+differenceToneInversions.deviationTolerance);
        
        differenceToneInversions.differenceTones().stream().forEach(tone -> {
            final List<TonePair> intervals = differenceToneInversions.getIntervalsGenerating(tone);
            System.out.println(
                    tone.ipnName+"\t"+
                    intervals.size()+": "+
                    intervals);
        });
    }
}
