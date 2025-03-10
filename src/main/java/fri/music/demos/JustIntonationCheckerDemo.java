package fri.music.demos;

import java.util.List;
import fri.music.JustIntonation.ChromaticScales;
import fri.music.ScaleTypes;
import fri.music.justintonation.JustIntonationChecker;
import fri.music.justintonation.JustIntonationChecker.Configuration;
import fri.music.justintonation.JustIntonationChecker.Result;

public class JustIntonationCheckerDemo
{
    public static void main(String[] args) {
        final Boolean showScalesOnly = null;
        final Boolean showUnjustOnly = true;
        final Boolean checkAgainst5LimitIntervals = null;
        final Boolean alsoCheckMajorSecondAndMinorSeventh = null;
        final List<String> checkedScaleNames = List.of(ScaleTypes.IONIAN, ScaleTypes.AEOLIAN);
        final Boolean checkWhiteKeyScalesOnly = null;
        final Boolean showTriadsOnly = null;
        final Boolean considerDifferenceTones = null;
        final Boolean checkChromaticScaleDifferenceTones = null;
        
        final Configuration configuration = new Configuration(
                showScalesOnly,
                showUnjustOnly,
                checkAgainst5LimitIntervals,
                alsoCheckMajorSecondAndMinorSeventh,
                checkedScaleNames,
                checkWhiteKeyScalesOnly,
                showTriadsOnly,
                considerDifferenceTones,
                checkChromaticScaleDifferenceTones);
        
        Result result;
        
        result = new JustIntonationChecker(configuration).check(
                ChromaticScales.LIMIT_5_SYMMETRIC_1);
        System.out.println(result);
        result = new JustIntonationChecker(configuration).check(
                ChromaticScales.LIMIT_5_SYMMETRIC_2);
        System.out.println(result);
        result = new JustIntonationChecker(configuration).check(
                ChromaticScales.LIMIT_5_ASYMMETRIC);
        System.out.println(result);
        
        result = new JustIntonationChecker(configuration).check(
                ChromaticScales.HARMONIC_SERIES);
        System.out.println(result);
        
//        result = new JustIntonationChecker(configuration).check(
//                ChromaticScale.PYTHAGOREAN);
//        System.out.println(result);
        
//      result = new JustIntonationChecker(configuration).check(
//              ChromaticScale.LIMIT_7);
//      System.out.println(result);
//      result = new JustIntonationChecker(configuration).check(
//              ChromaticScale.LIMIT_17);
//      System.out.println(result);
    }
}