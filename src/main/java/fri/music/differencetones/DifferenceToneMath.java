package fri.music.differencetones;

/**
 * Difference tones are at frequencies (y - x), (2x - y) and (3x - 2y).
 * @see https://www.sfu.ca/sonic-studio-webdav/handbook/Combination_Tones.html
 */
public final class DifferenceToneMath
{
    public static double primaryDifference(double frequencySmall, double frequencyBig) {
        if (frequencySmall > frequencyBig) {
            final double swap = frequencySmall;
            frequencySmall = frequencyBig;
            frequencyBig = swap;
        }
        return frequencyBig - frequencySmall;
    }
    
    public static double secondaryDifference(double frequencySmall, double frequencyBig) {
        if (frequencySmall > frequencyBig) {
            final double swap = frequencySmall;
            frequencySmall = frequencyBig;
            frequencyBig = swap;
        }
        return 2.0 * frequencySmall - frequencyBig;
    }
    
    public static double tertiaryDifference(double frequencySmall, double frequencyBig) {
        if (frequencySmall > frequencyBig) {
            final double swap = frequencySmall;
            frequencySmall = frequencyBig;
            frequencyBig = swap;
        }
        return 3.0 * frequencySmall - 2.0 * frequencyBig;
    }
    
    private DifferenceToneMath() {} // do not instantiate
}
