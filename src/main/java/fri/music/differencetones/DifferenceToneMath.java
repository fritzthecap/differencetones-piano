package fri.music.differencetones;

/**
 * Difference tones are at frequencies (y - x), (2x - y) and (3x - 2y).
 * @see https://www.sfu.ca/sonic-studio-webdav/handbook/Combination_Tones.html
 */
public final class DifferenceToneMath
{
    public static double primaryDifference(double frequencySmall, double frequencyBig) {
        double[] smallerFirst = smallerFirst(frequencySmall, frequencyBig);
        return smallerFirst[1] - smallerFirst[0];
    }
    
    public static double secondaryDifference(double frequencySmall, double frequencyBig) {
        double[] smallerFirst = smallerFirst(frequencySmall, frequencyBig);
        return 2.0 * smallerFirst[0] - smallerFirst[1];
    }
    
    public static double tertiaryDifference(double frequencySmall, double frequencyBig) {
        double[] smallerFirst = smallerFirst(frequencySmall, frequencyBig);
        return 3.0 * smallerFirst[0] - 2.0 * smallerFirst[1];
    }
    
    
    private static double[] smallerFirst(double frequencySmall, double frequencyBig) {
        if (frequencySmall < 0.0 || frequencyBig < 0.0)
            throw new IllegalArgumentException("Can not process negative frequencies: "+frequencySmall+", "+frequencyBig);
        
        return (frequencySmall > frequencyBig)
                ? new double[] { frequencyBig, frequencySmall }
                : new double[] { frequencySmall, frequencyBig };
    }
    
    private DifferenceToneMath() {} // do not instantiate
}
