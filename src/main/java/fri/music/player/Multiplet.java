package fri.music.player;

/**
 * Duplets, triplets, quadruplets, ... and their duration adjustment.
 */
public enum Multiplet
{
    /** 2 notes in the time of 3, occurs only in meters like 6/8 or 12/8. */
    DUPLET(2, 3, Integer.valueOf(2)),
    /** 3 notes in the time of 2. */
    TRIPLET(3, 2, Integer.valueOf(3)),
    /** 4 notes in the time of 3, occurs only in meters like 6/8 or 12/8. */
    QUADRUPLET(4, 3, Integer.valueOf(4)),
    /** 5 notes in the time of 4. */
    QUINTUPLET(5, 4, Integer.valueOf(5)),
    /** 6 notes in the time of 4. */
    SEXTUPLET(6, 4, Integer.valueOf(6));
    
    /**
     * @param multipletType 2, 3, 4, 5, 6, the type of multiplet occurring in notes.
     * @return the enum describing given multiplet type.
     */
    public static Multiplet toEnum(Integer multipletType) {
        for (Multiplet m : Multiplet.values())
            if (m.multipletType == multipletType)
                return m;
        throw new IllegalArgumentException("Unknown multiplet type: "+multipletType);
    }
    
    public final Integer multipletType;
    private final double factor;
    
    /**
     * @param divisor for a triplet, this would be 3
     * @param dividend for a triplet, this would be 2
     * @param multipletType
     */
    private Multiplet(int divisor, int dividend, Integer multipletType) {
        this.factor = (double) dividend / (double) divisor;
        this.multipletType = multipletType;
    }
    
    /** 
     * Adjusts a duration when a note is a multiplet.
     * @param duration the non-multiplet duration that must be adjusted.
     * @return in array[0] the corrected duration according to the type of this multiplet,
     *      in array[1] the overall length (sum) of all notes inside the multiplet, 
     *      assuming subsequent notes will have the same length; in other words,
     *      the leading note determines the multiplet's overall duration.
     */
    public double[] adjust(double duration) {
        final double multipletDuration = duration * factor;
        return new double[] {
                multipletDuration,
                multipletDuration * multipletType.doubleValue()
            };
    }
}