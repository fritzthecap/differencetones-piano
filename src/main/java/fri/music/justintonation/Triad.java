package fri.music.justintonation;

/**
 * Major, minor and diminished triad chords,
 * including all their inversions,
 * described by their 1-n <b>diatonic</b> step-numbers.
 * For example, the TONIC of a D-minor chord would be
 * D-F-A-D-F, which is 1-3-5-8-10.
 */
public enum Triad
{
    /** Example: C, E, G, C', E' */
    TONIC      (1, 3,  5,  8, 10),
    /** Example: D, F, A, D', F' */
    SUPERTONIC (2, 4,  6,  9, 11),
    /** Example: E, G, B, E', G' */
    MEDIANT    (3, 5,  7, 10, 12),
    /** Example: F, A, C, F', A' */
    SUBDOMINANT(4, 6,  8, 11, 13),
    /** Example: G, B, D, G', B' */
    DOMINANT   (5, 7,  9, 12, 14),
    /** Example: A, C, E, A', C' */
    SUBMEDIANT (6, 8, 10, 13, 15),
    /** Example: B, D, F, B', D' */
    LEADING    (7, 9, 11, 14, 16),
    ;

    /**
     * Gives the indexes within the five <code>Triad</code> step-numbers (1-n)
     * that describe all contained different intervals, e.g. 
     * <code>0: C-E (major third), 1: C-G (fifth), 2: E-G (minor third), 
     * 3: E-C (minor sixth), 4: G-C (fourth), 5: G-E (major sixth)</code>.
     */
    public static final int[][] intervalIndexTuples = new int[][] {
        { 0, 1 }, // C-E
        { 0, 2 }, // C-G
        { 1, 2 }, // E-G
        { 1, 3 }, // E-C
        { 2, 3 }, // G-C
        { 2, 4 }, // G-E
    };

    public final int[] diatonicTones1ToN;
    
    private Triad(int... diatonicTones1ToN) {
        this.diatonicTones1ToN = diatonicTones1ToN;
    }
}