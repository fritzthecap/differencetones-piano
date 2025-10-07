package fri.music;

/**
 * Tones of "equal temperament" tuning (today's standard), 
 * where one octave is divided into 12 equally sized semi-tone steps.
 * This class represents only one tuning.
 * <pre>
 *   frequency = FREQUENCY_A4 * Math.pow(2, semitoneOffsetFromA4 / 12.0);
 * </pre>
 * Tone range of 9 octaves:
 * <pre>
    Subcontra Octave, starts at C0 = MIDI 12
    Contra Octave, C1 = MIDI 24
    Big Octave, C2 = MIDI 36
    Small Octave, C3 = MIDI 48
    ' Octave, C4 = MIDI 60, the tone one below violin-clef note lines
    '' Octave, C5 = MIDI 72, the tone within violin-clef note lines
    ''' Octave, C6 = MIDI 84, the tone two lines above violin-clef note lines
    '''' Octave, C7 = MIDI 96
    ''''' Octave, C8 = MIDI 108 ... B8 = MIDI 119
 * </pre>
 */
public final class EqualTemperament extends AbstractToneSystem
{
    public static final String NAME_POSTFIX = "(12-ET)";
    
    public EqualTemperament() {
        this(-1.0);
    }
    public EqualTemperament(String baseToneIpnName) {
        this(baseToneIpnName, -1);
    }
    public EqualTemperament(int octaves) {
        this(null, octaves);
    }
    public EqualTemperament(double frequencyOfA4) {
        this(frequencyOfA4, null, -1);
    }
    public EqualTemperament(double frequencyOfA4, String baseToneIpnName) {
        this(frequencyOfA4, baseToneIpnName, -1);
    }
    public EqualTemperament(String baseToneIpnName, int octaves) {
        this(-1.0, baseToneIpnName, octaves);
    }
    /**
     * @param frequencyOfA4 the desired frequency of tone "A4".
     * @param baseToneIpnName the lowest note of the requested tone array e.g. "E3" or "G1".
     * @param octaves the 0-n number of octaves + 1 to return.
     *      When zero, just the lowest tone is returned,
     *      when less than zero, 9 octaves will be returned.
     */
    public EqualTemperament(double frequencyOfA4, String baseToneIpnName, int octaves) {
        super(frequencyOfA4, baseToneIpnName, baseToneIpnName, octaves);
    }
    
    /** Appends other popular names to super-imlementation. */
    @Override
    public String name() {
        return super.name()+" "+NAME_POSTFIX;
    }
    
    
    @Override
    protected Object getCacheKey() {
        return referenceFrequency();
    }

    /** Always builds 12 octaves + 1 from C0. Caller will cut out what it needs. */
    @Override
    protected Tone[] createTones() {
        final int semitonesPerOctave = ToneSystem.SEMITONES_PER_OCTAVE;
        final int semitonesOffsetC0FromA4 = -(4 * semitonesPerOctave + 9); // -57, 9 is major-sixth
        final Tone[] tones = new Tone[ToneSystem.MAXIMUM_OCTAVES * semitonesPerOctave + 1];
        final double frequencyOfA4 = referenceFrequency();
        
        for (int semitone = 0, cent = 0; semitone < tones.length; semitone++, cent += 100) {
            final int octave = semitone / semitonesPerOctave;
            final String ipnNoteName = ToneSystem.IPN_BASE_NAMES[semitone % semitonesPerOctave]+octave;
            final int semitoneOffsetFromA4 = semitonesOffsetC0FromA4 + semitone;
            final int midiNoteNumber = ToneSystem.DEFAULT_BASETONE_MIDI_NUMBER + semitone;
            
            // frequency is 12th root of 2 power semitoneOffsetFromA4
            final double ratio = Math.pow(2, semitoneOffsetFromA4 / 12.0);
            final double frequency = frequencyOfA4 * ratio;
            tones[semitone] = new Tone(ipnNoteName, frequency, midiNoteNumber, cent);
            
            if (frequency < ToneSystem.MINIMUM_FREQUENCY || frequency > ToneSystem.MAXIMUM_FREQUENCY) // E10 is last with 21096.16, F10 with 22350.61 is above
                throw new IllegalArgumentException("Tone out of audible range: "+tones[semitone]);
        }
        return tones;
    }
}