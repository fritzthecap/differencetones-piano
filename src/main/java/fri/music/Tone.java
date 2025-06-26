package fri.music;

import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.Objects;

/**
 * One note or tone, given by frequency and a name that
 * contains the IPN-octave (C-based). The MIDI number is
 * for easily connecting with some 12-tone keyboard device.
 * IPN = international pitch notation.
 * @see https://www.flutopedia.com/octave_notation.htm.
 */
public class Tone implements Comparable<Tone>
{
    public static final NumberFormat frequencyFormat = new DecimalFormat("0.000");
    
    /** IPN name including octave number, something like "F#4". */
    public final String ipnName;
    /** Hertz of this tone. */
    public final double frequency;
    /** The "C"-based IPN-octave this note resides in, 0-n. Increases with every next higher "C". */
    public final int ipnOctave;
    /** The number to feed into a MIDI-synthesizer to play this tone. */
    public final int midiNumber;
    /** The cent amount of the tone, counted from lowest of the scale. */
    public final int cent;
    
    protected String formattedFrequency;
    private String ipnNameWithoutOctave;
    
    public Tone(String ipnName, double frequency, int midiNumber, int cent) {
        this.ipnName = Objects.requireNonNull(ipnName);
        this.frequency = frequency;
        this.midiNumber = midiNumber;
        this.cent = cent;
        
        final String onlyDigits = ipnName.replaceAll("[^0-9\\-]", ""); // deletes all non-digits
        int octave;
        try {
            octave = Integer.valueOf(onlyDigits);
        }
        catch (NumberFormatException e) { // happens on REST_SYMBOL
            octave = 0;
        }
        this.ipnOctave = octave;
    }
    
    public String ipnNameWithoutOctave() {
        if (ipnNameWithoutOctave == null)
            ipnNameWithoutOctave = ipnName.replaceAll("[0-9\\-]", ""); // deletes all digits
        return ipnNameWithoutOctave;
    }
    
    
    /** Equality goes to IPN-name only. */
    @Override
    public boolean equals(Object other) {
        return (other instanceof Tone) ? ((Tone) other).ipnName.equals(ipnName) : false;
    }
    
    /** Hashcode goes to IPN-name only. */
    @Override
    public int hashCode() {
        return ipnName.hashCode();
    }

    @Override
    public int compareTo(Tone other) {
        return midiNumber - other.midiNumber;
    }
    
    
    /** @return the frequency formatted by <code>frequencyFormat</code>. */
    public final String formattedFrequency() {
        if (formattedFrequency == null)
            formattedFrequency = frequencyFormat.format(frequency);
        return formattedFrequency;
    }
    
    @Override
    public String toString() {
        return baseToString()+"\t"+centToString();
    }
    
    protected final String baseToString() {
        return ipnName+" ("+midiNumber+"),\t"+formattedFrequency();
    }
    protected final String centToString() {
        return cent+" ¢";
    }
}