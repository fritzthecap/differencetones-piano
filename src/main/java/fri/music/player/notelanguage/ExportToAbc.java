package fri.music.player.notelanguage;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import fri.music.TextUtil;
import fri.music.ToneSystem;
import fri.music.player.Note;

/**
 * 
 */
public class ExportToAbc
{
    private static final Map<String,String> IPN_NOTE_TO_ABC_FLAT = new HashMap<>();
    private static final Map<String,String> IPN_NOTE_TO_ABC_SHARP = new HashMap<>();
    
    static {
        buildIpnToAbcNameMapping();
    }
    
    private static void buildIpnToAbcNameMapping() {
        // IPN = ABC note-name and octave
        // C0 = C,,,, 
        // ...
        // C2 = C,,
        // C3 = C,
        // C4 = C D E F G A B 
        // C5 = c d e f g a b
        // C6 = c'
        // C7 = c''
        // ...
        // C9 = c''''
        // C10 = c'''''
        for (int octave = ToneSystem.LOWEST_OCTAVE; octave < ToneSystem.MAXIMUM_OCTAVES; octave++) {
            if (octave < 0)
                throw new IllegalArgumentException("Can not process octave < 0!");
            
            for (int i = 0; i < ToneSystem.IPN_BASE_NAMES.length; i++) {
                final String ipnName = ToneSystem.IPN_BASE_NAMES[i];
                final String ipnNameWithoutAccent = getIpnNameWithoutAccent(ipnName);
                
                final String abcOctave = getAbcOctave(octave);
                final String abcName = toAbcName(ipnNameWithoutAccent, octave);
                final String abcNameWithOctave = abcName + abcOctave;
                
                final boolean accented = (ToneSystem.IPN_BASE_NAMES[i].length() != ipnNameWithoutAccent.length());
                final String ipnNameWithOctave = ipnName + octave;
                if (accented) {
                    IPN_NOTE_TO_ABC_SHARP.put(ipnNameWithOctave, "^" + abcNameWithOctave);
                    
                    if (i < ToneSystem.IPN_BASE_NAMES.length - 1) { // not yet at "B"
                        final String ipnForFlat = getIpnNameWithoutAccent(ToneSystem.IPN_BASE_NAMES[i + 1]); // "B"
                        final String abcForFlat = toAbcName(ipnForFlat, octave);
                        final String abcNameWithOctaveFlat = "_" + abcForFlat + abcOctave;
                        IPN_NOTE_TO_ABC_FLAT.put(ipnNameWithOctave, abcNameWithOctaveFlat);
                    }
                }
                else {
                    IPN_NOTE_TO_ABC_SHARP.put(ipnNameWithOctave, abcNameWithOctave);
                    IPN_NOTE_TO_ABC_FLAT.put(ipnNameWithOctave, abcNameWithOctave);
                }
            }
        }
    }
    
    private static String getIpnNameWithoutAccent(String ipnName) {
        if (ipnName.endsWith("#"))
            return ipnName.substring(0, ipnName.length() - "#".length());
        return ipnName;
    }
    
    private static String toAbcName(String ipnNameWithoutAccent, int octave) {
        if (octave < 5)
            return ipnNameWithoutAccent; // already in upper case
        return ipnNameWithoutAccent.toLowerCase();
    }
    
    private static String getAbcOctave(int octave) {
        if (octave < 4)
            return ",".repeat(4 - octave);
        else if (octave > 5)
            return "'".repeat(octave - 5);
        return "";
    }
    
    
    /** ABC header data. */
    public record Configuration(
            String title, 
            String subTitle, 
            String keyAndClef, 
            String author, 
            String date,
            int numberOfBarsPerLine)
    {
        public Configuration() {
            this("Title", "C", "Author");
        }
        public Configuration(String title, String keyAndClef, String author) {
            this(title, null, keyAndClef, author, null, 4);
        }
        
        /** @return true when the tune's key is F, Bb, Eb, Ab, ... Db, else false. */
        public boolean isFlatKey() {
            if (keyAndClef.startsWith("F"))
                return true;
            if (keyAndClef.length() > 1 && keyAndClef.charAt(1) == 'b')
                return true;
            return false;
        }
    }
    
    
    private final Note[][] notes;
    
    /** Construct an exporter for different export-configurations. */
    public ExportToAbc(Note[][] notes) {
        Objects.requireNonNull(notes);
        if (notes.length <= 0 || notes[0].length <= 0)
            throw new IllegalArgumentException("Can not export empty notes!");
        
        if (notes[0][0].beatInfo == null || notes[0][0].beatInfo.timeSignature() == null)
            throw new IllegalArgumentException("First note is expected to carry time-signature!");
            
        this.notes = notes;
    }
    
    /**
     * Export constructor notes with given header configuration.
     * @param configuration the configuration of the ABC header.
     * @return a text with ABC notation of given notes.
     */
    public String export(Configuration configuration) {
        if (configuration == null) // get a default
            configuration = new Configuration();
        
        final StringBuilder result = new StringBuilder();
        final int numberOfBarsPerLine = Math.max(configuration.numberOfBarsPerLine(), 1);
        final boolean isFlatKey = configuration.isFlatKey();
        
        String timeSignature = writeHeader(configuration, result);
        int barCount = 0;
        boolean wasDotted = false;
        
        for (int i = 0; i < notes.length; i++) {
            final Note[] chord = notes[i];
            final Note firstNote = chord[0];
            
            final boolean barStart = (i > 0 && firstNote.emphasized);
            if (barStart) {
                result.append("|");
                barCount++;
            }
            
            final boolean gotoNextLine = (barCount == numberOfBarsPerLine);
            
            // check for bar change
            final boolean barChange = 
                    (null != firstNote.beatInfo.timeSignature() &&
                    false == firstNote.beatInfo.timeSignature().equals(timeSignature));
            
            if (barChange) {
                timeSignature = firstNote.beatInfo.timeSignature();
                
                if (barStart == false)
                    result.append("|");
                result.append(TextUtil.NEWLINE);
                result.append("M: "+timeSignature);
                if (gotoNextLine == false)
                    result.append(TextUtil.NEWLINE);
            }
                
            if (gotoNextLine) {
                result.append(TextUtil.NEWLINE);
                barCount = 0;
            }
            else if (barStart && false == barChange)
                result.append(" ");
            
            final boolean slurStart = Boolean.TRUE.equals(firstNote.connectionFlags.slurred());
            if (slurStart)
                result.append("(");
            
            final boolean moreThanOneNote = (chord.length > 1);
            if (moreThanOneNote)
                result.append("[");
            
            final Integer multipletType = isMultipletLength(firstNote.lengthNotation);
            if (multipletType != null)
                result.append("("+multipletType+" ");
            
            final boolean isDotted = isDotted(firstNote.lengthNotation);
            final String length;
            if (wasDotted)
                length = toAbcDoubleLength(firstNote.lengthNotation, isDotted);
            else
                length = toAbcLength(firstNote.lengthNotation, isDotted);
                
            wasDotted = isDotted;
            
            for (Note note : chord) { // single notes, mostly just one
                final String noteName;
                if (isFlatKey)
                    noteName = IPN_NOTE_TO_ABC_FLAT.get(note.ipnName);
                else
                    noteName = IPN_NOTE_TO_ABC_SHARP.get(note.ipnName);
                
                result.append(noteName);
                result.append("/"+length);
            }
            
            if (moreThanOneNote)
                result.append("]");
            
            final boolean tied = Boolean.TRUE.equals(firstNote.connectionFlags.tied());
            if (tied)
                result.append("-");
            
            final boolean slurEnd = Boolean.FALSE.equals(firstNote.connectionFlags.slurred());
            if (slurEnd)
                result.append(")");
            
            if (isEighthOrShorter(firstNote.lengthNotation))
                result.append('`'); // connect short notes by beam
            else
                result.append(' ');
        }
        
        result.append("||");
        return result.toString();
    }


    private String writeHeader(Configuration configuration, StringBuilder result) {
        appendLine(result, "X: 1");
        
        if (configuration.title() != null)
            appendLine(result, "T: "+configuration.title());

        if (configuration.subTitle() != null)
            appendLine(result, "T: "+configuration.subTitle());

        final String date = (configuration.date() != null)
                ? configuration.date()
                : new SimpleDateFormat("yyyy-MM-dd HH:mm").format(new Date());
        if (configuration.author() != null)
            appendLine(result, "C: "+configuration.author()+", "+date);
        else
            appendLine(result, "C: "+date);
        
        final Note firstNote = notes[0][0];
        final String timeSignature = Objects.requireNonNull(firstNote.beatInfo.timeSignature());
        appendLine(result, "M: "+timeSignature);
        
        if (firstNote.beatInfo.beatsPerMinute() != null)
            appendLine(result, "Q: 1/4="+firstNote.beatInfo.beatsPerMinute());
        
        appendLine(result, "L: 1/1");
        
        appendLine(result, "K: "+configuration.keyAndClef());
        
        return timeSignature;
    }
    
    private void appendLine(StringBuilder stringBuilder,  String toAppend) {
        stringBuilder.append(toAppend);
        stringBuilder.append(TextUtil.NEWLINE);
    }
    
    
    private Integer isMultipletLength(String lengthNotation) {
        final int multipletSeparatorIndex = lengthNotation.indexOf(MelodyFactory.MULTIPLET_SEPARATOR);
        return MelodyFactory.getMultipletType(lengthNotation, multipletSeparatorIndex);
    }
    
    private boolean isDotted(String lengthNotation) {
        return lengthNotation.endsWith(".");
    }
    
    private String toAbcLength(String lengthNotation, boolean isDotted) {
        lengthNotation = stripLengthToNumber(lengthNotation, isDotted);
        return editDottedLength(lengthNotation, isDotted);
    }
    
    private String toAbcDoubleLength(String lengthNotation, boolean isDotted) {
        lengthNotation = stripLengthToNumber(lengthNotation, isDotted);
        lengthNotation = Integer.toString(Integer.valueOf(lengthNotation) / 2);
        return editDottedLength(lengthNotation, isDotted);
    }
    
    private String editDottedLength(String lengthNotation, boolean isDotted) {
        return lengthNotation + (isDotted ? ">" : ""); // "broken rhythm" marker, also affects the following note!
    }
    
    private String stripLengthToNumber(String lengthNotation, boolean isDotted) {
        if (isDotted)
            lengthNotation = lengthNotation.substring(0, lengthNotation.length() - ".".length());
        
        final int multipletSeparatorIndex = lengthNotation.indexOf(MelodyFactory.MULTIPLET_SEPARATOR);
        if (multipletSeparatorIndex >= 0)
            lengthNotation = lengthNotation.substring(0, multipletSeparatorIndex);
        
        return lengthNotation;
    }
    
    private boolean isEighthOrShorter(String lengthNotation) {
        return lengthNotation.startsWith("8") || 
            lengthNotation.startsWith("16") || 
            lengthNotation.startsWith("32") ||
            lengthNotation.startsWith("64");
    }
}