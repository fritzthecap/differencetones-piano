package fri.music.player.notelanguage.abc;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Objects;
import fri.music.TextUtil;
import fri.music.player.Note;
import fri.music.player.notelanguage.MelodyFactory;

/**
 * Exports MelodyFactory notation to ABC notation.
 * Result can be tried out on
 * <ul>
 *  <li>https://www.abcjs.net/abcjs-editor</li>
 *  <li>https://michaeleskin.com/abctools/abctools.html</li>
 *  <li>https://notabc.app/abc-converter/</li>
 *  <li>https://editor.drawthedots.com/</li>
 *  <li>https://abc.hieuthi.com/</li>
 *  <li>https://abc.rectanglered.com/</li>
 *  <li>https://www.abctransposer.de/</li>
 *  <li>https://www.maztr.com/sheetmusiceditor</li>
 *  <li>https://spuds.thursdaycontra.com/SPUDSConverter.html</li>
 * </ul>
 * @see https://abcnotation.com/wiki/abc:standard:v2.1
 */
public class AbcExport
{
    /** ABC header data. */
    public record Configuration(
            Integer songNumber,
            String title, 
            String subTitle, 
            String author, 
            String date,
            String keyAndClef, 
            int numberOfBarsPerLine)
    {
        public Configuration() {
            this(null, null, "C");
        }
        public Configuration(String keyAndClef) {
            this(null, null, keyAndClef);
        }
        public Configuration(String title, String author, String keyAndClef) {
            this(1, title, null, author, null, keyAndClef, 4);
        }
        
        /** @return true when the tune's key is F, Bb, Eb, Ab, ... Db, else false. */
        public boolean isFlatKey() {
            if (keyAndClef == null)
                return false;
            if (keyAndClef.startsWith("F"))
                return true;
            if (keyAndClef.length() > 1 && keyAndClef.charAt(1) == 'b')
                return true;
            return false;
        }
        
        /** @return the upper-case key without optional clef. */
        public String getKey() {
            final String trimmed = (keyAndClef != null) ? keyAndClef.trim() : "";
            if (trimmed.length() <= 0)
                return "C";
            
            final int spaceIndex = trimmed.indexOf(' ');
            final String key = (spaceIndex > 0) ? trimmed.substring(0, spaceIndex) : trimmed;
            return key.toUpperCase();
        }
    }
    
    
    private static final String ABC_DOT = ">"; // "broken rhythm" marker, also affects the following note!
    private static final String ABC_CHORD_OPEN = "[";
    private static final String ABC_CHORD_CLOSE = "]";
    private static final String ABC_SLUR_OPEN = "(";
    private static final String ABC_SLUR_CLOSE = ")";
    private static final String ABC_TIE = "-";
    private static final String ABC_BAR = "|";
    
    
    private final Note[][] notes;
    private final DateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm");
    
    private AbcKeyToAccidentalsMap keyToAccidentalsMap;
    
    /** Constructor of an exporter for different export-configurations from text. */
    public AbcExport(String notes, MelodyFactory melodyFactory) {
        this(melodyFactory.translate(notes));
    }
    
    /** Constructor of an exporter for different export-configurations from parsed notes. */
    public AbcExport(Note[][] notes) {
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
    public synchronized String export(Configuration configuration) {
        if (configuration == null) // get a default
            configuration = new Configuration();
        
        this.keyToAccidentalsMap = new AbcKeyToAccidentalsMap(configuration);
        
        final StringBuilder result = new StringBuilder();
        final int numberOfBarsPerLine = Math.max(configuration.numberOfBarsPerLine(), 1);
        
        String timeSignature = writeHeader(configuration, result);
        int barCount = 0;
        boolean wasDotted = false;
        boolean inSlur = false;
        boolean inMultiplet = false;
        
        for (int i = 0; i < notes.length; i++) {
            final Note[] chord = notes[i];
            final Note firstNote = chord[0];
            final Note lastNote = chord[chord.length - 1];
            final Note nextFirstNote = (i < notes.length - 1) ? notes[i + 1][0] : null;
            
            final boolean barStart = (i > 0 && firstNote.emphasized);
            if (barStart) {
                result.append(ABC_BAR);
                barCount++;
                keyToAccidentalsMap.barStart(); // no more need to resolve accents in a new bar
            }
            
            final boolean gotoNextLine = (barCount == numberOfBarsPerLine);
            
            // check for time-signature change
            final boolean meterChange = isMeterChange(timeSignature, firstNote);
            if (meterChange)
                timeSignature = meterChange(result, timeSignature, firstNote, barStart, gotoNextLine);
                
            if (gotoNextLine) {
                result.append(TextUtil.NEWLINE);
                barCount = 0;
            }
            else if (barStart && false == meterChange)
                result.append(" ");
            
            inSlur = detectSlurStart(result, inSlur, firstNote);
            
            final boolean moreThanOneNote = (chord.length > 1);
            if (moreThanOneNote)
                result.append(ABC_CHORD_OPEN);
            
            if (firstNote.connectionFlags.multiplet() == Boolean.TRUE) { // triplet or other
                if (inMultiplet == false) { // multiplet start
                    result.append("("+firstNote.connectionFlags.multipletType()+" ");
                    inMultiplet = true;
                }
            }
            else if (firstNote.connectionFlags.multiplet() == Boolean.FALSE) {
                inMultiplet = false;
            }
            
            final boolean isDotted = isDotted(firstNote.lengthNotation);
            // do not yet append ">" when more than one note is in chord
            final boolean makeDotted = moreThanOneNote ? false : isDotted;
            final String length = getAbcDottedLength(wasDotted, firstNote, makeDotted);
            wasDotted = isDotted;
            
            writeChordNotes(result, chord, length); // chord notes write loop
            
            if (moreThanOneNote) {
                result.append(ABC_CHORD_CLOSE);
                if (isDotted) // the ">" was not appended, do it now after all chord notes
                    result.append(ABC_DOT);
            }
            
            if (Boolean.TRUE.equals(lastNote.connectionFlags.tied()))
                result.append(ABC_TIE);
            
            inSlur = detectSlurEnd(result, inSlur, lastNote);
            
            finish(result, lastNote, moreThanOneNote, nextFirstNote);
        }
        
        result.append("||");
        return result.toString();
    }
    
    private String writeHeader(Configuration configuration, StringBuilder result) {
        appendLine(result, "X: "+(configuration.songNumber() != null ? configuration.songNumber() : 1));
        
        if (nonEmpty(configuration.title()))
            appendLine(result, "T: "+configuration.title());

        if (nonEmpty(configuration.subTitle()))
            appendLine(result, "T: "+configuration.subTitle());

        final String date = nonEmpty(configuration.date())
                ? configuration.date().replace("{date}", dateFormat.format(new Date()))
                : "";
        if (nonEmpty(configuration.author()))
            if (nonEmpty(date))
                appendLine(result, "C: "+configuration.author()+", "+date);
            else
                appendLine(result, "C: "+configuration.author());
        else if (nonEmpty(date))
            appendLine(result, "C: "+date);
        
        final Note firstNote = notes[0][0];
        final String timeSignature = Objects.requireNonNull(firstNote.beatInfo.timeSignature());
        appendLine(result, "M: "+timeSignature);
        
        if (firstNote.beatInfo.beatsPerMinute() != null)
            appendLine(result, "Q: 1/4="+firstNote.beatInfo.beatsPerMinute());
        
        appendLine(result, "L: 1/1"); // always keep duration on note, not in "L" field!
        
        if (nonEmpty(configuration.keyAndClef()))
            appendLine(result, "K: "+configuration.keyAndClef());
        
        return timeSignature;
    }
    
    private void writeChordNotes(StringBuilder result, Note[] chord, String length) {
        for (final Note note : chord) { // single notes, mostly just one
            final String adjustedAbcNoteName = keyToAccidentalsMap.getAdjustedNote(note.ipnName);
            
            result.append(adjustedAbcNoteName);
            result.append("/"+length);
            
            if (isEighthOrShorter(length) == false && note != chord[chord.length - 1])
                result.append(" ");
        }
    }
    
    private void finish(StringBuilder result, Note firstNote, boolean moreThanOneNoteInChord, Note nextFirstNote) {
        final boolean isEighthOrShorter = isEighthOrShorter(firstNote.lengthNotation);
        final boolean nextIsEighthOrShorter = (nextFirstNote != null && isEighthOrShorter(nextFirstNote.lengthNotation));
        final boolean barEnd = (nextFirstNote == null || nextFirstNote.emphasized);
        final boolean endOfMultiplet = 
                (firstNote.connectionFlags.multiplet() == Boolean.FALSE);
        final boolean beforeMultiplet = 
                (firstNote.connectionFlags.multiplet() == null &&
                 nextFirstNote != null && nextFirstNote.connectionFlags.multiplet() == Boolean.TRUE);
        final boolean isRest = firstNote.isRest() || (nextFirstNote != null && nextFirstNote.isRest());
        
        if (isEighthOrShorter && 
                nextIsEighthOrShorter && 
                barEnd == false && 
                endOfMultiplet == false &&
                beforeMultiplet == false &&
                isRest == false)
        {
            if (moreThanOneNoteInChord == false) // no ` inside a chord ...
                result.append('`');
            // ... else create beam by leaving out space
        }
        else { // longer than eighth, separate by space for readability
            result.append(' ');
        }
    }

    // helper methods
    
    private boolean nonEmpty(String s) {
        return s != null && s.trim().length() > 0;
    }
    
    private void appendLine(StringBuilder stringBuilder,  String toAppend) {
        stringBuilder.append(toAppend);
        stringBuilder.append(TextUtil.NEWLINE);
    }
    
    private boolean isMeterChange(String timeSignature, Note firstNote) {
        return firstNote.beatInfo.timeSignature() != null &&
                false == timeSignature.equals(firstNote.beatInfo.timeSignature());
    }

    private String meterChange(
            StringBuilder result, 
            String timeSignature, 
            Note firstNote,
            boolean barStart, 
            boolean gotoNextLine)
    {
        timeSignature = firstNote.beatInfo.timeSignature(); // pick up new one
        
        if (barStart == false)
            result.append(ABC_BAR);
        
        final String escape = gotoNextLine ? "" : "\\"; // escape backslash when going to next ABC text line
        result.append(escape+TextUtil.NEWLINE);
        
        result.append("M:"+timeSignature); // no space allowed here in mid-tune!
        if (gotoNextLine == false)
            result.append(TextUtil.NEWLINE);
        
        return timeSignature;
    }
    
    private String getAbcDottedLength(boolean wasDotted, Note firstNote, boolean isDotted) {
        return wasDotted
                ? toAbcDoubleLength(firstNote.lengthNotation, isDotted) // abc shortens a ">" follower note, thus double it
                : toAbcLength(firstNote.lengthNotation, isDotted);
    }

    private boolean detectSlurStart(StringBuilder result, boolean inSlur, Note firstNote) {
        final boolean slurStart = (inSlur == false) && Boolean.TRUE.equals(firstNote.connectionFlags.slurred());
        if (slurStart) {
            result.append(ABC_SLUR_OPEN);
            inSlur = true;
        }
        return inSlur;
    }

    private boolean detectSlurEnd(StringBuilder result, boolean inSlur, Note lastNote) {
        final boolean slurEnd = (inSlur == true) && Boolean.FALSE.equals(lastNote.connectionFlags.slurred());
        if (slurEnd) {
            result.append(ABC_SLUR_CLOSE);
            inSlur = false;
        }
        return inSlur;
    }

    private boolean isDotted(String lengthNotation) {
        return lengthNotation.endsWith(Note.DOTTED_SYMBOL);
    }
    
    private String toAbcLength(String lengthNotation, boolean isDotted) {
        lengthNotation = stripLengthToNumber(lengthNotation);
        return editDottedLength(lengthNotation, isDotted);
    }
    
    private String toAbcDoubleLength(String lengthNotation, boolean isDotted) {
        lengthNotation = stripLengthToNumber(lengthNotation);
        lengthNotation = Integer.toString(Integer.valueOf(lengthNotation) / 2);
        return editDottedLength(lengthNotation, isDotted);
    }
    
    private String editDottedLength(String lengthNotation, boolean isDotted) {
        return lengthNotation + (isDotted ? ABC_DOT : "");
    }
    
    private String stripLengthToNumber(String lengthNotation) {
        if (lengthNotation.endsWith(Note.DOTTED_SYMBOL))
            lengthNotation = lengthNotation.substring(0, lengthNotation.length() - Note.DOTTED_SYMBOL.length());
        
        final int multipletSeparatorIndex = lengthNotation.indexOf(Note.MULTIPLET_SEPARATOR);
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