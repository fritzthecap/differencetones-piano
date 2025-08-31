package fri.music.player.notelanguage.abc;

import fri.music.TextUtil;

/** Merges to ABC tunes to one with two voices. */
public class AbcTunesCombiner
{
    public String combine(String upperVoiceName, String upperVoice, String lowerVoiceName, String lowerVoice) {
        final String[] upperHeaderAndNotes = splitIntoHeaderAndNotes(upperVoice);
        final String[] lowerHeaderAndNotes = splitIntoHeaderAndNotes(lowerVoice);
        
        final String header = lowerHeaderAndNotes[0];
        
        final String upperVoiceDefinition = "V: "+upperVoiceName+TextUtil.NEWLINE;
        final String lowerVoiceDefinition = "V: "+lowerVoiceName+TextUtil.NEWLINE;
        
        final String upperTune = upperHeaderAndNotes[1];
        final String lowerTune = lowerHeaderAndNotes[1];
        
        final StringBuilder sb = new StringBuilder();
        sb.append(header);
        sb.append(upperVoiceDefinition);
        sb.append(upperTune);
        sb.append(lowerVoiceDefinition);
        sb.append(lowerTune);
        
        return sb.toString();
    }

    private String[] splitIntoHeaderAndNotes(String abcTune) {
        // scan for last occurrence of newline - uppercase letter - colon
        final StringBuilder header = new StringBuilder();
        final StringBuilder notes = new StringBuilder();
        final String[] lines = abcTune.lines().toArray(String[]::new);
        for (String line : lines) {
            final char c1 = line.charAt(0);
            final char c2 = line.charAt(1);
            final boolean headerLine = (Character.isLetter(c1) && Character.isUpperCase(c1) && c2 == ':');
            line = line + TextUtil.NEWLINE;
            if (headerLine)
                header.append(line);
            else
                notes.append(line);
        }
        return new String[] { header.toString(), notes.toString() };
    }
}