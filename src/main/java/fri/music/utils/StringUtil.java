package fri.music.utils;

public final class StringUtil
{
    /**
     * The Java-internal newline, as used in JTextArea.
     * No read or write from/to file-system happens in this application, so we don't need more.
     */
    public static final String NEWLINE = "\n"; // System.getProperty("line.separator");
    
    /**
     * @param stringBuilder the string to examine for ending with newline.
     * @return true when end of string is a newline, else false.
     */
    public static boolean endsWithNewline(StringBuilder stringBuilder) {
        final int newlineLength = NEWLINE.length();
        if (stringBuilder.length() < newlineLength)
            return false;
        
        int checkPosition = stringBuilder.length() - newlineLength;
        for (int i = 0; i < NEWLINE.length(); i++, checkPosition++)
            if (stringBuilder.charAt(checkPosition) != NEWLINE.charAt(i))
                return false;
        
        return true;
    }
    
    /**
     * @param stringBuilder the text where to remove trailing blanks from all lines.
     * @return the given text without trailing line blanks.
     */
    public static String removeTrailingLineBlanks(StringBuilder stringBuilder) {
        final StringBuilder sb = new StringBuilder();
        for (String line : stringBuilder.toString().lines().toList()) {
            sb.append(line.stripTrailing());
            sb.append(NEWLINE);
        }
        return sb.toString();
    }
    
    /**
     * This is 10 times faster than <code>Integer.valueOf(ipnName.replaceAll("[^0-9]", ""))</code>.
     * @param name some IPN note name like "C4".
     * @return the first occurring number in given name, e.g. 4 from "C4",
     *      or -1 when no number found.
     */
    public static int getFirstNumber(String name) {
        boolean inDigits = false;
        int number = 0;
        for (int i = 0; i < name.length(); i++) {
            char c = name.charAt(i);
            if (Character.isDigit(c)) {
                inDigits = true;
                number = number * 10 + (c - '0');
            }
            else if (inDigits)
                return number;
        }
        return inDigits ? number : -1;
    }

    /**
     * This is 10 times faster than <code>ipnName.replaceAll("[0-9]", "")</code>.
     * @param name some IPN note name like "C4".
     * @return the name without first occurring number, e.g. "C" from "C4",
     *      or empty string if first character is digit,
     *      or given name if no digit is in it.
     */
    public static String getUntilFirstNumber(String name) {
        final StringBuilder sb = new StringBuilder();
        for (int i = 0; i < name.length(); i++) {
            char c = name.charAt(i);
            if (Character.isDigit(c))
                return sb.toString();
            sb.append(c);
        }
        return sb.toString();
    }

    private StringUtil() {} // do not instantiate
}