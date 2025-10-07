package fri.music.utils;

public final class StringUtil
{
    public static final String NEWLINE = System.getProperty("line.separator");
    
    /**
     * @param stringBuilder the string to examine for ending with newline.
     * @return true when end of string is a platform-newline ("line.separator"), else false.
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
     * @return the name without first occurring number, e.g. "C" from "C4".
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
