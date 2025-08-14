package fri.music;

public final class TextUtil
{
    public static final String NEWLINE = System.getProperty("line.separator");
    
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

    private TextUtil() {} // do not instantiate
}
