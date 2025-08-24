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

    public static String getWithoutFirstNumber(String name) {
        final StringBuilder sb = new StringBuilder();
        boolean checkDigits = true;
        boolean inDigits = false;
        for (int i = 0; i < name.length(); i++) {
            char c = name.charAt(i);
            if (checkDigits && Character.isDigit(c)) {
                inDigits = true;
            }
            else {
                sb.append(c);
                if (inDigits)
                    checkDigits = false;
            }
        }
        return sb.toString();
    }

    private TextUtil() {} // do not instantiate
}
