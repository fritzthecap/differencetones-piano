package fri.music.swingutils;

import java.awt.Component;
import java.awt.Dimension;

public class SizeUtil
{
    public static void forceSize(Component component, Dimension size) {
        component.setPreferredSize(size);
        component.setMinimumSize(size);
        component.setMaximumSize(size);
    }

}
