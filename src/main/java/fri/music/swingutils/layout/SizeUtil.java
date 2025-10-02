package fri.music.swingutils.layout;

import java.awt.Component;
import java.awt.Dimension;

public class SizeUtil
{
    public static <C extends Component> C forceSize(C component, Dimension size) {
        component.setPreferredSize(size);
        component.setMinimumSize(size);
        component.setMaximumSize(size);
        return component;
    }

}
