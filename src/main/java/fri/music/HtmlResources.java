package fri.music;

import java.awt.Dimension;
import java.net.URL;

/**
 * HTML help-texts resource-loader for this and all sub.packages.
 */
public interface HtmlResources
{
    /** Support for loading resources from package-local HelpForXXX classes. */
    static URL getUrl(Class<?> htmlResourceClass) {
        return htmlResourceClass.getResource(htmlResourceClass.getSimpleName()+".html");
    }
    
    Dimension DEFAULT_FRAME_SIZE = new Dimension(830, 620);
}
