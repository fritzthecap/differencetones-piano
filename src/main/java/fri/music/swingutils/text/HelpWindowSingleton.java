package fri.music.swingutils.text;

import java.awt.Component;
import java.awt.Window;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.net.URL;
import java.util.Hashtable;
import java.util.Map;
import fri.music.HtmlResources;
import fri.music.swingutils.window.DialogStarter;

/**
 * Avoid help multiple "Help" windows for the same URL.
 */
public final class HelpWindowSingleton
{
    private static final Map<URL,Window> urlToWindow = new Hashtable<>();
    
    public static Window start(Component parent, String title, URL helpUrl) {
        final Window window = urlToWindow.get(helpUrl);
        if (window != null) {
            window.setVisible(true);
            return window;
        }
        
        final Window helpDialog = DialogStarter.htmlDialog(
                title,
                parent,
                helpUrl,
                HtmlResources.DEFAULT_FRAME_SIZE);
        
        helpDialog.addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                final URL url = urlToWindow.entrySet().stream()
                        .filter(entry-> entry.getValue() == e.getSource())
                        .map(entry -> entry.getKey())
                        .findFirst()
                        .orElse(null);
                if (url != null)
                    urlToWindow.remove(url);
            }
        });

        urlToWindow.put(helpUrl, helpDialog);
        
        return helpDialog;
    }
    
    private HelpWindowSingleton() {} // do not instantiate
}
