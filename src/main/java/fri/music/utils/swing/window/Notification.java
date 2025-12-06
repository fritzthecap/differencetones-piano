package fri.music.utils.swing.window;

import java.awt.Color;
import java.awt.Component;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowEvent;
import java.util.List;
import java.util.Objects;
import javax.swing.AbstractAction;
import javax.swing.BorderFactory;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.SwingUtilities;
import javax.swing.border.Border;
import fri.music.utils.swing.KeyStrokeUtil;
import fri.music.utils.swing.layout.LocationUtil;

/**
 * A notification window without title-bar that is
 * always over a given parent component (like a JButton).
 * It can be closed by clicking into it, by ENTER key, 
 * or by closing its parent window.
 */
public class Notification
{
    private final Component component;
    private final Window window;
    private final JDialog dialog;
    private final JLabel linesLabel;
    
    /** @param parent required, the component to show over. */
    public Notification(Component parent) {
        this.component = Objects.requireNonNull(parent);
        this.window = Objects.requireNonNull(SwingUtilities.windowForComponent(parent));
        this.dialog = new JDialog(window, "Undecorated");
        dialog.setUndecorated(true);
        this.linesLabel = new JLabel();
        
        setBorder(BorderFactory.createLineBorder(Color.BLUE, 3, true));
        
        KeyStrokeUtil.install(linesLabel, JComponent.WHEN_IN_FOCUSED_WINDOW, "closeAction", KeyEvent.VK_ENTER, new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                dialog.dispatchEvent(new WindowEvent(dialog, WindowEvent.WINDOW_CLOSING));
            }
        });
        linesLabel.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                dialog.dispatchEvent(new WindowEvent(dialog, WindowEvent.WINDOW_CLOSING));
            }
        });
        linesLabel.setToolTipText("Click to Close");
        
        window.addComponentListener(new ComponentAdapter() {
            @Override
            public void componentHidden(ComponentEvent e) {
                dialog.setVisible(false);
            }
            @Override
            public void componentMoved(ComponentEvent e) {
                LocationUtil.locateRelativeToComponent(dialog, window, component, true);
            }
            @Override
            public void componentResized(ComponentEvent e) {
                LocationUtil.locateRelativeToComponent(dialog, window, component, true);
            }
            @Override
            public void componentShown(ComponentEvent e) {
                dialog.setVisible(true);
            }
        });
        
        dialog.add(linesLabel);
        setOpacity(0.72f); // transparency
    }
    
    /** Sets given border combined with some inner padding. */
    public void setBorder(Border border) {
        linesLabel.setBorder(
                BorderFactory.createCompoundBorder(
                    border,
                    BorderFactory.createEmptyBorder(8, 24, 8, 24) // left and right is wider
                )
            );
    }

    /** Sets transparency, default opacity is 0.72f. */
    public void setOpacity(float opacity) {
        dialog.setOpacity(opacity);
    }
    
    /**
     * Sets a list of lines to render.
     * @param lines the text lines to render, 
     *      closes the notification when open and given list is empty,
     *      else opens it when not yet open.
     */
    public void setLines(List<String> lines) {
        if (lines.size() <= 0) {
            linesLabel.setText("");
            dialog.setVisible(false);
        }
        else {
            linesLabel.setText(toHtml(lines));
            dialog.pack();
            LocationUtil.locateRelativeToComponent(dialog, window, component, true);
            dialog.setVisible(true);
        }
    }
    
    private String toHtml(List<String> lines) {
        final StringBuilder htmlBuilder = new StringBuilder(lines.size() * 3 + 24);
        // each line has maximum 3 chars, HTML wrapper needs 24
        
        htmlBuilder.append("<html>");
        for (final String textLine : lines)
            htmlBuilder.append("<div>"+textLine+"</div>");
        htmlBuilder.append("</html>");
        
        return htmlBuilder.toString();
    }
}