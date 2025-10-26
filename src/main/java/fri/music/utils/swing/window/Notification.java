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
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import javax.swing.AbstractAction;
import javax.swing.BorderFactory;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.SwingUtilities;
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
    private final Window window;
    private final JDialog dialog;
    private final JLabel linesLabel;
    
    private Component component;
    private List<String> lines = new ArrayList<>();
    
    public Notification(Component parent) {
        this(SwingUtilities.windowForComponent(parent), parent);
    }
    
    private Notification(Window parentWindow, Component parent) {
        this.window = Objects.requireNonNull(parentWindow);
        this.component = Objects.requireNonNull(parent);
        
        this.dialog = new JDialog(window, "Undecorated");
        dialog.setUndecorated(true);
        
        this.linesLabel = new JLabel();
        linesLabel.setBorder(
                BorderFactory.createCompoundBorder(
                    BorderFactory.createLineBorder(Color.BLUE, 3, true),
                    BorderFactory.createEmptyBorder(8, 24, 8, 24)
                )
            );
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
        dialog.setOpacity(0.72f);
    }
    
    public void addLine(String line, int index) {
        final int i = (index < 0) ? lines.size() : Math.min(index, lines.size());
        lines.add(i, line);
        refresh();
    }
    
    public void removeLine(String line) {
        if (lines.remove(line))
            refresh();
    }
    
    public void hide() {
        lines.clear();
        refresh();
    }

    
    private void refresh() {
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
        final StringBuilder sb = new StringBuilder("<html>");
        for (final String textLine : lines)
            sb.append("<div>"+textLine+"</div>");
        sb.append("</html>");
        return sb.toString();
    }
    
    
    /*
    public static void main(String[] args) {
        final javax.swing.JFrame frame = new javax.swing.JFrame("Notification Test");
        frame.setDefaultCloseOperation(javax.swing.JFrame.EXIT_ON_CLOSE);
        
        final javax.swing.JPanel panel = new javax.swing.JPanel(new java.awt.BorderLayout());
        panel.add(new JLabel(" "), java.awt.BorderLayout.CENTER);
        
        final javax.swing.JButton notificationButton1 = new javax.swing.JButton("Launch Notification");
        final javax.swing.JButton notificationButton2 = new javax.swing.JButton("Launch Notification");
        final javax.swing.JPanel buttonPanel = new javax.swing.JPanel(new java.awt.FlowLayout());
        buttonPanel.add(notificationButton1);
        buttonPanel.add(notificationButton2);
        panel.add(buttonPanel, java.awt.BorderLayout.SOUTH);
        
        frame.getContentPane().add(panel);
        
        final Notification notification = new Notification(notificationButton1);
        
        final java.awt.event.ActionListener actionListener = new java.awt.event.ActionListener() {
            private int count = 0;
            
            @Override
            public void actionPerformed(java.awt.event.ActionEvent e) {
                count++;
                notification.addLine("Line "+count, -1);
            }
        };
        notificationButton1.addActionListener(actionListener);
        notificationButton2.addActionListener(actionListener);
        
        frame.setSize(new java.awt.Dimension(400, 400));
        frame.setLocationByPlatform(true);
        frame.setVisible(true);
    }*/
}
