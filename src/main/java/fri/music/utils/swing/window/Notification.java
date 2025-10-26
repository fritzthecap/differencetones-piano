package fri.music.utils.swing.window;

import java.awt.Component;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowEvent;
import java.util.Objects;
import javax.swing.AbstractAction;
import javax.swing.BorderFactory;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.SwingUtilities;
import fri.music.utils.swing.KeyStrokeUtil;

public class Notification
{
    private final Window window;
    private final JDialog dialog;
    private final JLabel textLabel;
    
    public Notification(Component parent) {
        this.window = (parent instanceof Window) ? (Window) parent : SwingUtilities.windowForComponent(Objects.requireNonNull(parent));
        this.dialog = new JDialog(Objects.requireNonNull(window), "");
        dialog.setUndecorated(true);
        
        this.textLabel = new JLabel();
        textLabel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
        KeyStrokeUtil.install(textLabel, JComponent.WHEN_IN_FOCUSED_WINDOW, "closeAction", KeyEvent.VK_ENTER, new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                dialog.dispatchEvent(new WindowEvent(dialog, WindowEvent.WINDOW_CLOSING));
            }
        });
        textLabel.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                dialog.dispatchEvent(new WindowEvent(dialog, WindowEvent.WINDOW_CLOSING));
            }
        });
        
        dialog.add(textLabel);
    }
    
    public void setText(String[] textLines) {
        textLabel.setText(toHtml(textLines));
        dialog.pack();
        dialog.setLocationRelativeTo(window);
        dialog.setVisible(true);
    }

    private String toHtml(String[] textLines) {
        final StringBuilder sb = new StringBuilder("<html>");
        for (final String textLine : textLines) {
            sb.append("<div>"+textLine+"</div>");
        }
        sb.append("</html>");
        return sb.toString();
    }
    
    
    public static void main(String[] args) {
        javax.swing.JFrame frame = new javax.swing.JFrame("Notification Test");
        frame.setDefaultCloseOperation(javax.swing.JFrame.EXIT_ON_CLOSE);
        frame.setSize(new java.awt.Dimension(200, 100));
        
        final javax.swing.JButton notificationButton = new javax.swing.JButton("Launch Notification");
        final Notification notification = new Notification(frame);
        notificationButton.addActionListener(new java.awt.event.ActionListener() {
            private int count = 1;
            
            @Override
            public void actionPerformed(java.awt.event.ActionEvent e) {
                final String[] textLines = new String[count];
                for (int i = 0; i < count; i++)
                    textLines[i] = "Line "+i;
                notification.setText(textLines);
                count++;
            }
        });
        
        frame.getContentPane().add(notificationButton);
        frame.setLocationByPlatform(true);
        frame.setVisible(true);
    }
}
