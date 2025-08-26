package fri.music.demos;

import java.awt.Dimension;
import java.awt.LayoutManager;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import fri.music.instrument.OctaveColor;

public class SoundColorDemo
{
    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            final JFrame frame = new JFrame("Octave Colors");
            frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            
            final JPanel colorPanel = new JPanel();
            final LayoutManager layout = new BoxLayout(colorPanel, BoxLayout.X_AXIS);
            colorPanel.setLayout(layout);
            for (OctaveColor soundColor : OctaveColor.values()) {
                final JComponent label = new JButton(soundColor.ordinal()+": "+soundColor.name());
                label.setMaximumSize(new Dimension(120, 100));
                label.setPreferredSize(new Dimension(120, 100));
                label.setOpaque(true);
                label.setBackground(soundColor.rgbColor);
                colorPanel.add(label);
            }
            
            frame.getContentPane().add(colorPanel);
            frame.pack();
            frame.setLocationRelativeTo(null);
            frame.setVisible(true);
        });
    }
}