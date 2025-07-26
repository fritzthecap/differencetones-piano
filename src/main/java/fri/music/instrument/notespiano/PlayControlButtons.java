package fri.music.instrument.notespiano;

import java.awt.Dimension;
import java.awt.Font;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JPanel;

class PlayControlButtons extends JPanel
{
    public interface Listener
    {
        void fastBackwardPressed();
        void backwardPressed();
        void playPressed();
        void fastForwardPressed();
        void forwardPressed();
    }
    
    private static final String PLAY = "\u23F5";
    private static final String STOP = "\u23F9";
    
    public final JButton fastBackward;
    public final JButton backward;
    public final JButton play;
    public final JButton forward;
    public final JButton fastForward;
    
    private Listener listener;
    
    PlayControlButtons() {
        final BoxLayout layout = new BoxLayout(this, BoxLayout.X_AXIS);
        setLayout(layout);
        
        add(fastBackward = newButton("\u23EE", "Rewind to Start"));
        add(backward     = newButton("\u23EA", "Play Previous Note"));
        add(play         = newButton("\u23F5", "Play Notes in Textarea on Piano"));
        add(forward      = newButton("\u23E9", "Play Next Note"));
        add(fastForward  = newButton("\u23ED", "Go to End"));
    }

    /** Overridden to delegate enabling to buttons. */
    @Override
    public void setEnabled(boolean enabled) {
        fastBackward.setEnabled(enabled);
        backward.setEnabled(enabled);
        play.setEnabled(enabled);
        forward.setEnabled(enabled);
        fastForward.setEnabled(enabled);
    }
    
    /** Called by controller, sets it as listener. */
    void setListener(Listener listener) {
        this.listener = listener;
    }

    /** Called by controller, sets the correct icon on "Play" button. */
    void setPlaying(boolean playing) {
        play.setText(playing ? STOP : PLAY);
    }

    
    private JButton newButton(String text, String tooltip) {
        final JButton button = new JButton(text);
        button.setToolTipText(tooltip);
        button.setFont(new Font(Font.SANS_SERIF, Font.PLAIN, 18));
        button.setMargin(new Insets(4, 4, 4, 4));
        button.setFocusPainted(false);
        
        final Dimension size = new Dimension(30, 30);
        button.setPreferredSize(size);
        button.setMaximumSize(size);
        button.setMinimumSize(size);
        
        button.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                if (listener != null)
                    if (e.getSource() == fastBackward)
                        listener.fastBackwardPressed();
                    else if (e.getSource() == backward)
                        listener.backwardPressed();
                    else if (e.getSource() == play)
                        listener.playPressed();
                    else if (e.getSource() == forward)
                        listener.forwardPressed();
                    else if (e.getSource() == fastForward)
                        listener.fastForwardPressed();
            }
        });
        
        return button;
    }
    
    
    public static void main(String[] args) {
        javax.swing.JFrame f = new javax.swing.JFrame();
        f.setDefaultCloseOperation(javax.swing.JFrame.EXIT_ON_CLOSE);
        f.getContentPane().add(new PlayControlButtons());
        f.setSize(new Dimension(600, 200));
        f.setLocationRelativeTo(null);
        f.setVisible(true);
    }
}