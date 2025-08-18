package fri.music.instrument.notespiano;

import java.awt.Dimension;
import java.awt.Font;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.Objects;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JPanel;

/**
 * The player controls for playing, rewinding and single-step playing.
 */
public class PlayControlButtons extends JPanel
{
    /** Who wants to receive control button clicks implements this interface. */
    public interface Listener
    {
        /** Stops playing when playing and rewinds to start but does not play any note. */
        void fastBackwardPressed();
        /** Plays the previous note continually, or stops playing. */
        void backwardPressed();
        /** Stops continual playing of previous note. */
        void backwardReleased();
        /** Plays the melody backwards. */
        void reversePressed();
        /** Plays the melody forwards. */
        void playPressed();
        /** Plays the next note continually, or stops playing when playing. */
        void forwardPressed();
        /** Stops continual playing of next note. */
        void forwardReleased();
        /** Stops playing when playing and goes to end but does not play any note. */
        void fastForwardPressed();
    }
    
    private static final String REVERSE = "\u23F4";
    private static final String PLAY    = "\u23F5";
    private static final String STOP    = "\u23F9";
    
    private final JButton fastBackward;
    private final JButton backward;
    private final JButton reverse;
    private final JButton play;
    private final JButton forward;
    private final JButton fastForward;
    
    private final Listener listener;
    
    PlayControlButtons(Listener listener) {
        this.listener = Objects.requireNonNull(listener);
        
        final BoxLayout layout = new BoxLayout(this, BoxLayout.X_AXIS);
        setLayout(layout);
        
        add(this.fastBackward = newButton("\u23EE", "Rewind to Start", false));
        add(this.backward     = newButton("\u23EA", "Play Previous Note", true));
        add(this.reverse      = newButton(REVERSE, "Play Notes in Textarea in Reverse Order", false));
        add(this.play         = newButton(PLAY, "Play Notes in Textarea on Piano", false));
        add(this.forward      = newButton("\u23E9", "Play Next Note", true));
        add(this.fastForward  = newButton("\u23ED", "Go to End", false));
    }

    /** Overridden to delegate enabling to buttons. */
    @Override
    public void setEnabled(boolean enabled) {
        fastBackward.setEnabled(enabled);
        backward.setEnabled(enabled);
        reverse.setEnabled(enabled);
        play.setEnabled(enabled);
        forward.setEnabled(enabled);
        fastForward.setEnabled(enabled);
    }
    
    /** Called by controller, sets the correct icon on "Play" button. */
    void setPlaying(boolean playing, boolean isReverse) {
        if (isReverse)
            reverse.setText(playing ? STOP : REVERSE);
        else
            play.setText(playing ? STOP : PLAY);
    }

    
    private JButton newButton(String text, String tooltip, boolean isSingleStepButton) {
        final JButton button = new JButton(text);
        button.setToolTipText(tooltip);
        // make font bigger for UNICODE letters
        button.setFont(new Font(Font.SANS_SERIF, Font.PLAIN, 18));
        button.setMargin(new Insets(1, 1, 1, 1)); // else text will be "..."
        button.setFocusPainted(false); // no thin rectangle on focus
        // make buttonas small as possible
        final Dimension size = new Dimension(25, 25);
        button.setPreferredSize(size);
        button.setMaximumSize(size);
        button.setMinimumSize(size);
        
        // install listener callbacks
        if (isSingleStepButton)
            button.addMouseListener(new MouseAdapter() {
                @Override
                public void mousePressed(MouseEvent e) {
                    if (e.getSource() == backward)
                        listener.backwardPressed();
                    else if (e.getSource() == forward)
                        listener.forwardPressed();
                }
                @Override
                public void mouseReleased(MouseEvent e) {
                    if (e.getSource() == backward)
                        listener.backwardReleased();
                    else if (e.getSource() == forward)
                        listener.forwardReleased();
                }
            });
        else
            button.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    if (listener == null)
                        throw new IllegalStateException("You forgot to set a button listener!");
                    
                    if (e.getSource() == fastBackward)
                        listener.fastBackwardPressed();
                    else if (e.getSource() == reverse)
                        listener.reversePressed();
                    else if (e.getSource() == play)
                        listener.playPressed();
                    else if (e.getSource() == fastForward)
                        listener.fastForwardPressed();
                }
            });
            
        return button;
    }
}