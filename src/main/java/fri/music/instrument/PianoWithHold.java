package fri.music.instrument;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.util.LinkedHashSet;
import java.util.SequencedSet;
import javax.swing.BorderFactory;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.border.Border;
import fri.music.SoundChannel;

/**
 * Piano that enables to continue playing clicked keys
 * until they get clicked again.
 */
public class PianoWithHold extends PianoWithVolume
{
    private JComponent pianoPanel;
    
    public PianoWithHold(SoundChannel channel) {
        this(null, channel);
    }
    public PianoWithHold(PianoWithSound.Configuration config, SoundChannel channel) {
        super(config, channel);
    }
    
    @Override
    public JComponent getKeyboard() {
        if (this.pianoPanel != null)
            return this.pianoPanel; // just one view, due to mouseHandler that stores UI-state
        
        final JComponent pianoPanel = super.getKeyboard();
        
        final JCheckBox holdCheckbox = new JCheckBox("Hold");
        holdCheckbox.setToolTipText("Hold struck tones until struck again");
        holdCheckbox.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                ((HoldMouseHandler) getMouseHandler()).setHoldState(holdCheckbox.isSelected());
            }
        });
        getControlPanel().add(holdCheckbox, 1);
        
        return this.pianoPanel = pianoPanel;
    }
    
    /** Overwritten to use HoldMouseHandler, implementing "Hold" checkbox. */
    @Override
    protected MouseHandler newMouseHandler() {
        return new HoldMouseHandler(this);
    }
    
    
    protected static class HoldMouseHandler extends MouseHandler
    {
        /** Contains notes that play continuously due to activated "Hold" checkbox. */
        protected final SequencedSet<Keyboard.Key> holdPlayingNotes = new LinkedHashSet<>();
        private boolean isHoldActive;
        private final Border redBorder;
        private Border originalBorder;
        
        public HoldMouseHandler(PianoWithSound piano) {
            super(piano);
            redBorder = BorderFactory.createLineBorder(Color.RED, 2);
        }
        
        /** Sets a new hold state (whether or not notes should play continuously) and calls reset(). */
        public final void setHoldState(boolean holdState) {
            isHoldActive = holdState;
            reset();
        }

        @Override
        public void mousePressed(MouseEvent e) {
            if (isHoldActive() == false || isPlaying(e) == false)
                super.mousePressed(e);
            else // is playing
                super.mouseReleased(e);
        }
        
        @Override
        public void mouseReleased(MouseEvent e) {
            if (isHoldActive() == false)
                super.mouseReleased(e);
            else
                mouseOverKey = null; // stops glissando and holds current tone
        }
        
        @Override
        protected void noteOn(Keyboard.Key key) {
            if (isHoldActive()) {
                holdPlayingNotes.add(key);
                setRedBorder(key, true);
            }
            super.noteOn(key);
        }
        @Override
        protected void noteOff(Keyboard.Key key) {
            if (isHoldActive()) {
                holdPlayingNotes.remove(key);
                setRedBorder(key, false);
            }
            super.noteOff(key);
        }
        
        /** Resets this listener completely. */
        protected void reset() {
            piano.getSoundChannel().allNotesOff();
            mouseOverKey = null;
            
            if (originalBorder != null)
                for (Keyboard.Key key : holdPlayingNotes)
                    setRedBorder(key, false);
            
            holdPlayingNotes.clear();
        }

        protected final boolean isHoldActive() {
            return isHoldActive;
        }
        
        protected void setRedBorder(Keyboard.Key key, boolean select) {
            if (originalBorder == null) // save original border for reset
                originalBorder = key.getBorder();
            key.setBorder(select ? redBorder : originalBorder);
        }
        
        private boolean isPlaying(MouseEvent e) {
            return holdPlayingNotes.contains(getKey(e));
        }
    }
}