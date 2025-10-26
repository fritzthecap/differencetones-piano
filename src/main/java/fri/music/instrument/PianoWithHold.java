package fri.music.instrument;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.InputEvent;
import java.util.LinkedHashSet;
import java.util.SequencedSet;
import javax.swing.BorderFactory;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.border.Border;
import fri.music.SoundChannel;

/**
 * Piano that enables to continue playing clicked keys until they get clicked again.
 * The "Hold" state is rendered by a red border, but no other visual selection.
 */
public class PianoWithHold extends PianoWithVolume
{
    private JComponent pianoPanel;
    private JCheckBox holdCheckbox;
    
    public PianoWithHold(SoundChannel channel) {
        this(null, channel);
    }
    public PianoWithHold(PianoWithSound.Configuration config, SoundChannel channel) {
        super(config, channel);
    }
    
    /** @return the piano panel and all controls. */
    @Override
    public JComponent getKeyboard() {
        if (this.pianoPanel != null)
            return this.pianoPanel; // just one view, due to mouseHandler that stores UI-state
        
        final JComponent pianoPanel = super.getKeyboard();
        
        this.holdCheckbox = new JCheckBox("Hold");
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
    
    /** Some sub-classes may prefer to not support "Hold". */
    public void setHoldCheckboxEnabled(boolean enable) {
        holdCheckbox.setEnabled(enable);
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
        private final Border holdBorder;
        private Border originalBorder;
        
        public HoldMouseHandler(PianoWithSound piano) {
            super(piano);
            this.holdBorder = BorderFactory.createLineBorder(Color.RED, 2);
        }
        
        /** Sets a new hold state (whether or not notes should play continuously) and calls reset(). */
        public final void setHoldState(boolean holdState) {
            isHoldActive = holdState;
            reset();
        }

        @Override
        protected void pressed(InputEvent e) {
            if (isHoldActive() == false || isPlaying(e) == false)
                super.pressed(e);
            else // is playing
                super.released(e);
        }
        @Override
        protected void released(InputEvent e) {
            if (isHoldActive() == false)
                super.released(e);
            else 
                setMouseOverKeyToNull();
        }
        
        @Override
        protected void noteOn(Keyboard.Key key) {
            if (isHoldActive()) {
                holdPlayingNotes.add(key);
                setHoldBorder(key, true);
            }
            super.noteOn(key);
        }
        @Override
        protected void noteOff(Keyboard.Key key) {
            if (isHoldActive()) {
                holdPlayingNotes.remove(key);
                setHoldBorder(key, false);
            }
            super.noteOff(key);
        }
        
        /** Resets this listener completely. */
        protected void reset() {
            piano.getSoundChannel().allNotesOff();
            setMouseOverKeyToNull();
            
            if (originalBorder != null)
                for (Keyboard.Key key : holdPlayingNotes)
                    setHoldBorder(key, false);
            
            holdPlayingNotes.clear();
        }

        protected final boolean isHoldActive() {
            final PianoWithHold pianoWithHold = (PianoWithHold) piano;
            return isHoldActive && pianoWithHold.holdCheckbox.isEnabled();
        }
        
        private void setHoldBorder(Keyboard.Key key, boolean select) {
            if (originalBorder == null) // save original border for reset
                originalBorder = key.getBorder();
            key.setBorder(select ? holdBorder : originalBorder);
        }
        
        private boolean isPlaying(InputEvent e) {
            return holdPlayingNotes.contains(getKey(e));
        }
    }
}