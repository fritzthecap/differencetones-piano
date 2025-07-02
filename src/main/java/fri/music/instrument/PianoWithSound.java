package fri.music.instrument;

import java.awt.Color;
import java.awt.Component;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.event.FocusEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import javax.swing.JComponent;
import fri.music.ScaleTypes;
import fri.music.SoundChannel;
import fri.music.ToneSystem;

/**
 * Piano keyboard user-interface with 12 white and black keys.
 * Optionally displays MIDI number and IPN-name on keys.
 * Installs a mouse-listener to respond to mouse-clicks with real sound.
 */
public class PianoWithSound extends Piano
{
    /** Adds view-options to super-class configuration. */
    public static class Configuration extends Piano.Configuration
    {
        public final boolean showIpnNameOnKey;
        public final boolean showMidiNumberAsTooltip;
        
        public Configuration() {
            this(-1, null);
        }
        public Configuration(int octaves, String lowestToneIpnName) {
            this(octaves, lowestToneIpnName, -1);
        }
        public Configuration(int octaves, String lowestToneIpnName, int blackKeyWidthPixels) {
            this(octaves, lowestToneIpnName, null, blackKeyWidthPixels);
        }
        public Configuration(int octaves, String lowestToneIpnName, Boolean isVertical, int blackKeyWidthPixels) {
            this(octaves, lowestToneIpnName, isVertical, blackKeyWidthPixels, null, null, null);
        }
        /**
         * See super-class.
         * @param showIpnNameOnKey true for showing IPN-names on piano keys.
         * @param showMidiNumberAsTooltip true for showing MIDI note numbers in tooltips of keys.
         */
        public Configuration(int octaves, String lowestToneIpnName, Boolean isVertical, 
                int blackKeyWidthPixels, Boolean showIpnNameOnKey, Boolean showMidiNumberAsTooltip,
                Boolean colouredWhiteKeys)
        {
            super(octaves, lowestToneIpnName, isVertical, blackKeyWidthPixels, colouredWhiteKeys);
            
            this.showIpnNameOnKey = (showIpnNameOnKey == null) ? true : showIpnNameOnKey;
            this.showMidiNumberAsTooltip = (showMidiNumberAsTooltip == null) ? true : showMidiNumberAsTooltip;
        }
    }
    
    
    /** Adds MIDI-numbers and IPN-names to keys of super-class. */
    public static class Keyboard extends Piano.Keyboard
    {
        protected final int lowestToneMidiNumber;
        
        public Keyboard(PianoWithSound.Configuration config) {
            super(config);
            
            final int semitoneInOctaveBasedOnC = ScaleTypes.cBasedSemitoneIndex(config.lowestToneIpnName);
            this.lowestToneMidiNumber = 
                    ToneSystem.DEFAULT_BASETONE_MIDI_NUMBER + // 12
                    (config.lowestToneOctaveBasedOnC * ToneSystem.SEMITONES_PER_OCTAVE) + // 12
                    semitoneInOctaveBasedOnC;
        }
        
        private PianoWithSound.Configuration config() {
            return (PianoWithSound.Configuration) config;
        }
    
        /** Overridden to deliver another type of black key. */
        @Override
        protected Piano.Keyboard.Key newBlackKey(int absoluteIndexInBlackKeys) {
            return new PianoWithSound.Keyboard.Key(false, absoluteIndexInBlackKeys);
        }

        /** Overridden to deliver another type of white key. */
        @Override
        protected Piano.Keyboard.Key newWhiteKey( int absoluteIndexInWhiteKeys) {
            return new PianoWithSound.Keyboard.Key(true, absoluteIndexInWhiteKeys);
        }

        
        /** A black or white keyboard-key with IPN-name and MIDI-number. */
        public class Key extends Piano.Keyboard.Key implements Comparable<Key>
        {
            public final int midiNoteNumber;
            public final String ipnName;
            public final boolean isWhite;
            
            private boolean ignoreMouse; // is false by default
            
            public Key(boolean isWhite, int indexInWhiteOrBlackKeys) {
                super(isWhite, indexInWhiteOrBlackKeys);
                
                this.isWhite = isWhite;
                
                final int semitoneInOctave = ScaleTypes.octaveBasedSemitoneIndex(
                        config.lowestToneIpnName, positionInOctave, isWhite);
                this.midiNoteNumber = 
                        lowestToneMidiNumber + 
                        (octaveBasedOnScaleStart * ToneSystem.SEMITONES_PER_OCTAVE) + // 12
                        semitoneInOctave;
                
                this.ipnName = ScaleTypes.ipnName(
                        config.lowestToneIpnName, config.lowestToneOctaveBasedOnC + octaveBasedOnScaleStart, semitoneInOctave);
                
                if (config().showMidiNumberAsTooltip)
                    setToolTipText(""+midiNoteNumber);
            }
            
            public void setIgnoreMouse(boolean ignore) {
                this.ignoreMouse = ignore;
            }
            
            @Override
            protected void processMouseEvent(MouseEvent e) {
                if (ignoreMouse == false)
                    super.processMouseEvent(e);
            }
            
            /** Overridden to optionally paint note names onto all keys of the keyboard. */
            @Override
            public void paintComponent(Graphics g) {
                super.paintComponent(g);
                if (config().showIpnNameOnKey == false)
                    return;

                FontMetrics fm = getFontMetrics(g.getFont());
                int width = fm.stringWidth(ipnName);
                int height = (fm.getHeight() + fm.getAscent()) / 2 - fm.getDescent();
                if (getSize().width <= width || getSize().height <= height)
                    return; // too small for text
                
                g.setColor(isWhite ? Color.DARK_GRAY : Color.LIGHT_GRAY);
                if (config.isVertical)
                    g.drawString(ipnName, getSize().width - width - 4, (getSize().height + height) / 2);
                else
                    g.drawString(ipnName, (getSize().width - width) / 2, getSize().height - 6);
            }
            
            /** Make keyboard keys sort-able by MIDI-note-number. */
            @Override
            public int compareTo(Key other) {
                return midiNoteNumber - other.midiNoteNumber;
            }
            
            /** Do nothing to fix bug when selecting interval D-E and then C-D.
             * In that case a focus-lost-event sets the button's armed / pressed to false
             * when they actually have been set to true by visualSelect() . */
            @Override
            protected void processFocusEvent(FocusEvent e) {
            }
            
            @Override
            public String toString() {
                return ipnName+" ("+midiNoteNumber+"), "+(isWhite ? "white" : "black");
            }
        }
    }   // end class Keyboard
    
    
    /** Mouse handler that plays sound on mouse click, and manages glissandos. */
    protected static class MouseHandler extends MouseAdapter
    {
        protected final PianoWithSound piano;
        protected Keyboard.Key mouseOverKey;
        
        public MouseHandler(PianoWithSound piano) {
            this.piano = piano;
        }
        
        /** Listens to key presses to start a tone. */
        @Override
        public void mousePressed(MouseEvent e) {
            final Keyboard.Key key = getKey(e);
            noteOn(key);
            mouseOverKey = key; // start glissando
        }
        /** Listens to key releases to stop a tone. */
        @Override
        public void mouseReleased(MouseEvent e) {
            final Keyboard.Key key = getKey(e);
            noteOff(key);
            stopGlissando(key);
        }
        /** Listens to mouse drag to play entered key as glissando. */
        @Override
        public void mouseEntered(MouseEvent e) {
            final Keyboard.Key changedMouseOverKey = getChangedMouseOverKey(e);
            if (changedMouseOverKey != null) { // mouse is still pressed and entered another key
                noteOff(mouseOverKey);
                continueGlissando(changedMouseOverKey);
            }
        }
        
        protected Keyboard.Key getChangedMouseOverKey(MouseEvent e) {
            if (mouseOverKey != null) { // mouse is still pressed
                final Keyboard.Key key = getKey(e);
                if (mouseOverKey != key) // mouse entered another key
                    return key;
            }
            return null;
        }
        protected void continueGlissando(Keyboard.Key changedMouseOverKey) {
            noteOn(changedMouseOverKey);
            mouseOverKey = changedMouseOverKey;
            // visual selection of current key, will be removed by Swing
            visualSelect(changedMouseOverKey, true);
        }
        protected void stopGlissando(Keyboard.Key key) {
            if (mouseOverKey != null) {
                if (mouseOverKey != key)
                    noteOff(mouseOverKey);
                mouseOverKey = null;
            }
        }
        protected void visualSelect(Keyboard.Key key, boolean pressed) {
            // START keep order - this MUST NOT trigger an action! 
            //     See AbstractButton.doClick() for necessary order of calls to trigger an action
            key.getModel().setPressed(pressed);
            key.getModel().setArmed(pressed);
            // END keep order
        }
        
        protected Keyboard.Key getKey(MouseEvent e) {
            return (Keyboard.Key) e.getSource();
        }
        
        protected void noteOn(Keyboard.Key key) {
            piano.getSoundChannel().noteOn(key.midiNoteNumber, piano.getVelocity());
        }
        protected void noteOff(Keyboard.Key key) {
            piano.getSoundChannel().noteOff(key.midiNoteNumber);
        }
    }   // end class MouseHandler
    
    
    
    /** The sound generator playing key presses, visible for sub-classes. */
    private final SoundChannel soundChannel;
    /** The single mouse-handler for all piano views, visible for sub-classes. */
    private final MouseHandler mouseHandler;
    private JComponent pianoPanel;
    private List<Keyboard.Key> keyList;
    
    public PianoWithSound(SoundChannel channel) {
        this(null, channel);
    }
    public PianoWithSound(PianoWithSound.Configuration config, SoundChannel channel) {
        super(config != null ? config : new PianoWithSound.Configuration());
        this.soundChannel = Objects.requireNonNull(channel);
        this.mouseHandler = newMouseHandler();
    }
    
    /** @return the piano panel, as it was configured by Configuration in constructor. */
    @Override
    public JComponent getKeyboard() {
        if (pianoPanel != null)
            return pianoPanel; // just one view, due to mouseHandler that stores UI-state
        return pianoPanel = super.getKeyboard();
    }

    /** @return fixed velocity (touch force), 127 / 4 = a fourth of possible. To be overridden. */
    protected int getVelocity() {
        return 127 / 4;
    }

    /** Overridden factory method to use Piano.Keyboard. */
    @Override
    protected Piano.Keyboard newKeyboard(Piano.Configuration config) {
        return new PianoWithSound.Keyboard((PianoWithSound.Configuration) config);
    }

    /** Factory method for mouse handling, called from constructor. */
    protected MouseHandler newMouseHandler() {
        return new MouseHandler(this);
    }
    /** The handler from factory. To be overridden for other mouse handlers. */
    protected MouseHandler getMouseHandler() {
        return mouseHandler;
    }

    /** Overridden to handle mouse events for playing sounds. */
    @Override
    protected void configureKeyboardKey(Piano.Keyboard.Key key) {
        key.addMouseListener(getMouseHandler());
    }

    /** @return the sound channel given to constructor. Can be overridden. */
    protected SoundChannel getSoundChannel() {
        return soundChannel;
    }
    
    /** @return all key of the keyboard the mouse-listener was attached to. */
    public final List<Keyboard.Key> getKeys() {
        if (this.keyList != null)
            return this.keyList;
        
        final JComponent viewPort = (JComponent) pianoPanel.getComponent(0);
        final JComponent keyboardPanel = (JComponent) viewPort.getComponent(0);
        final List<PianoWithSound.Keyboard.Key> keyList = new ArrayList<>();
        for (Component component : keyboardPanel.getComponents())
            keyList.add((Keyboard.Key) component);

        Collections.sort(keyList); // Keyboard.Key implements Comparable via midiNoteNumber
        return this.keyList = keyList;
    }
    
    /** @return a WindowListener that calls <code>destroyKeyboard()</code> on window-closing event. */
    public WindowListener getWindowClosingListener() {
        return new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                destroyKeyboard(pianoPanel);
            }
        };
    }
    
    /** Removes mouse listener from all keys and turns off all sound. */
    public void destroyKeyboard(JComponent pianoPanel) {
        for (Keyboard.Key key : getKeys())
            key.removeMouseListener(getMouseHandler());
        getSoundChannel().allNotesOff(); // closes all sound-channels
    }
}