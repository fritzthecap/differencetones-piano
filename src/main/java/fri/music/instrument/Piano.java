package fri.music.instrument;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Rectangle;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import fri.music.ScaleTypes;
import fri.music.ToneSystem;

/**
 * Piano keyboard user-interface with 12 keys per octave,
 * 7 white and 5 black, always starting on the left with a white key.
 * Any white key could be the left-sided start key.
 * No mouse listener or sound is provided by this class.
 */
public class Piano
{
    /** Piano creation configuration parameters. */
    public static class Configuration
    {
        public final int octaves;
        public final String lowestToneIpnName;
        public final int lowestToneOctaveBasedOnC;
        public final boolean isVertical;
        public final int blackKeyWidth;
        public final boolean colouredOctaves;
        
        public final int blackKeyHeight;
        public final int whiteKeyWidth;
        public final int whiteKeyHeight;
        
        public Configuration() {
            this(-1, null);
        }
        public Configuration(int octaves, String lowestToneIpnName) {
            this(octaves, lowestToneIpnName, -1);
        }
        public Configuration(int octaves, String lowestToneIpnName, int blackKeyWidthPixels) {
            this(octaves, lowestToneIpnName, null, blackKeyWidthPixels, null);
        }
        /**
         * @param octaves number of octaves the piano should have, minimum 1 to maximum 9.
         * @param lowestToneIpnName the IPN-name of the first (lowest) key on left side,
         *      something like "C1" or "D2", but not "C#1", just "scales" ("modes", Kirchentonarten) are supported.
         *      The lowest tone also determines the scale (mode) that is shown by the piano.
         *      When being e.g. "E3", there will be a PHRYGIAN keyboard with E as the leftmost key.
         * @param isVertical true for a flipped bottom-to-top piano, null or false for default horizontal orientation.
         * @param blackKeyWidthPixels width of a black key, all others will be proportional to this, default is 16.
         */
        public Configuration(int octaves, String lowestToneIpnName, Boolean isVertical, int blackKeyWidthPixels, Boolean colouredOctaves) {
            this.octaves = (octaves <= 0 || octaves > ToneSystem.MAXIMUM_OCTAVES) ? ToneSystem.MAXIMUM_OCTAVES : octaves;
            
            this.isVertical = (isVertical == null) ? false : isVertical;
            
            this.lowestToneIpnName = (lowestToneIpnName == null) ? "C0" : lowestToneIpnName;
            // peel the "5" out of "C5" in lowestToneIpnName
            final String onlyDigits = this.lowestToneIpnName.replaceAll("[^0-9\\-]", ""); // delete non-digits
            this.lowestToneOctaveBasedOnC = (onlyDigits.length() > 0) ? Integer.valueOf(onlyDigits) : 0;
            
            this.blackKeyWidth = (blackKeyWidthPixels <= 0) ? 16 : blackKeyWidthPixels;
            // for the black keys of a horizontal piano, 28 pixels are minimum to see the the IPN-name
            
            this.colouredOctaves = (colouredOctaves == null) ? false : colouredOctaves;
            
            this.blackKeyHeight = (this.blackKeyWidth * 9) / 2;
            this.whiteKeyWidth = (this.blackKeyWidth * 3) / 2;
            this.whiteKeyHeight = (blackKeyHeight * 3) / 2;
            // white key width of about 23.5 mm, length of about 150 mm
            // black key width of about 13.7 mm, length of about 90 mm
        }
    }
    
    
    /** Build-logic for all black and white keys of a piano keyboard. */
    public static class Keyboard
    {
        public static final int WHITE_KEYS_PER_OCTAVE = ScaleTypes.numberOfWhiteKeysPerOctave();
        public static final int BLACK_KEYS_PER_OCTAVE = ScaleTypes.numberOfBlackKeysPerOctave();
        
        protected final Configuration config;
        protected final int numberOfAllWhiteKeys;
        protected final int numberOfAllBlackKeys;
        
        public Keyboard(Configuration config) {
            this.config = config;
            
            this.numberOfAllWhiteKeys = WHITE_KEYS_PER_OCTAVE * config.octaves + 1; // + 1: add also the first note of next octave on right side
            this.numberOfAllBlackKeys = BLACK_KEYS_PER_OCTAVE * config.octaves;
        }
    
        /** Factory method for any black key, to be overridden. */
        protected Piano.Keyboard.Key newBlackKey(int absoluteIndexInBlackKeys) {
            return new Key(false, absoluteIndexInBlackKeys);
        }

        /** Factory method for any white key, to be overridden. */
        protected Piano.Keyboard.Key newWhiteKey( int absoluteIndexInWhiteKeys) {
            return new Key(true, absoluteIndexInWhiteKeys);
        }


        /** A black or white keyboard-key. */
        public class Key extends JButton
        {
            /** Octave 0-n relative to lowest tone of the keyboard (lowest is not always C!). */
            protected final int octaveBasedOnScaleStart;
            /** Index 0-6 (white) or 0-4 (black) relative to lowest tone of the current octave. */
            protected final int positionInOctave;
            
            /**
             * @param isWhite true for a white key, false for a black key.
             * @param indexInKeysOfSameType the 0-n index in all black keys when isWhite false, 
             *      the 0-n index in all white keys when isWhite true. This is NOT a semi-tone index!
             */
            public Key(boolean isWhite, int indexInKeysOfSameType) {
                final int numberOfKeysPerOctave = isWhite ? WHITE_KEYS_PER_OCTAVE : BLACK_KEYS_PER_OCTAVE;
                this.octaveBasedOnScaleStart = indexInKeysOfSameType / numberOfKeysPerOctave;
                this.positionInOctave = indexInKeysOfSameType % numberOfKeysPerOctave;
                
                if (isWhite) {
                    final int indexInWhiteKeys = indexInKeysOfSameType;
                    final int offset = config.whiteKeyWidth * indexInWhiteKeys; // first offset will be 0
                    if (config.isVertical) { // swap x and y and width and height, flip top to bottom
                        final int bottom = numberOfAllWhiteKeys * config.whiteKeyWidth;
                        final int y = bottom - offset - config.whiteKeyWidth; // minus whiteKeyWidth because it is positioned from top
                        setBounds(0, y, config.whiteKeyHeight, config.whiteKeyWidth);
                    }
                    else {
                        setBounds(offset, 0, config.whiteKeyWidth, config.whiteKeyHeight);
                    }
                    
                    if (config.colouredOctaves)
                        setBackground(OctaveColor.forOctave(config.lowestToneOctaveBasedOnC + octaveBasedOnScaleStart));
                    else
                        setBackground(Color.WHITE);
                }
                else { // is black key
                    final int whiteKeysBelowInOctave = ScaleTypes.numberOfWhiteKeysBelowBlackKey(config.lowestToneIpnName, positionInOctave);
                    final int gapPosition = config.whiteKeyWidth * (octaveBasedOnScaleStart * WHITE_KEYS_PER_OCTAVE + whiteKeysBelowInOctave);
                    if (config.isVertical) { // swap x and y and width and height, flip top to bottom
                        final int bottom = numberOfAllWhiteKeys * config.whiteKeyWidth;
                        final int y = bottom - gapPosition - config.blackKeyWidth / 2; // minus blackKeyWidth because it is positioned from top
                        setBounds(0, y, config.blackKeyHeight, config.blackKeyWidth);
                    }
                    else {
                        setBounds(gapPosition - config.blackKeyWidth / 2, 0, config.blackKeyWidth, config.blackKeyHeight);
                    }
                    setBackground(Color.BLACK);
                }
            }
        }   // end class Key
    }   // end class Keyboard
    
    
    
    protected final Piano.Configuration config;
    
    /** A horizontal piano view with 7 octaves, based on "C1". */
    public Piano() {
        this(null);
    }
    /** A horizontal piano view according to given configuration. */
    public Piano(Piano.Configuration config) {
        this.config = (config != null) ? config : new Piano.Configuration();
    }
    
    /** @return a new horizontal piano view according to given configuration. */
    public JComponent getKeyboard() {
        final Keyboard keyboard = newKeyboard(config);
        
        final JPanel keyboardPanel = new JPanel(null) { // null layout
            @Override
            public Dimension getPreferredSize() {
                final Component rightMost = getComponent(getComponentCount() - 1);
                final Rectangle rightMostBounds = rightMost.getBounds();
                final int width = rightMostBounds.x + rightMostBounds.width;
                final int height = rightMostBounds.y + rightMostBounds.height;
                return new Dimension(width, height);
            }

            /** @return false because black keys will overlap white keys. */
            @Override
            public boolean isOptimizedDrawingEnabled() {
                return false;
            }
        };

        addBlackAndWhiteKeys(keyboard, keyboardPanel);

        return new JScrollPane(keyboardPanel);
    }

    private void addBlackAndWhiteKeys(Keyboard keyboard, JPanel keyboardPanel) {
        final List<Keyboard.Key> blackKeyList = new ArrayList<>();
        for (int i = 0; i < keyboard.numberOfAllBlackKeys; i++) {
            final Keyboard.Key key = keyboard.newBlackKey(i);
            configureKeyboardKey(key);
            blackKeyList.add(key);
        }
        final List<Keyboard.Key> whiteKeyList = new ArrayList<>();
        for (int i = 0; i < keyboard.numberOfAllWhiteKeys; i++) {
            final Keyboard.Key key = keyboard.newWhiteKey(i);
            configureKeyboardKey(key);
            whiteKeyList.add(key);
        }
        
        // Swing calculates correct preferred size only when
        // children are added starting from zero (both x or y)
        if (keyboard.config.isVertical) {
            Collections.reverse(blackKeyList);
            Collections.reverse(whiteKeyList);
        }
        
        // START keep order of statements:
        // overlapping buttons, Swing paints the last added button first
        for (Keyboard.Key key : blackKeyList)
            keyboardPanel.add(key);
        for (Keyboard.Key key : whiteKeyList)
            keyboardPanel.add(key);
        // END keep order of statements
    }

    /** Factory method for Keyboard, called from getKeyboard(), to be overridden. */
    protected Piano.Keyboard newKeyboard(Piano.Configuration config) {
        return new Piano.Keyboard(config);
    }

    /**
     * Does nothing, called from build. 
     * To be overridden by sub-classes that want to play a tone on key press.
     * @param key the keyboard-key that was just generated and can be configured here.
     */
    protected void configureKeyboardKey(Keyboard.Key key) {
    }
}