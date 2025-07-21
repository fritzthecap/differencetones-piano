package fri.music.instrument.wave;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import javax.swing.BorderFactory;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JPanel;
import fri.music.instrument.PianoWithHold;
import fri.music.instrument.PianoWithSound;
import fri.music.swingutils.SmartComboBox;
import fri.music.swingutils.SmartPanel;
import fri.music.wavegenerator.WaveSoundChannel;

/**
 * Piano that auto-plays a configured interval when pressing a key.
 */
public class IntervalPlayingPiano extends PianoWithHold
{
    protected static final List<String> intervalNames = Arrays.asList(
            "", // no interval
            "Minor Second",
            "Major Second",
            "Minor Third",
            "Major Third",
            "Fourth",
            "Tritone",
            "Fifth",
            "Minor Sixth",
            "Major Sixth",
            "Minor Seventh",
            "Major Seventh",
            "Octave");
    
    private JComponent pianoPanel;
    private TuningComponent tuningComponent;
    private JComboBox<String> intervalChoice;
    private JCheckBox intervalActive;
    private int selectedSemitoneSteps = 0;
    
    public IntervalPlayingPiano(PianoWithSound.Configuration config, WaveSoundChannel channel) {
        super(config, channel);
    }
    
    @Override
    public JComponent getKeyboard() {
        if (this.pianoPanel != null)
            return this.pianoPanel;

        final JComponent pianoPanel = super.getKeyboard();
        
        final String[] chooserNames = intervalChooserItems();
        this.intervalChoice = new SmartComboBox(chooserNames);
        intervalChoice.setBorder(BorderFactory.createTitledBorder(intervalChooserLabel()));
        intervalChoice.setToolTipText(intervalChooserTooltip());
        intervalChoice.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                restartPlayingNotes((String) intervalChoice.getSelectedItem());
            }
        });
        
        this.intervalActive = new JCheckBox();
        intervalActive.setToolTipText("Deactivates or Activates Chosen Interval");
        intervalActive.setSelected(true);
        intervalActive.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                final boolean active = ((JCheckBox) e.getSource()).isSelected();
                final String chosenInterval = active 
                        ? (String) intervalChoice.getSelectedItem()
                        : intervalNames.get(0);
                intervalChoice.setEnabled(active);
                restartPlayingNotes(chosenInterval);
            }
        });
        
        final JPanel intervalPanel = new SmartPanel();
        intervalPanel.setLayout(new BorderLayout());
        intervalPanel.add(intervalChoice, BorderLayout.CENTER);
        intervalPanel.add(intervalActive, BorderLayout.EAST);
        
        getControlPanel().add(intervalPanel, 2);
        
        this.tuningComponent = newTuningComponent();
        getControlPanel().add(tuningComponent.getTuningChoice(null), 3);
        
        return this.pianoPanel = pianoPanel;
    }
    
    /** @return the tone-system chooser. */
    public JComboBox<String> getTuningChoice() {
        return tuningComponent.getTuningChoice(null);
    }
    
    /** Some sub-classes may prefer to not support choosing intervals. */
    public void setIntervalChooserEnabled(boolean enable) {
        intervalChoice.setEnabled(enable);
        intervalActive.setEnabled(enable);
    }

    /** Factory method called from getKeyboard(). To be overridden. */
    protected TuningComponent newTuningComponent() {
        return new TuningComponent(config.lowestToneIpnName, config.octaves, (WaveSoundChannel) getSoundChannel());
    }

    protected String intervalChooserLabel() {
        return "Autoplay Interval";
    }

    protected String intervalChooserTooltip() {
        return "Chosen interval will be added automatically when pressing a key";
    }

    protected String[] intervalChooserItems() {
        return intervalNames.toArray(new String[intervalNames.size()]);
    }

    protected void intervalChooserAction(String intervalName) {
        selectedSemitoneSteps = intervalNames.indexOf(intervalName);
    }
    
    /** Overwritten to use AdditionalTonesPlayingMouseHandler, implementing "Intervals" choice. */
    @Override
    protected MouseHandler newMouseHandler() {
        return new AdditionalTonesPlayingMouseHandler(this);
    }
    
    private void restartPlayingNotes(String chosenInterval) {
        intervalChooserAction(chosenInterval);
        ((AdditionalTonesPlayingMouseHandler) getMouseHandler()).restartPlayingNotes();
    }
    
    
    protected static class AdditionalTonesPlayingMouseHandler extends HoldMouseHandler
    {
        /** Holds an actually played piano-key as map-key, and the generated interval-tone as map-value. */
        protected final Map<Keyboard.Key,Object> keyToAdditional = new LinkedHashMap<>();
        
        public AdditionalTonesPlayingMouseHandler(PianoWithSound piano) {
            super(piano);
        }
        
        public final void restartPlayingNotes() {
            final Map<Keyboard.Key,Object> keyMapClone = new LinkedHashMap<>(keyToAdditional);
            
            reset(); // stops all tones and resets lists
            
            for (Keyboard.Key key : keyMapClone.keySet()) // restore intervals having played
                noteOn(key);
        }
        
        @Override
        protected void reset() {
            super.reset();
            keyToAdditional.clear();
        }
        
        @Override
        protected void noteOn(Keyboard.Key key) {
            super.noteOn(key);
            
            if (noAdditionalRequired() == false) {
                final Object additional = additionalFromKeyboard(key);
                if (additional != null) {
                    keyToAdditional.put(key, additional);
                    playAdditional(additional);
                }
            }
        }

        protected boolean noAdditionalRequired() {
            return selectedSemitones() <= 0;
        }
        protected Object additionalFromKeyboard(Keyboard.Key key) {
            final List<Keyboard.Key> keys = piano.getKeys(); // sorted by MIDI-number
            final int keyIndex = findKeyboardIndex(key.midiNoteNumber, keys);
            final int additionalKeyIndex = keyIndex + selectedSemitones();
            return (additionalKeyIndex < keys.size()) ? keys.get(additionalKeyIndex) : null;
        }
        protected final int findKeyboardIndex(int midiNoteNumber, List<Keyboard.Key> keys) {
            final int lowestMidiNumber = keys.get(0).midiNoteNumber;
            return midiNoteNumber - lowestMidiNumber;
        }
        protected void playAdditional(Object additional) {
            final Keyboard.Key key = (Keyboard.Key) additional;
            super.noteOn(key); // recursive call
            if (isHoldActive() == false)
                visualSelect(key, true);
        }
        

        @Override
        protected void noteOff(Keyboard.Key key) {
            final Keyboard.Key leadKey = findLeadKeyForAdditional(key); // null when key is leadKey
            if (leadKey != null)
                key = leadKey;
            final Object toTurnOff = keyToAdditional.get(key);
                
            keyToAdditional.remove(key);
            turnOffKey(key);
            
            if (toTurnOff != null) // set off additional when not recursive call
                turnOff(toTurnOff);
        }

        protected final void turnOffKey(Keyboard.Key key) {
            final boolean isInNoOtherPlayingInterval =
                    keyToAdditional.containsKey(key) == false &&
                    findLeadKeyForAdditional(key) == null;
            
            if (isInNoOtherPlayingInterval) {
                keyToAdditional.remove(key);
                super.noteOff(key);
            }
        }
        protected void turnOff(Object toTurnOff) {
            final Keyboard.Key key = (Keyboard.Key) toTurnOff;
            turnOffKey(key);
            visualSelect(key, false);
        }
        protected final Keyboard.Key findLeadKeyForAdditional(Keyboard.Key additional) {
            return keyToAdditional.entrySet().stream()
                    .filter(entry -> isAdditional(additional, entry.getValue()))
                    .map(entry -> entry.getKey())
                    .findFirst()
                    .orElse(null);
        }
        protected boolean isAdditional(Keyboard.Key key, Object mapValue) {
            return key == mapValue;
        }

        private int selectedSemitones() {
            final IntervalPlayingPiano myPiano = (IntervalPlayingPiano) piano;
            return myPiano.intervalChoice.isEnabled() ? myPiano.selectedSemitoneSteps : 0;
        }
    }   // end MouseHandler
}