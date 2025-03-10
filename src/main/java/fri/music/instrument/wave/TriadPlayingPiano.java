package fri.music.instrument.wave;

import static fri.music.ToneSystem.*;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import fri.music.SoundChannel;
import fri.music.instrument.PianoWithSound;

/**
 * Piano that auto-plays a configured triad when pressing a key.
 */
public class TriadPlayingPiano extends IntervalPlayingPiano
{
    /** Key = chord name, value = the 2 semitone step numbers to the missing chord tones. */
    private static final Map<String,int[]> triadToSemitones = new LinkedHashMap<>();
    static {
        triadToSemitones.put("", new int[0]); // no triad
        triadToSemitones.put("Major Chord", new int[] { semitoneSteps(MAJOR_THIRD), semitoneSteps(FIFTH) });
        triadToSemitones.put("Minor Chord", new int[] { semitoneSteps(MINOR_THIRD), semitoneSteps(FIFTH) });
        triadToSemitones.put("Diminished Chord", new int[] { semitoneSteps(MINOR_THIRD), semitoneSteps(TRITONE) });
        triadToSemitones.put("Augmented Chord", new int[] { semitoneSteps(MAJOR_THIRD), semitoneSteps(MAJOR_THIRD), 8 });
        triadToSemitones.put("Major Chord 1st Inversion", new int[] { semitoneSteps(MINOR_THIRD), semitoneSteps(MINOR_SIXTH) });
        triadToSemitones.put("Minor Chord 1st Inversion", new int[] { semitoneSteps(MAJOR_THIRD), semitoneSteps(MAJOR_SIXTH) });
        triadToSemitones.put("Major Chord 2nd Inversion", new int[] { semitoneSteps(FOURTH), semitoneSteps(MAJOR_SIXTH) });
        triadToSemitones.put("Minor Chord 2nd Inversion", new int[] { semitoneSteps(FOURTH), semitoneSteps(MINOR_SIXTH) });
    }
    
    private int[] selectedSemitoneSteps = triadToSemitones.get("");
    
    public TriadPlayingPiano(PianoWithSound.Configuration config, SoundChannel channel) {
        super(config, channel);
    }
    
    @Override
    protected String intervalChooserLabel() {
        return "Autoplay Triad";
    }

    @Override
    protected String intervalChooserTooltip() {
        return "Tones of chosen triad will be added automatically when pressing a key";
    }

    @Override
    protected String[] intervalChooserItems() {
        return triadToSemitones.keySet().toArray(new String[triadToSemitones.size()]);
    }

    @Override
    protected void intervalChooserAction(String triadName) {
        selectedSemitoneSteps = triadToSemitones.get(triadName);
    }
    
    /** Overwritten to use AdditionalTonesPlayingMouseHandler, implementing "Intervals" choice. */
    @Override
    protected MouseHandler newMouseHandler() {
        return new TriadPlayingMouseHandler(this);
    }
    
    
    protected static class TriadPlayingMouseHandler extends AdditionalTonesPlayingMouseHandler
    {
        public TriadPlayingMouseHandler(TriadPlayingPiano piano) {
            super(piano);
        }
        
        @Override
        protected boolean noAdditionalRequired() {
            return selectedSemitones().length <= 0;
        }
        @Override
        protected Object additionalFromKeyboard(Keyboard.Key key) {
            final List<Keyboard.Key> keys = piano.getKeys(); // sorted by MIDI-number
            final int keyIndex = findKeyboardIndex(key.midiNoteNumber, keys);
            final List<Keyboard.Key> additionalKeys = new ArrayList<>();
            for (int semitoneSteps : selectedSemitones()) {
                final int additionalKeyIndex = keyIndex + semitoneSteps;
                if (additionalKeyIndex < keys.size())
                    additionalKeys.add(keys.get(additionalKeyIndex));
            }
            return additionalKeys;
        }
        @Override
        protected void playAdditional(Object additional) {
            @SuppressWarnings("unchecked")
            final List<Keyboard.Key> keyList = (List<Keyboard.Key>) additional;
            for (Keyboard.Key key : keyList)
                super.playAdditional(key);
        }

        @Override
        protected void turnOff(Object toTurnOff) {
            @SuppressWarnings("unchecked")
            final List<Keyboard.Key> keyList = (List<Keyboard.Key>) toTurnOff;
            for (Keyboard.Key keyToTurnOff : keyList)
                super.turnOff(keyToTurnOff);
        }
        @Override
        protected boolean isAdditional(Keyboard.Key key, Object mapValue) {
            @SuppressWarnings("unchecked")
            final List<Keyboard.Key> keyList = (List<Keyboard.Key>) mapValue;
            return keyList.contains(key);
        }
        
        private int[] selectedSemitones() {
            return ((TriadPlayingPiano) piano).selectedSemitoneSteps;
        }
    }   // end MouseHandler
}