package fri.music.instrument.notespiano;

import java.awt.Component;
import java.awt.event.MouseEvent;
import java.util.List;
import fri.music.SoundChannel;
import fri.music.instrument.PianoWithSound;
import fri.music.swingutils.ButtonUtil;

/**
 * Used as <code>SoundChannel</code> for <code>Player</code>, 
 * connects notes to the piano's keys.
 * That way <code>NotesPiano</code> never accesses the piano's
 * <code>SoundChannel</code> directly, instead it always plays
 * tones using the piano keyboard.
 */
class PianoKeyConnector implements SoundChannel
{
    private final PianoWithSound.MouseHandler mouseHandler;
    private final List<PianoWithSound.Keyboard.Key> keys;
    private final int lowestMidiNumber;
    
    PianoKeyConnector(PianoWithSound piano) {
        this.mouseHandler = piano.getMouseHandler();
        this.keys = piano.getKeys();
        this.lowestMidiNumber = keys.get(0).midiNoteNumber;
    }
    
    @Override
    public void noteOn(int midiNoteNumber, int velocity) {
        final PianoWithSound.Keyboard.Key key = findKey(midiNoteNumber);
        //velocityChange(velocity);
        pressOrReleaseKey(key, true);
    }
    @Override
    public void noteOff(int midiNoteNumber) {
        final PianoWithSound.Keyboard.Key key = findKey(midiNoteNumber);
        pressOrReleaseKey(key, false);
    }
    
    @Override
    public void volumeChange(int volume) {
    }
    @Override
    public void allNotesOff() {
    }
    
    
    private PianoWithSound.Keyboard.Key findKey(int midiNoteNumber) {
        return keys.get(midiNoteNumber - lowestMidiNumber);
    }
    
    private void pressOrReleaseKey(PianoWithSound.Keyboard.Key key, boolean press) {
        if (press) {
            ButtonUtil.press(key); // this was the only working way to visibly show key-presses
            mouseHandler.mousePressed(createMouseEvent(key, MouseEvent.MOUSE_PRESSED));
        }
        else { // release
            ButtonUtil.release(key);
            mouseHandler.mouseReleased(createMouseEvent(key, MouseEvent.MOUSE_RELEASED));
        }
    }
    
    private MouseEvent createMouseEvent(Component eventSource, int mouseEventId) {
        return new MouseEvent(
                eventSource, // where the event occurred
                mouseEventId, // MouseEvent.MOUSE_XXX
                System.currentTimeMillis(), // when
                0, // modifiers
                2, 2, /// x, y coordinates
                1, // click count
                false, // popup trigger
                MouseEvent.BUTTON1); // which button of mouse: left
    }
}