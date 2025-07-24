package fri.music.instrument.notespiano;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import fri.music.instrument.PianoWithSound;
import fri.music.player.Note;
import fri.music.player.notelanguage.MelodyFactory;

/**
 * Writes notes from piano keyboard to text-area.
 */
class NotesWritingMouseListener extends MouseAdapter
{
    private final NotesPianoPlayer notesPiano;
    private final JPopupMenu popup;
    
    private boolean active;
    private PianoWithSound.Keyboard.Key key;
    private long startMillis;
    
    NotesWritingMouseListener(NotesPianoPlayer notesPiano) {
        this.notesPiano = notesPiano;
        
        this.popup = new JPopupMenu();
        final ActionListener menuListener = new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                if (key == null)
                    return;

                final String noteLength = ((JMenuItem) e.getSource()).getActionCommand();
                final String noteString = writeNoteToTextarea(noteLength);
                notesPiano.playSingleNote(noteString);
            }
        };
        for (int i = 1; i <= MelodyFactory.SHORTEST_NOTELENGTH_DIVISOR; i *= 2) { // 1, 2, 4, 8, 16, 32, 64
            final JMenuItem item = new JMenuItem(""+i);
            item.setToolTipText("1/"+i+" Note");
            item.addActionListener(menuListener);
            popup.add(item);
        }
    }
    
    /** Turns off and on this mouse listener. Initially it is active. */
    public void setActive(boolean active) {
        this.active = active;
    }
    
    // mouse listener methods
    
    @Override
    public void mousePressed(MouseEvent e) {
        if (active == false)
            return;
        
        key = getKey(e);
        
        if (showPopupMenu(e) == false)
            startMillis = System.currentTimeMillis();
    }
    
    @Override
    public void mouseReleased(MouseEvent e) {
        if (active == false || key == null || popup.isShowing()) // was a mouse drag
            return;
        
        if (showPopupMenu(e) == false)
            calculateAndWriteNote();
    }
    
    @Override
    public void mouseEntered(MouseEvent e) {
        if (active && key != getKey(e))
            key = null; // ignore mouse drags
    }
    
    
    /**
     * Recommended way to display a context-menu <b>platform-independently</b>
     * is to do it on both mouse-press and mouse-release events.
     */
    private boolean showPopupMenu(MouseEvent e) {
        final boolean isPopupEvent = e.isPopupTrigger();
        if (isPopupEvent)
            popup.show(getKey(e), e.getX(), e.getY());
        return isPopupEvent;
    }

    private void calculateAndWriteNote() {
        final long endMillis = System.currentTimeMillis();
        final double SHRINK_FACTOR = 0.8; // this makes it easier to achieve 1/16 notes
        final long durationMillis = Math.round(SHRINK_FACTOR * (double) (endMillis - startMillis));
        // calculate noteLengthDivisor from durationMillis
        final String noteLength = notesPiano.noteLengthForMillis((int) durationMillis);
        
        writeNoteToTextarea(noteLength);
    }

    private String writeNoteToTextarea(String noteLength) {
        final String noteWithLength = key.ipnName + Note.DURATION_SEPARATOR + noteLength;
        notesPiano.writeSingleNote(noteWithLength);
        return noteWithLength;
    }

    private PianoWithSound.Keyboard.Key getKey(MouseEvent e) {
        return (PianoWithSound.Keyboard.Key) e.getSource();
    }
}