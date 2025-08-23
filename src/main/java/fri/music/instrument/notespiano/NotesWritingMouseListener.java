package fri.music.instrument.notespiano;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import fri.music.instrument.PianoWithSound;
import fri.music.player.Note;
import fri.music.swingutils.MouseKeyAdapter;

/**
 * Writes notes from piano keyboard to text-area.
 * Call <code>setActive(true)</code> to make it work, initially it is not active.
 */
public class NotesWritingMouseListener extends MouseKeyAdapter
{
    private final NotesPianoPlayer notesPianoPlayer;
    private final JPopupMenu popup;
    
    private boolean active;
    private PianoWithSound.Keyboard.Key key;
    private long startMillis;
    
    public NotesWritingMouseListener(NotesPianoPlayer notesPianoPlayer) {
        this.notesPianoPlayer = notesPianoPlayer;
        
        this.popup = new JPopupMenu();
        final ActionListener menuListener = new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                noteLengthSelected(((JMenuItem) e.getSource()).getActionCommand());
            }
        };
        for (int i = 1; i <= Note.SHORTEST_NOTELENGTH_DIVISOR; i *= 2) { // 1, 2, 4, 8, 16, 32, 64
            final String actionCommand = Integer.toString(i);
            final JMenuItem menuItem = new JMenuItem(actionCommand);
            menuItem.setToolTipText("1/"+actionCommand+" Note");
            menuItem.addActionListener(menuListener);
            popup.add(menuItem);
        }
    }
    
    /** Turns this mouse listener on and off. Initially it is NOT active. */
    public void setActive(boolean active) {
        this.active = active;
    }
    
    protected final boolean isActive() {
        return active;
    }
    
    // mouse listener methods
    
    @Override
    public void mousePressed(MouseEvent e) {
        key = getKey(e);
        
        if (showContextMenu(e) == false) // is left mouse button
            startMillis = System.currentTimeMillis();
    }
    
    @Override
    public void mouseReleased(MouseEvent e) {
        if (key == null || popup.isShowing()) // was a mouse drag
            return;
        
        if (showContextMenu(e) == false) // is left mouse button
            calculateAndWriteNote();
    }
    
    @Override
    public void mouseEntered(MouseEvent e) {
        if (key != getKey(e))
            key = null; // ignore mouse drags
    }
    
    
    /** Context menu callback. */
    private void noteLengthSelected(String noteLength) {
        if (key == null)
            return; // some wrong state

        final String noteString = writeNoteToTextarea(noteLength);
        if (noteString != null)
            notesPianoPlayer.playSingleNote(noteString);
    }
    
    /**
     * Recommended way to display a context-menu <b>platform-independently</b>
     * is to do it on both mouse-press and mouse-release events.
     */
    private boolean showContextMenu(MouseEvent e) {
        final boolean isPopupEvent = (active && e.isPopupTrigger());
        if (isPopupEvent)
            popup.show(getKey(e), e.getX(), e.getY());
        return isPopupEvent;
    }

    private void calculateAndWriteNote() {
        final long endMillis = System.currentTimeMillis();
        final double SHRINK_FACTOR = 0.8; // this makes it easier to achieve 1/16 notes
        final long durationMillis = Math.round(SHRINK_FACTOR * (double) (endMillis - startMillis));
        // calculate noteLengthDivisor from durationMillis
        final String noteLength = notesPianoPlayer.noteLengthForMillis((int) durationMillis);
        
        writeNoteToTextarea(noteLength);
    }

    /** Writes clicked key plus selected or measured note-length to notesPianoPlayer when active, else returns null. */
    protected String writeNoteToTextarea(String noteLength) {
        if (active == false)
            return null;
        
        final String noteWithLength = key.ipnName + Note.DURATION_SEPARATOR + noteLength;
        notesPianoPlayer.writeSingleNote(notesPianoPlayer.view(), noteWithLength);
        return noteWithLength;
    }

    private PianoWithSound.Keyboard.Key getKey(MouseEvent e) {
        return (PianoWithSound.Keyboard.Key) e.getSource();
    }
}