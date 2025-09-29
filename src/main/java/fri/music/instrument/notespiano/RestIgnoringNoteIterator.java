package fri.music.instrument.notespiano;

import java.util.Iterator;
import fri.music.player.Note;

/**
 * Iterates over given notes ignoring rests and subsequent tied notes
 * (i.e. not the first tied note, but all that are tied to it).
 */
public class RestIgnoringNoteIterator implements Iterator<Note[]>
{
    private final Note[][] notes;
    private int index;
    
    public RestIgnoringNoteIterator(Note[][] notes) {
        this.notes = notes;
        if (notes != null)
            skipRests();
    }

    @Override
    public boolean hasNext() {
        return notes != null && index < notes.length;
    }
    
    @Override
    public Note[] next() {
        final Note[] next = notes[index];
        
        // START keep order
        skipTies(); // read away tied follower notes
        index++; // skip to next note
        skipRests(); // read away rests
        // END keep order

        return next;
    }
    
    private void skipTies() {
        while (index < notes.length && Boolean.TRUE.equals(notes[index][0].connectionFlags.tied()))
            index++; // FALSE would be the last tied note
    }
    private void skipRests() {
        while (index < notes.length && notes[index][0].isRest())
            index++; // being on note after rest
    }
}