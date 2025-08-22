package fri.music.player;

import java.util.Arrays;

/**
 * Functionality for Note that can not be shared through inheritance.
 */
public class NotesUtil
{
    /**
     * Converts given notes to a single-voiced melody.
     * @param notesArray the 2-dimensional array to be converted to a 1-dimensional array.
     * @return a Note array created from given array.
     * @throws IllegalArgumentException when a chord is contained in given NotesArray. 
     */
    public static Note[] toSingleNotesArray(Note[][] notesArray) {
        final Note[] result = new Note[notesArray.length];
        for (int i = 0; i < notesArray.length; i++) {
            final Note[] chord = notesArray[i];
            if (chord.length > 1)
                throw new IllegalArgumentException("Can not convert a chord to a single note: "+Arrays.toString(chord));
            
            result[i] = chord[0];
        }
        return result;
    }
}