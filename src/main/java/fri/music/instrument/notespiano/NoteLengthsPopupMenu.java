package fri.music.instrument.notespiano;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import fri.music.player.Note;
import fri.music.player.notelanguage.MelodyFactory;

/** Lets choose a note length. */
public abstract class NoteLengthsPopupMenu extends JPopupMenu
{
    private String currentlySelectedLength = MelodyFactory.DEFAULT_NOTE_LENGTH;
    
    public NoteLengthsPopupMenu() {
        final ActionListener menuListener = new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                currentlySelectedLength = ((JMenuItem) e.getSource()).getActionCommand();
                noteLengthWasSelected(currentlySelectedLength);
            }
        };
        for (int i = 1; i <= Note.SHORTEST_NOTELENGTH_DIVISOR; i *= 2) { // 1, 2, 4, 8, 16, 32, 64
            final String actionCommand = Integer.toString(i);
            final JMenuItem menuItem = new JMenuItem(actionCommand);
            menuItem.setToolTipText("1/"+actionCommand+" Note");
            menuItem.addActionListener(menuListener);
            add(menuItem);
        }
    }
    
    /** @return the most recently selected length item from popup menu. */
    public String getCurrentlySelectedLength() {
        return currentlySelectedLength;
    }

    /**
     * Called when user chooses a note length from this popup-menu.
     * @param noteLength the chosen note length.
     */
    public abstract void noteLengthWasSelected(String noteLength);
}