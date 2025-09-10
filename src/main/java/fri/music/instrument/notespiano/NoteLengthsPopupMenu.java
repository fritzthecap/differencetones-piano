package fri.music.instrument.notespiano;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import fri.music.player.Note;

/** Lets choose a note length. */
public abstract class NoteLengthsPopupMenu extends JPopupMenu
{
    public NoteLengthsPopupMenu() {
        final ActionListener menuListener = new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                noteLengthWasSelected(((JMenuItem) e.getSource()).getActionCommand());
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

    /**
     * Called when user chooses a note length from this popup-menu.
     * @param noteLength the chosen note length.
     */
    protected abstract void noteLengthWasSelected(String noteLength);
}