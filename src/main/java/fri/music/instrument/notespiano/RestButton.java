package fri.music.instrument.notespiano;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.SwingUtilities;
import fri.music.ToneSystem;
import fri.music.player.Note;
import fri.music.player.notelanguage.MelodyFactory;
import fri.music.swingutils.SizeUtil;

public class RestButton extends JButton
{
    public interface Callback
    {
        void writeRest(String note);
    }
    
    private static final String REST_PREFIX = "\u2012 / "; // dash
    
    public RestButton(final Callback callback) {
        super(REST_PREFIX + MelodyFactory.DEFAULT_NOTE_LENGTH);
        setBackground(Color.WHITE);
        setFont(getFont().deriveFont(Font.BOLD, 16f));
        setBorder(BorderFactory.createLineBorder(Color.BLACK, 2, true));
        setToolTipText("Write Rest to Textarea at Cursor Position, Right Click for Length Choice");
        
        final NoteLengthsPopupMenu popupMenu = new NoteLengthsPopupMenu() {
            @Override
            public void noteLengthWasSelected(String noteLength) {
                setText(REST_PREFIX + noteLength);
                callback.writeRest(Note.toString(ToneSystem.REST_SYMBOL, noteLength));
            }
        };
        
        addMouseListener(new MouseAdapter() { // support both left and right mouse button
            @Override
            public void mousePressed(MouseEvent e) {
                if (e.isPopupTrigger()) // let choose length
                    popupMenu.show(RestButton.this, e.getX(), e.getY());
            }
            @Override
            public void mouseReleased(MouseEvent e) {
                if (SwingUtilities.isLeftMouseButton(e)) // deliver the most recently chosen length
                    popupMenu.noteLengthWasSelected(popupMenu.getCurrentlySelectedLength());
                else if (e.isPopupTrigger()) // let choose length
                    popupMenu.show(RestButton.this, e.getX(), e.getY());
            }
        });
        
        SizeUtil.forceSize(this, new Dimension(64, 32));
    }
}