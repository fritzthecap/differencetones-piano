package fri.music.swingutils;

import java.awt.event.ActionEvent;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.KeyStroke;
import javax.swing.event.UndoableEditEvent;
import javax.swing.event.UndoableEditListener;
import javax.swing.text.JTextComponent;
import javax.swing.text.Keymap;
import javax.swing.undo.CannotRedoException;
import javax.swing.undo.CannotUndoException;
import javax.swing.undo.UndoManager;

public class TextAreaUtil
{
    /**
     * Adds an UndoManager and key bindings Ctrl-Z (undo) and Ctrl-Y (redo) to given textarea.
     * @param textComponent the textarea to add an UndoManager to.
     * @return the two created actions (undo and redo) that can be used for buttons.
     */
    public static Action[] addUndoManagement(JTextComponent textComponent)   {
        final UndoManager undoManager = new UndoManager();

        textComponent.getDocument().addUndoableEditListener(new UndoableEditListener()   {
            @Override
            public void undoableEditHappened(UndoableEditEvent e) {
                undoManager.addEdit(e.getEdit());
            }
        });

        final Keymap keymap = JTextComponent.addKeymap("FriUndoBindings", textComponent.getKeymap());

        final Action undo = new AbstractAction("Undo")  {
            @Override
            public void actionPerformed(ActionEvent e)  {
                try { undoManager.undo(); } catch (CannotUndoException ex)  {}
            }
        };
        KeyStroke key = KeyStroke.getKeyStroke(KeyEvent.VK_Z, InputEvent.CTRL_DOWN_MASK);
        keymap.addActionForKeyStroke(key, undo);

        final Action redo = new AbstractAction("Redo")  {
            @Override
            public void actionPerformed(ActionEvent e)  {
                try { undoManager.redo(); } catch (CannotRedoException ex)  {}
            }
        };
        key = KeyStroke.getKeyStroke(KeyEvent.VK_Y, InputEvent.CTRL_DOWN_MASK);
        keymap.addActionForKeyStroke(key, redo);

        textComponent.setKeymap(keymap);
        
        return new Action[] { undo, redo };
    }
}