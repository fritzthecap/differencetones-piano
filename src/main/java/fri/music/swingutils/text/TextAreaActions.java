package fri.music.swingutils.text;

import java.awt.Font;
import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.DataFlavor;
import java.awt.event.ActionEvent;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ActionMap;
import javax.swing.JComponent;
import javax.swing.JMenu;
import javax.swing.event.UndoableEditEvent;
import javax.swing.event.UndoableEditListener;
import javax.swing.text.DefaultEditorKit;
import javax.swing.text.JTextComponent;
import javax.swing.undo.CannotRedoException;
import javax.swing.undo.CannotUndoException;
import javax.swing.undo.UndoManager;
import fri.music.swingutils.KeyStrokeUtil;

/**
 * Text-area or text-field with cut/copy/paste/clear/undo/redo actions
 * and according key-bindings. Constructor installs
 * itself as mouse-listener to given text-component.
 * Usage:
 * <pre>
 *   JTextArea textArea = new JTextArea();
 *   TextAreaActions textAreaActions = new TextAreaActions(textArea);
 * </pre>
 */
public class TextAreaActions extends FontActions
{
    private final Action cut;
    private final Action copy;
    private final Action paste;
    private final Action undo;
    private final Action redo;
    private final Action selectAll;
    private final Action clear;
    private final UndoManager undoManager;
    
    private float fontSize;
    
    /**
     * Attaches standard actions to given text-component.
     * @param textComponent the JTextArea or JTextField to add actions to.
     */
    public TextAreaActions(final JTextComponent textComponent) {
        super(textComponent);
        
        final ActionMap actionMap = textComponent.getActionMap();
        
        this.cut = actionMap.get(DefaultEditorKit.cutAction);
        cut.putValue(Action.NAME, "Cut (Ctrl-X)");
        this.copy = actionMap.get(DefaultEditorKit.copyAction);
        copy.putValue(Action.NAME, "Copy (Ctrl-C)");
        this.paste = actionMap.get(DefaultEditorKit.pasteAction);
        paste.putValue(Action.NAME, "Paste (Ctrl-V)");
        this.selectAll = actionMap.get(DefaultEditorKit.selectAllAction);
        selectAll.putValue(Action.NAME, "Select (Ctrl-A)");
        
        this.undoManager = new UndoManager();
        undoManager.setLimit(300);
        final Action[] undoRedo = addUndoManagement(textComponent, undoManager);
        this.undo = undoRedo[0]; // Ctrl-z
        this.redo = undoRedo[1]; // Ctrl-y
        
        this.clear = new AbstractAction() { // erase all text
            @Override
            public void actionPerformed(ActionEvent e) {
                textComponent.setText("");
            }
        };
        clear.putValue(Action.NAME, "Clear");
        
        final JMenu fontMenu = buildFontMenu(textComponent);
        
        contextMenu.add(cut);
        contextMenu.add(copy);
        contextMenu.add(paste);
        contextMenu.addSeparator();
        contextMenu.add(undo);
        contextMenu.add(redo);
        contextMenu.addSeparator();
        contextMenu.add(selectAll);
        contextMenu.add(clear);
        contextMenu.addSeparator();
        contextMenu.add(fontMenu);
        
        enableActions(textComponent);
    }
    
    @Override
    public void magnifyFont(boolean bigger, JTextComponent textComponent) {
        final Font font = textComponent.getFont();
        if (fontSize <= 0)
            fontSize = font.getSize2D();
        else if (fontSize <= 8 && bigger == false || fontSize >= 28 && bigger == true)
            return; // deny smaller or bigger fonts
        
        fontSize += bigger ? +1 : -1;
        final Font newFont = font.deriveFont(fontSize);
        textComponent.setFont(newFont);
    }
    
    @Override
    protected void enableActions(JTextComponent textComponent) {
        final boolean textIsSelected = (textComponent.getSelectionStart() != textComponent.getSelectionEnd());
        cut.setEnabled(textIsSelected);
        copy.setEnabled(textIsSelected);
        
        final Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
        try {
            final Object data = clipboard.getData(DataFlavor.stringFlavor);
            paste.setEnabled(data != null);
        }
        catch (Exception e) {
            paste.setEnabled(false);
        }
        
        undo.setEnabled(undoManager.canUndo());
        redo.setEnabled(undoManager.canRedo());
        
        final boolean textExists = (textComponent.getDocument().getLength() > 0);
        clear.setEnabled(textExists);
        selectAll.setEnabled(textExists == true && textIsSelected == false);
    }
    
    
    /**
     * Adds an UndoManager and key bindings Ctrl-Z (undo) and Ctrl-Y (redo) to given textarea.
     * @return the two created actions (undo and redo) that can be used for buttons.
     */
    private Action[] addUndoManagement(JTextComponent textComponent, UndoManager undoManager)   {
        // listen to document changes to add edits
        textComponent.getDocument().addUndoableEditListener(new UndoableEditListener()   {
            @Override
            public void undoableEditHappened(UndoableEditEvent e) {
                undoManager.addEdit(e.getEdit());
            }
        });

        // build undo action
        final Action undo = new AbstractAction("Undo (Ctrl-Z)")  {
            @Override
            public void actionPerformed(ActionEvent e)  {
                try { undoManager.undo(); } catch (CannotUndoException ex)  {}
            }
        };
        KeyStrokeUtil.install(
                textComponent, 
                JComponent.WHEN_FOCUSED,
                "magnifyFont", 
                KeyEvent.VK_Z,
                InputEvent.CTRL_DOWN_MASK,
                undo);

        // build redo action
        final Action redo = new AbstractAction("Redo (Ctrl-Y)")  {
            @Override
            public void actionPerformed(ActionEvent e)  {
                try { undoManager.redo(); } catch (CannotRedoException ex)  {}
            }
        };
        KeyStrokeUtil.install(
                textComponent, 
                JComponent.WHEN_FOCUSED,
                "magnifyFont", 
                KeyEvent.VK_Y,
                InputEvent.CTRL_DOWN_MASK,
                redo);
        
        return new Action[] { undo, redo };
    }
}