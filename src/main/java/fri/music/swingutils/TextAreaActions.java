package fri.music.swingutils;

import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.DataFlavor;
import java.awt.event.ActionEvent;
import java.awt.event.InputEvent;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ActionMap;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JPopupMenu;
import javax.swing.KeyStroke;
import javax.swing.event.UndoableEditEvent;
import javax.swing.event.UndoableEditListener;
import javax.swing.text.DefaultEditorKit;
import javax.swing.text.JTextComponent;
import javax.swing.text.Keymap;
import javax.swing.undo.CannotRedoException;
import javax.swing.undo.CannotUndoException;
import javax.swing.undo.UndoManager;

/**
 * Text-area with cut/copy/paste/clear/undo/redo actions
 * and according key-bindings. Usage:
 * <pre>
 *     JTextArea textArea = new JTextArea();
 *     textArea.addMouseListener(new TextAreaActions(textArea));
 * </pre>
 */
public class TextAreaActions extends MouseAdapter
{
    private final Action cut;
    private final Action copy;
    private final Action paste;
    private final Action clear;
    private final Action undo;
    private final Action redo;
    private final Action selectAll;
    private final UndoManager undoManager;
    private final JPopupMenu contextMenu;
    
    /**
     * Attaches standard actions to given text-component.
     * This is a mouse- and key-listener to the component.
     * The component will not be stored here, it will be accessed
     * via <code>anyEvent.getSource()</code>.
     * @param textComponent the text-component to add actions to.
     */
    public TextAreaActions(JTextComponent textComponent) {
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
        
        this.contextMenu = new JPopupMenu();
        contextMenu.add(cut);
        contextMenu.add(copy);
        contextMenu.add(paste);
        contextMenu.addSeparator();
        contextMenu.add(undo);
        contextMenu.add(redo);
        contextMenu.addSeparator();
        contextMenu.add(selectAll);
        contextMenu.add(clear);
        
        // we need to listen to any key for enabling actions
        textComponent.addKeyListener(new KeyAdapter() {
            @Override
            public void keyReleased(KeyEvent e) {
                setActionsEnabled(textComponent);
            }
        });
        
        setActionsEnabled(textComponent);
    }
    
    @Override
    public void mousePressed(MouseEvent e) {
        showContextMenu(e);
    }
    
    @Override
    public void mouseReleased(MouseEvent e) {
        if (showContextMenu(e) == false)
            setActionsEnabled((JTextComponent) e.getSource());
            // listen here to text-selection via mouse-drag because mouseDragged() doesn't work
    }
    
    /**
     * Adds an UndoManager and key bindings Ctrl-Z (undo) and Ctrl-Y (redo) to given textarea.
     * @return the two created actions (undo and redo) that can be used for buttons.
     */
    private Action[] addUndoManagement(JTextComponent textComponent, UndoManager undoManager)   {
        textComponent.getDocument().addUndoableEditListener(new UndoableEditListener()   {
            @Override
            public void undoableEditHappened(UndoableEditEvent e) {
                undoManager.addEdit(e.getEdit());
            }
        });

        final Keymap keymap = JTextComponent.addKeymap("TextArea-Undo-Redo-Bindings", textComponent.getKeymap());

        final Action undo = new AbstractAction("Undo (Ctrl-Z)")  {
            @Override
            public void actionPerformed(ActionEvent e)  {
                try { undoManager.undo(); } catch (CannotUndoException ex)  {}
            }
        };
        KeyStroke key = KeyStroke.getKeyStroke(KeyEvent.VK_Z, InputEvent.CTRL_DOWN_MASK);
        keymap.addActionForKeyStroke(key, undo);

        final Action redo = new AbstractAction("Redo (Ctrl-Y)")  {
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
    
    private boolean showContextMenu(MouseEvent e) {
        final boolean isPopupEvent = e.isPopupTrigger();
        if (isPopupEvent) {
            setActionsEnabled((JTextComponent) e.getSource());
            contextMenu.show((JComponent) e.getSource(), e.getX(), e.getY());
        }
        return isPopupEvent;
    }
    
    private void setActionsEnabled(JTextComponent textComponent) {
        final boolean textIsSelected = textComponent.getSelectionStart() != textComponent.getSelectionEnd();
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


    public static void main(String[] args) {
        javax.swing.JFrame frame = new javax.swing.JFrame();
        javax.swing.JTextArea textArea = new javax.swing.JTextArea();
        textArea.setText("abc\ndef\nghi\njkl\nmno\npqr\nstu\nvwx\nyz");
        textArea.addMouseListener(new TextAreaActions(textArea));
        frame.getContentPane().add(new javax.swing.JScrollPane(textArea));
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setSize(new java.awt.Dimension(400, 300));
        frame.setLocationRelativeTo(null);
        frame.setVisible(true);
    }
}
