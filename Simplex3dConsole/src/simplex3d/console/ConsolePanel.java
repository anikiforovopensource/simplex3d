/*
 * Simplex3dConsole
 * Copyright (C) 2011, Aleksey Nikiforov
 *
 * This file is part of Simplex3dConsole.
 *
 * Simplex3dConsole is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dConsole is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.console;

import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.util.prefs.Preferences;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.InputMap;
import javax.swing.JComponent;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;
import javax.swing.JTextArea;
import javax.swing.KeyStroke;
import javax.swing.SwingWorker;
import javax.swing.UIManager;
import simplex3d.console.findreplace.*;
import org.fife.ui.rsyntaxtextarea.RSyntaxTextArea;
import org.fife.ui.rsyntaxtextarea.SyntaxConstants;
import org.fife.ui.rtextarea.RTextScrollPane;


/**
 * @author Aleksey Nikiforov (lex)
 */
public class ConsolePanel extends javax.swing.JPanel {

    private RSyntaxTextArea textComponent;
    private final AbstractAction runAction;
    private final AbstractAction resetInterpreterAction;
    private SimplexInterpreter interpreter;


    public ConsolePanel() {
        initComponents();
        
        Preferences prefs = Preferences.userNodeForPackage(this.getClass());

        textComponent = new RSyntaxTextArea();
        FindReplacePanel findReplace = new FindReplacePanel(
            prefs,
            textComponent, new RTextScrollPane(),
            JComponent.WHEN_IN_FOCUSED_WINDOW
        );

        textComponent.setTextAntiAliasHint("VALUE_TEXT_ANTIALIAS_LCD_HRGB");
        textComponent.setSyntaxEditingStyle(SyntaxConstants.SYNTAX_STYLE_SCALA);
        textComponent.setTabsEmulated(true);
        textComponent.setTabSize(2);

        // Setup undo.
        findReplace.setUndoForwards(
            RSyntaxTextArea.getAction(RSyntaxTextArea.UNDO_ACTION),
            RSyntaxTextArea.getAction(RSyntaxTextArea.REDO_ACTION)
        );

        JPopupMenu menu = textComponent.getPopupMenu();
        menu.remove(0);
        menu.remove(0);
        menu.insert(findReplace.getRedoAction(), 0);
        menu.insert(findReplace.getUndoAction(), 0);
        menu.addSeparator();
        menu.add(new JMenuItem(findReplace.getShowFindDialogAction()));
        menu.add(new JMenuItem(findReplace.getShowReplaceDialogAction()));

        InputMap map = (InputMap) UIManager.get("RSyntaxTextAreaUI.inputMap");
        int mods = Toolkit.getDefaultToolkit().getMenuShortcutKeyMask();
        map.remove(KeyStroke.getKeyStroke(KeyEvent.VK_Z, mods));
        map.remove(KeyStroke.getKeyStroke(KeyEvent.VK_Y, mods));

        // Set the default example.
        textComponent.setText(Examples.getExample("scala/Greeting.scala"));
        textComponent.getCaret().setDot(0);

        // Add the find/replace panel.
        editorPanel.add(findReplace, java.awt.BorderLayout.CENTER);


        // Setup actions.
        runAction = new AbstractAction("Run") {
            {
                putValue(ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, InputEvent.CTRL_MASK));
                putValue(MNEMONIC_KEY, (int) 'R');
                putValue(ACTION_COMMAND_KEY, "runCmd");
            }

            public void actionPerformed(ActionEvent e) {
                runAction.setEnabled(false);

                new SwingWorker() {
                    @Override protected String doInBackground() throws Exception {
                        String res = interpreter.interpret(textComponent.getText());
                        return res;
                    }

                    @Override protected void done() {
                        try {
                            feedTextArea.setText((String) get());
                            runAction.setEnabled(true);
                        } catch (Exception e) {
                            String error = "EXCEPTION:\n" + e.toString();
                            for (StackTraceElement st : e.getStackTrace()) {
                                error += "\n" + st.toString();
                            }
                            feedTextArea.setText(error);
                            runAction.setEnabled(true);
                        }
                    }
                }.execute();
            }
        };

        resetInterpreterAction = new AbstractAction("Reset Interpreter") {
            {
                putValue(MNEMONIC_KEY, (int) 'I');
                putValue(ACTION_COMMAND_KEY, "resetInterpreterCmd");
            }

            public void actionPerformed(ActionEvent e) {
                runAction.setEnabled(false);
                resetInterpreterAction.setEnabled(false);
                feedTextArea.setText("");

                new SwingWorker() {
                    @Override protected String doInBackground() throws Exception {
                        interpreter.reset();
                        return null;
                    }

                    @Override protected void done() {
                        runAction.setEnabled(true);
                        resetInterpreterAction.setEnabled(true);
                    }
                }.execute();
            }
        };

        runButton.setAction(runAction);
        resetInterpreterButton.setAction(resetInterpreterAction);

        Utils.setSandboxEnabled(true);
    }

    
    public void takeFocus() {
        textComponent.requestFocus();
    }

    public final void setSandboxEnabled(boolean enabled) {
        if (!enabled) {
            String[] options = new String[]{ "Cancel", "Disable", "No", "Yes" };

            int selection = JOptionPane.showOptionDialog(
                this,
                "Turning off the sandbox mode will give the code from\n" +
                "the editor a full access to your system.\n\n" +
                "Are you sure you want to do this?\n",
                "Warning: turning off the sandbox mode!",
                JOptionPane.DEFAULT_OPTION, JOptionPane.WARNING_MESSAGE, null,
                options, "Cancel"
            );

            if ("Disable".equals(options[selection])) {
                Utils.setSandboxEnabled(enabled);
            }
        }
        else {
            JOptionPane.showMessageDialog(
                this,
                "The sandbox mode has been enabled.",
                "Info: the sandbox mode is enabled.", JOptionPane.INFORMATION_MESSAGE
            );
            Utils.setSandboxEnabled(true);
        }
    }

    public SimpleInterpreter getInterpreter() {
        return interpreter;
    }

    public void setInterpreter(SimplexInterpreter interpreter) {
        if (this.interpreter != null) this.interpreter.dispose();
        this.interpreter = interpreter;
    }

    public JTextArea getTextComponent() {
        return textComponent;
    }

    public Action getRunAction() {
        return runAction;
    }

    public Action getResetInterpreterAction() {
        return resetInterpreterAction;
    }


    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        buttonPanel = new javax.swing.JPanel();
        runButton = new javax.swing.JButton();
        clearEditorButton = new javax.swing.JButton();
        resetInterpreterButton = new javax.swing.JButton();
        splitPane = new javax.swing.JSplitPane();
        editorPanel = new javax.swing.JPanel();
        feedPanel = new javax.swing.JPanel();
        feedScrollPane = new javax.swing.JScrollPane();
        feedTextArea = new javax.swing.JTextArea();

        setLayout(new java.awt.BorderLayout());

        runButton.setText("Run");

        clearEditorButton.setText("Clear Editor");
        clearEditorButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                clearEditorButtonActionPerformed(evt);
            }
        });

        resetInterpreterButton.setText("Reset Interpreter");

        javax.swing.GroupLayout buttonPanelLayout = new javax.swing.GroupLayout(buttonPanel);
        buttonPanel.setLayout(buttonPanelLayout);
        buttonPanelLayout.setHorizontalGroup(
            buttonPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, buttonPanelLayout.createSequentialGroup()
                .addComponent(runButton)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 358, Short.MAX_VALUE)
                .addComponent(clearEditorButton)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(resetInterpreterButton))
        );
        buttonPanelLayout.setVerticalGroup(
            buttonPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(buttonPanelLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(buttonPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(resetInterpreterButton)
                    .addComponent(runButton)
                    .addComponent(clearEditorButton))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        add(buttonPanel, java.awt.BorderLayout.PAGE_START);

        splitPane.setDividerLocation(400);
        splitPane.setDividerSize(5);
        splitPane.setOrientation(javax.swing.JSplitPane.VERTICAL_SPLIT);
        splitPane.setResizeWeight(0.7);

        editorPanel.setLayout(new java.awt.BorderLayout());
        splitPane.setTopComponent(editorPanel);

        feedTextArea.setColumns(20);
        feedTextArea.setEditable(false);
        feedTextArea.setRows(5);
        feedScrollPane.setViewportView(feedTextArea);

        javax.swing.GroupLayout feedPanelLayout = new javax.swing.GroupLayout(feedPanel);
        feedPanel.setLayout(feedPanelLayout);
        feedPanelLayout.setHorizontalGroup(
            feedPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(feedScrollPane, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, 651, Short.MAX_VALUE)
        );
        feedPanelLayout.setVerticalGroup(
            feedPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(feedScrollPane, javax.swing.GroupLayout.DEFAULT_SIZE, 82, Short.MAX_VALUE)
        );

        splitPane.setRightComponent(feedPanel);

        add(splitPane, java.awt.BorderLayout.CENTER);
    }// </editor-fold>//GEN-END:initComponents

    private void clearEditorButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_clearEditorButtonActionPerformed
        textComponent.setText("");
        textComponent.requestFocus();
    }//GEN-LAST:event_clearEditorButtonActionPerformed


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JPanel buttonPanel;
    private javax.swing.JButton clearEditorButton;
    private javax.swing.JPanel editorPanel;
    private javax.swing.JPanel feedPanel;
    private javax.swing.JScrollPane feedScrollPane;
    private javax.swing.JTextArea feedTextArea;
    private javax.swing.JButton resetInterpreterButton;
    private javax.swing.JButton runButton;
    private javax.swing.JSplitPane splitPane;
    // End of variables declaration//GEN-END:variables

}
