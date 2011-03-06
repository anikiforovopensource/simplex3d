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

import java.awt.event.ActionEvent;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.KeyStroke;
import javax.swing.SwingWorker;
import org.fife.ui.rsyntaxtextarea.RSyntaxTextArea;
import org.fife.ui.rsyntaxtextarea.SyntaxConstants;
import org.fife.ui.rtextarea.RTextScrollPane;


/**
 * @author Aleksey Nikiforov (lex)
 */
public class MainPanel extends javax.swing.JPanel {

    private JScrollPane scrollPane;
    private RSyntaxTextArea textComponent;
    private final AbstractAction runAction;
    private final AbstractAction resetInterpreterAction;
    private SimplexInterpreter interpreter;


    public MainPanel() {
        initComponents();

        textComponent = new RSyntaxTextArea();
        scrollPane = new RTextScrollPane();
        scrollPane.setViewportView(textComponent);
        editorPanel.add(scrollPane, java.awt.BorderLayout.CENTER);

        textComponent.setTextAntiAliasHint("VALUE_TEXT_ANTIALIAS_LCD_HRGB");
        textComponent.setSyntaxEditingStyle(SyntaxConstants.SYNTAX_STYLE_SCALA);
        textComponent.setTabsEmulated(true);
        textComponent.setTabSize(2);

        textComponent.setText(Examples.getExample("scala/Greeting.scala"));


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
                            consoleTextArea.setText((String) get());
                            runAction.setEnabled(true);
                        } catch (Exception e) {
                            String error = "EXCEPTION:\n" + e.toString();
                            for (StackTraceElement st : e.getStackTrace()) {
                                error += "\n" + st.toString();
                            }
                            consoleTextArea.setText(error);
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
                consoleTextArea.setText("");

                new SwingWorker() {
                    @Override protected String doInBackground() throws Exception {
                        setInterpreter(new SimplexInterpreter());
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
    }

    public SimpleInterpreter getInterpreter() {
        return interpreter;
    }

    public void setInterpreter(SimplexInterpreter interpreter) {
        interpreter.dispose();
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
        consolePanel = new javax.swing.JPanel();
        consoleScrollPane = new javax.swing.JScrollPane();
        consoleTextArea = new javax.swing.JTextArea();

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
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 383, Short.MAX_VALUE)
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
        splitPane.setOrientation(javax.swing.JSplitPane.VERTICAL_SPLIT);
        splitPane.setResizeWeight(0.5);

        editorPanel.setLayout(new java.awt.BorderLayout());
        splitPane.setTopComponent(editorPanel);

        consoleTextArea.setColumns(20);
        consoleTextArea.setEditable(false);
        consoleTextArea.setRows(5);
        consoleScrollPane.setViewportView(consoleTextArea);

        javax.swing.GroupLayout consolePanelLayout = new javax.swing.GroupLayout(consolePanel);
        consolePanel.setLayout(consolePanelLayout);
        consolePanelLayout.setHorizontalGroup(
            consolePanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(consoleScrollPane, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, 676, Short.MAX_VALUE)
        );
        consolePanelLayout.setVerticalGroup(
            consolePanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(consoleScrollPane, javax.swing.GroupLayout.DEFAULT_SIZE, 156, Short.MAX_VALUE)
        );

        splitPane.setRightComponent(consolePanel);

        add(splitPane, java.awt.BorderLayout.CENTER);
    }// </editor-fold>//GEN-END:initComponents

    private void clearEditorButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_clearEditorButtonActionPerformed
        textComponent.setText("");
    }//GEN-LAST:event_clearEditorButtonActionPerformed


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JPanel buttonPanel;
    private javax.swing.JButton clearEditorButton;
    private javax.swing.JPanel consolePanel;
    private javax.swing.JScrollPane consoleScrollPane;
    private javax.swing.JTextArea consoleTextArea;
    private javax.swing.JPanel editorPanel;
    private javax.swing.JButton resetInterpreterButton;
    private javax.swing.JButton runButton;
    private javax.swing.JSplitPane splitPane;
    // End of variables declaration//GEN-END:variables

}
