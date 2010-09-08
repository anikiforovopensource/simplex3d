/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package visual.math.draw

object Launcher {
  def launch(function: Function) {
    java.awt.EventQueue.invokeLater(new Runnable() {
      def run() {
        val frame = new DrawFrame()
        frame.getDrawPanel().setPainter(FunctionPainter(function))
        frame.setVisible(true)
      }
    })
  }
}
