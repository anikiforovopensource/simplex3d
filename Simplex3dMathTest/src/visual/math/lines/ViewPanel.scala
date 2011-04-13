/*
 * Simplex3d, MathTest package
 * Copyright (C) 2009-2011, Aleksey Nikiforov
 *
 * This file is part of Simplex3dMathTest.
 *
 * Simplex3dMathTest is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dMathTest is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package visual.math.lines

import java.awt._
import java.awt.event._
import javax.swing._

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._


/**
 * @author Aleksey Nikiforov (lex)
 */
class ViewPanel extends JPanel with ActionListener {

  // Adjustable settings.
  var drawFps = true
  val lineWidth = 2
  val lineColor = ConstVec3(0, 1, 0)

  // Private fields.
  private val fpsTimer: FpsTimer = new FpsTimer()
  private var painter: (
    (inVec2, inVec2) => Unit,
    inVec2i,
    Double, Double
  ) => Unit = _

  // Timer to generates ticks.
  private val timer: Timer = new Timer(1, this)
  timer.start()

  // Java2D settings.
  val stroke = new BasicStroke(
    lineWidth, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND
  )
  val renderHints = new RenderingHints(
    RenderingHints.KEY_ANTIALIASING,
    RenderingHints.VALUE_ANTIALIAS_ON
  )
  renderHints.put(
    RenderingHints.KEY_RENDERING,
    RenderingHints.VALUE_RENDER_QUALITY
  )

  // Rendering method.
  override def paint(g: Graphics) {
    fpsTimer.update()
    val viewportDim = ConstVec2i(getWidth(), getHeight())

    // Clear the framebuffer.
    g.setColor(Color.BLACK)
    g.fillRect(0, 0, viewportDim.x, viewportDim.y)
    g.setColor(new Color(Float(lineColor.r), Float(lineColor.g), Float(lineColor.b)))

    // Render.
    if (painter != null) {

      val g2 = g.asInstanceOf[Graphics2D]
      g2.setStroke(stroke)
      g2.setRenderingHints(renderHints)
      val drawLine = (start: inVec2, end: inVec2) => {
        g2.drawLine(
            start.x.toInt, start.y.toInt,
            end.x.toInt, end.y.toInt
          )
      }

      painter.apply(drawLine, viewportDim, fpsTimer.uptime, fpsTimer.tpf)
    }

    if (drawFps) {
      val fpsLabel = String.valueOf(fpsTimer.fps.toInt)

      g.setColor(Color.LIGHT_GRAY)
      g.fillRect(9, 6, 32, 16)
      g.setColor(Color.BLACK)

      val bold = new Font("Monospaced", Font.BOLD, 16)
      g.setFont(bold)
      g.setColor(Color.BLACK)
      g.drawString(fpsLabel, 10, 20)
    }
  }

  def actionPerformed(e: ActionEvent) {
    repaint()
  }
}

object ViewPanel {
  def launch(
    painter: (
      (inVec2, inVec2) => Unit,
      inVec2i,
      Double, Double
    ) => Unit
  ) {
    java.awt.EventQueue.invokeLater(new Runnable() {
      def run() {
        val frame = new Software3dFrame()
        frame.getViewPanel().painter = painter
        frame.setVisible(true)
      }
    });
  }
}
