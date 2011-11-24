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

package simplex3d.console
package script

import java.awt._
import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import java.awt.event.WindowAdapter
import java.awt.event.WindowEvent
import java.awt.image.BufferedImage
import javax.swing._
import simplex3d.math._
import simplex3d.math.double._
import simplex3d.data._
import simplex3d.data.double._
import conversion.Double._


/**
 * @author Aleksey Nikiforov (lex)
 */
object ImageUtils extends ImageUtils


class ImageUtils {

  val DefaultDims = ConstVec2i(640, 480)
  val DefaultBackground = ConstVec3(0)


  private[script] def rgb(c: inVec3) :Int = {
    ((toUByte(c.r) & 0xFF) << 16) | ((toUByte(c.g) & 0xFF) << 8) | ((toUByte(c.b) & 0xFF))
  }
  private[script] def rgb(j: Int, d: inContiguous[SInt, UByte]) :Int = {
    (d(j) << 16) | (d(j + 1) << 8) | (d(j + 2))
  }

  private[this] def mkImgRgbVec3(dims: inVec2i, data: inDataSeq[Vec3, Raw]) = {
    val img = new BufferedImage(dims.x, dims.y, BufferedImage.TYPE_INT_RGB)

    var y = 0; while (y < dims.y) {
      var x = 0; while (x < dims.x) {

        img.setRGB(x, y, rgb(data(y*dims.x + x)))

        x += 1
      }
      y += 1
    }

    img
  }
  private[this] def mkImgRgbUByte(dims: inVec2i, data: inDataSeq[_, UByte]) = {
    val img = new BufferedImage(dims.x, dims.y, BufferedImage.TYPE_INT_RGB)
    val p = ReadContiguous[SInt, UByte](data.primitives)

    var y = 0; while (y < dims.y) {
      var x = 0; while (x < dims.x) {

        val i = y*dims.x + x
        val j = i*data.stride + data.offset
        img.setRGB(x, y, rgb(j, p))

        x += 1
      }
      y += 1
    }

    img
  }

  private[this] def position(frame: JFrame) {
    val dimensions = Toolkit.getDefaultToolkit.getScreenSize
    val px = dimensions.width/2 - frame.getWidth/2
    val py = dimensions.height/2 - frame.getHeight/2
    frame.setLocation(new Point(px, py))
  }


  def showImage(dims: inVec2i, data: inDataSeq[Vec3, Raw]) {
    showImage("Image", dims, data)
  }
  def showImage(title: String, dims: inVec2i, data: inDataSeq[Vec3, Raw]) {
    val img = {
      if (data.rawType == RawType.UByte) {
        mkImgRgbUByte(dims, data.asInstanceOf[inDataSeq[_, UByte]])
      }
      else {
        mkImgRgbVec3(dims, data)
      }
    }

    showBufferedImage(title, img)
  }
  protected def showBufferedImage(title: String, img: BufferedImage) {
    EventQueue.invokeLater(new Runnable {
      def run() {
        val panel = new JPanel() {
          override def paint(g: Graphics) {
            g.drawImage(img, 0, 0, this)
          }
        }
        panel.setPreferredSize(new Dimension(img.getWidth, img.getHeight))

        val frame = new JFrame(title + " " + img.getWidth + "x" + img.getHeight)
        frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
        frame.getContentPane.add(panel)
        frame.pack()
        frame.setResizable(false)

        position(frame)
        frame.setVisible(true)
      }
    })
  }


  def drawFunction(function: (inVec2i, inVec2) => ReadVec3) {
    val f = (dims: inVec2i, t: Double, p: inVec2) => function(dims, p)
    renderFunction(false)("Generated Image", DefaultDims)(f)
  }
  def drawFunction(title: String)(function: (inVec2i, inVec2) => ReadVec3) {
    val f = (dims: inVec2i, t: Double, p: inVec2) => function(dims, p)
    renderFunction(false)(title, DefaultDims)(f)
  }
  def drawFunction
    (title: String, dims: inVec2i)
    (function: (inVec2i, inVec2) => ReadVec3)
  {
    val f = (dims: inVec2i, t: Double, p: inVec2) => function(dims, p)
    renderFunction(false)(title, dims)(f)
  }


  def animateFunction(function: (inVec2i, Double, inVec2) => ReadVec3) {
    renderFunction(true)("Animation", DefaultDims)(function)
  }
  def animateFunction(title: String)(function: (inVec2i, Double, inVec2) => ReadVec3) {
    renderFunction(true)(title, DefaultDims)(function)
  }
  def animateFunction
    (title: String, dims: inVec2i)
    (function: (inVec2i, Double, inVec2) => ReadVec3)
  {
    renderFunction(true)(title, dims)(function)
  }

  protected def renderFunction
    (animate: Boolean)
    (title: String, dims: inVec2i)
    (function: (inVec2i, Double, inVec2) => ReadVec3)
  {
    val drawFps = animate
    val animator = FunctionRenderer(true, function, _.printStackTrace(System.out))
    var actionTimer: Timer = null

    val frame = new JFrame(title + " " + dims.x + "x" + dims.y) {
      override def dispose() {
        if (actionTimer != null) actionTimer.stop()
        animator.dispose()
        super.dispose()
      }
    }

    EventQueue.invokeLater(new Runnable {
      def run() {
        val panel = new JPanel() with ActionListener {

          val fpsTimer = new SystemTimer()
          private[this] var dims = ConstVec2i(0)
          var img: BufferedImage = _
          var error = false

          override def paint(g: Graphics) {
            if (error) return

            fpsTimer.update()

            if (dims.x != getWidth || dims.y != getHeight) {
              dims = ConstVec2i(getWidth, getHeight)
              img = new BufferedImage(dims.x, dims.y, BufferedImage.TYPE_INT_RGB)
            }

            try {
              val buffer = animator.nextFrame(dims, fpsTimer.uptime)
              img.setRGB(0, 0, dims.x, dims.y, buffer, 0, dims.x)
              g.drawImage(img, 0, 0, null)
            }
            catch {
              case t: Throwable =>
                error = true
                frame.dispose()
            }

            if (drawFps) {
              g.setColor(Color.LIGHT_GRAY)
              g.fillRect(9, 6, 32, 16)
              g.setColor(Color.BLACK)

              val bold = new Font("Monospaced", Font.BOLD, 16)
              g.setFont(bold)
              g.setColor(Color.BLACK)
              g.drawString(fpsTimer.averageFps.toInt.toString, 10, 20)
            }

            frame.setTitle(title + " " + dims.x + "x" + dims.y)
          }

          def actionPerformed(e: ActionEvent) {
            repaint()
          }
        }

        panel.setPreferredSize(new Dimension(dims.x, dims.y))
        if (animate) actionTimer = new Timer(1, panel)

        frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
        frame.getContentPane.add(panel)
        frame.pack()
        frame.setResizable(true)

        position(frame)
        frame.setVisible(true)
        if (actionTimer != null) actionTimer.start()
      }
    })
  }

  def drawLines
    (function: (inVec2i) => (ReadDataSeq[Vec2, RFloat], ReadDataSeq[Vec3, UByte], Int))
  {
    val f = (dims: inVec2i, time: Double) => function(dims)
    renderLines(false)("Generated Image", DefaultBackground, DefaultDims)(f)
  }
  def drawLines
    (title: String, background: inVec3)
    (function: (inVec2i) => (ReadDataSeq[Vec2, RFloat], ReadDataSeq[Vec3, UByte], Int))
  {
    val f = (dims: inVec2i, time: Double) => function(dims)
    renderLines(false)(title, background, DefaultDims)(f)
  }
  def drawLines
    (title: String, background: inVec3, dims: inVec2i)
    (function: (inVec2i) => (ReadDataSeq[Vec2, RFloat], ReadDataSeq[Vec3, UByte], Int))
  {
    val f = (dims: inVec2i, time: Double) => function(dims)
    renderLines(false)(title, background, dims)(f)
  }


  def animateLines
    (function: (inVec2i, Double) => (ReadDataSeq[Vec2, RFloat], ReadDataSeq[Vec3, UByte], Int))
  {
    renderLines(true)("Animation", DefaultBackground, DefaultDims)(function)
  }
  def animateLines
    (title: String, background: inVec3)
    (function: (inVec2i, Double) => (ReadDataSeq[Vec2, RFloat], ReadDataSeq[Vec3, UByte], Int))
  {
    renderLines(true)(title, background, DefaultDims)(function)
  }
  def animateLines
    (title: String, background: inVec3, dims: inVec2i)
    (function: (inVec2i, Double) => (ReadDataSeq[Vec2, RFloat], ReadDataSeq[Vec3, UByte], Int))
  {
    renderLines(true)(title, background, dims)(function)
  }

  protected def renderLines
    (animate: Boolean)
    (title: String, background: inVec3, dims: inVec2i)
    (function: (inVec2i, Double) => (ReadDataSeq[Vec2, RFloat], ReadDataSeq[Vec3, UByte], Int))
  {
    val drawFps = animate
    var actionTimer: Timer = null

    val frame = new JFrame(title + " " + dims.x + "x" + dims.y) {
      override def dispose() {
        if (actionTimer != null) actionTimer.stop()
        super.dispose()
      }
    }

    EventQueue.invokeLater(new Runnable {
      def run() {
        val panel = new JPanel() with ActionListener {

          val fpsTimer = new SystemTimer()
          private[this] var dims = ConstVec2i(0)
          var error = false

          // Java2D line rendering settings.
          val lineWidth = 3
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

          override def paint(g: Graphics) {
            if (error) return

            fpsTimer.update()

            if (dims.x != getWidth || dims.y != getHeight) {
              dims = ConstVec2i(getWidth, getHeight)
            }

            val g2 = g.asInstanceOf[Graphics2D]

            // Clear the framebuffer.
            g2.setComposite(AlphaComposite.Src)
            g.setColor(new Color(background.r.toFloat, background.g.toFloat, background.b.toFloat))
            g.fillRect(0, 0, dims.x, dims.y)

            // Apply the line rendering settings.
            g2.setStroke(stroke)
            g2.setRenderingHints(renderHints)
            g2.setComposite(AlphaComposite.SrcOver)

            try {
              val (lines, colors, count) = function(dims, fpsTimer.uptime)

              var i = 0; while (i < count/2) {
                val j = i*2

                val lineStart = lines(j)
                val lineEnd = lines(j + 1)

                val colorStart = colors(j)
                val colorEnd = colors(j + 1)

                g2.setPaint(new GradientPaint(
                  lineStart.x.toFloat, dims.y - lineStart.y.toFloat,
                  new Color(colorStart.r.toFloat, colorStart.g.toFloat, colorStart.b.toFloat, 0.7f),
                  lineEnd.x.toFloat, dims.y - lineEnd.y.toFloat,
                  new Color(colorEnd.r.toFloat, colorEnd.g.toFloat, colorEnd.b.toFloat, 0.7f)
                ))
                g2.drawLine(
                  lineStart.x.toInt, dims.y - lineStart.y.toInt,
                  lineEnd.x.toInt, dims.y - lineEnd.y.toInt
                )
              
                i += 1
              }
            }
            catch {
              case t: Throwable =>
                t.printStackTrace(System.out)
                error = true
                frame.dispose()
            }

            if (drawFps) {
              g.setColor(Color.LIGHT_GRAY)
              g.fillRect(9, 6, 32, 16)
              g.setColor(Color.BLACK)

              val bold = new Font("Monospaced", Font.BOLD, 16)
              g.setFont(bold)
              g.setColor(Color.BLACK)
              g.drawString(fpsTimer.averageFps.toInt.toString, 10, 20)
            }

            frame.setTitle(title + " " + dims.x + "x" + dims.y)
          }

          def actionPerformed(e: ActionEvent) {
            repaint()
          }
        }

        panel.setPreferredSize(new Dimension(dims.x, dims.y))
        if (animate) actionTimer = new Timer(1, panel)

        frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
        frame.getContentPane.add(panel)
        frame.pack()
        frame.setResizable(true)

        position(frame)
        frame.setVisible(true)
        if (actionTimer != null) actionTimer.start()
      }
    })
  }
}
