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

package simplex3d.console.extension

import java.awt.Color
import java.awt.Dimension
import java.awt.EventQueue
import java.awt.Font
import java.awt.Graphics
import java.awt.Point
import java.awt.Toolkit
import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import java.awt.event.WindowAdapter
import java.awt.event.WindowEvent
import java.awt.image.BufferedImage
import javax.swing.JFrame
import javax.swing.JPanel
import javax.swing.Timer
import javax.swing.WindowConstants
import simplex3d.math._
import simplex3d.math.double._
import simplex3d.data._
import simplex3d.data.double._
import conversion.Double._


/**
 * @author Aleksey Nikiforov (lex)
 */
object ImageUtils {

  private[extension] def rgb(c: inVec3) :Int = {
    ((toUByte(c.r) & 0xFF) << 16) | ((toUByte(c.g) & 0xFF) << 8) | ((toUByte(c.b) & 0xFF))
  }

  private[extension] def rgb(j: Int, d: inContiguous[SInt, UByte]) :Int = {
    (d(j) << 16) | (d(j + 1) << 8) | (d(j + 2))
  }


  private[this] def mkImgRgbVec3(dims: inVec2i, data: inData[Vec3]) = {
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
    val p = ReadContiguous[SInt, UByte](data.primitive)

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

  private[this] def showBufferedImage(title: String, img: BufferedImage) {
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

  def showImage(dims: inVec2i, data: inData[Vec3]) {
    showImage("Image", dims, data)
  }
  def showImage(title: String, dims: inVec2i, data: inData[Vec3]) {
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


  def drawFunction(function: (inVec2, inVec2) => ReadVec3) {
    drawFunction("Generated Image", ConstVec2i(640, 480))(function)
  }
  def drawFunction(title: String)(function: (inVec2, inVec2) => ReadVec3) {
    drawFunction(title, ConstVec2i(640, 480))(function)
  }
  def drawFunction
    (title: String, dims: inVec2i)
    (function: (inVec2, inVec2) => ReadVec3)
  {

    val frame = new JFrame(title + " " + dims.x + "x" + dims.y)

    EventQueue.invokeLater(new Runnable {
      def run() {

        val panel = new JPanel() {
          override def paint(g: Graphics) {

            val dims = ConstVec2i(getWidth, getHeight)
            val fpSize: ConstVec2 = dims
            val img = new BufferedImage(dims.x, dims.y, BufferedImage.TYPE_INT_RGB)

            var y = 0; while (y < dims.y) {
              val h = dims.y - 1 - y

              var x = 0; while (x < dims.x) {

                img.setRGB(x, y, rgb(function(fpSize, ConstVec2(x, h))))

                x += 1
              }
              y += 1
            }

            g.drawImage(img, 0, 0, this)
            frame.setTitle(title + " " + dims.x + "x" + dims.y)
          }
        }

        panel.setPreferredSize(new Dimension(dims.x, dims.y))

        frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
        frame.getContentPane.add(panel)
        frame.pack()
        frame.setResizable(true)

        position(frame)
        frame.setVisible(true)
      }
    })
  }


  def animateFunction(function: (inVec2, Double, inVec2) => ReadVec3) {
    animateFunction("Animation", ConstVec2i(640, 480))(function)
  }
  def animateFunction(title: String)(function: (inVec2, Double, inVec2) => ReadVec3) {
    animateFunction(title, ConstVec2i(640, 480))(function)
  }
  def animateFunction
    (title: String, dims: inVec2i)
    (function: (inVec2, Double, inVec2) => ReadVec3)
  {

    val drawFps = true

    val frame = new JFrame(title + " " + dims.x + "x" + dims.y)
    val animator = FunctionAnimator(function)

    EventQueue.invokeLater(new Runnable {
      def run() {
        val panel = new JPanel() with ActionListener {

          val fpsTimer = new SystemTimer()
          private[this] var dims = ConstVec2i(0)
          var img: BufferedImage = _

          override def paint(g: Graphics) {
            fpsTimer.update()

            if (dims.x != getWidth || dims.y != getHeight) {
              dims = ConstVec2i(getWidth, getHeight)
              img = new BufferedImage(dims.x, dims.y, BufferedImage.TYPE_INT_RGB)
            }

            img.setRGB(0, 0, dims.x, dims.y, animator.nextFrame(dims, fpsTimer.uptime), 0, dims.x)
            g.drawImage(img, 0, 0, null)

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
        val actionTimer = new Timer(1, panel)

        frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
        frame.addWindowListener(new WindowAdapter() {
          override def windowClosing(e: WindowEvent) {
            animator.dispose()
            actionTimer.stop()
            super.windowClosing(e)
          }
        })

        frame.getContentPane.add(panel)
        frame.pack()
        frame.setResizable(true)

        position(frame)
        frame.setVisible(true)
        actionTimer.start()
      }
    })
  }
}
