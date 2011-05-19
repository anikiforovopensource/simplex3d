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
package extension

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
object ImageUtils extends ImageUtils {
  val impl = new ImageUtils

  override def showImage(dims: inVec2i, data: inData[Vec3]) {
    PrivilegedRunner.queue {
      impl.showImage(dims, data)
    }
  }
  override def showImage(title: String, dims: inVec2i, data: inData[Vec3]) {
    PrivilegedRunner.queue {
      impl.showImage(title, dims, data)
    }
  }


  override def drawFunction(function: (inVec2i, inVec2) => ReadVec3) {
    PrivilegedRunner.queue {
      impl.drawFunction(function)
    }
  }
  override def drawFunction(title: String)(function: (inVec2i, inVec2) => ReadVec3) {
    PrivilegedRunner.queue {
      impl.drawFunction(title)(function)
    }
  }
  override def drawFunction
    (title: String, dims: inVec2i)
    (function: (inVec2i, inVec2) => ReadVec3)
  {
    PrivilegedRunner.queue {
      impl.drawFunction(title, dims)(function)
    }
  }

  override def animateFunction(function: (inVec2i, Double, inVec2) => ReadVec3) {
    PrivilegedRunner.queue {
      impl.animateFunction(function)
    }
  }
  override def animateFunction(title: String)(function: (inVec2i, Double, inVec2) => ReadVec3) {
    PrivilegedRunner.queue {
      impl.animateFunction(title)(function)
    }
  }
  override def animateFunction
    (title: String, dims: inVec2i)
    (function: (inVec2i, Double, inVec2) => ReadVec3)
  {
    PrivilegedRunner.queue {
      impl.animateFunction(title, dims)(function)
    }
  }
}

class ImageUtils {

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


  def drawFunction(function: (inVec2i, inVec2) => ReadVec3) {
    drawFunction("Generated Image", ConstVec2i(640, 480))(function)
  }
  def drawFunction(title: String)(function: (inVec2i, inVec2) => ReadVec3) {
    drawFunction(title, ConstVec2i(640, 480))(function)
  }
  def drawFunction
    (title: String, dims: inVec2i)
    (function: (inVec2i, inVec2) => ReadVec3)
  {

    val f = (dims: inVec2i, t: Double, p: inVec2) => function(dims, p)
    val animator = FunctionAnimator(f, _.printStackTrace(System.out), false)
    
    val frame = new JFrame(title + " " + dims.x + "x" + dims.y) {
      override def dispose() {
        animator.dispose()
        super.dispose()
      }
    }

    EventQueue.invokeLater(new Runnable {
      def run() {

        val panel = new JPanel() {
          var buffer = new Array[Int](0)
          var error = false

          override def paint(g: Graphics) {
            if (error) return

            val dims = ConstVec2i(getWidth, getHeight)
            val img = new BufferedImage(dims.x, dims.y, BufferedImage.TYPE_INT_RGB)

            try {
              buffer = animator.oneFrame(dims, 0, buffer)
              img.setRGB(0, 0, dims.x, dims.y, buffer, 0, dims.x)
              g.drawImage(img, 0, 0, null)
            }
            catch {
              case t: Throwable =>
                error = true
                frame.dispose()
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


  def animateFunction(function: (inVec2i, Double, inVec2) => ReadVec3) {
    animateFunction("Animation", ConstVec2i(640, 480))(function)
  }
  def animateFunction(title: String)(function: (inVec2i, Double, inVec2) => ReadVec3) {
    animateFunction(title, ConstVec2i(640, 480))(function)
  }
  def animateFunction
    (title: String, dims: inVec2i)
    (function: (inVec2i, Double, inVec2) => ReadVec3)
  {

    val drawFps = true

    val animator = FunctionAnimator(function, _.printStackTrace(System.out))
    var actionTimer: Timer = null

    val frame = new JFrame(title + " " + dims.x + "x" + dims.y) {
      override def dispose() {
        actionTimer.stop()
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
        actionTimer = new Timer(1, panel)

        frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
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
