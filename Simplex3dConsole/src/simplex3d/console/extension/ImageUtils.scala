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

import java.awt.Dimension
import java.awt.EventQueue
import java.awt.Graphics
import java.awt.Point
import java.awt.Toolkit
import java.awt.image.BufferedImage
import javax.swing.JFrame
import javax.swing.JPanel
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

  private[this] def rgb(c: inVec3) :Int = {
    ((toUByte(c.r) & 0xFF) << 16) | ((toUByte(c.g) & 0xFF) << 8) | ((toUByte(c.b) & 0xFF))
  }

  private[this] def rgb(j: Int, d: inContiguous[SInt, UByte]) :Int = {
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

  private[this] def showImage(title: String, img: BufferedImage) {
    EventQueue.invokeLater(new Runnable {
      def run() {
        val panel = new JPanel() {
          override def paint(g: Graphics) {
            g.drawImage(img, 0, 0, this)
          }
        }
        panel.setPreferredSize(new Dimension(img.getWidth, img.getHeight))

        val frame = new JFrame(title + " " + img.getWidth + "x" + img.getHeight)
        frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        frame.getContentPane.add(panel)
        frame.pack()
        frame.setResizable(false)

        val dim = Toolkit.getDefaultToolkit().getScreenSize();
        val px = dim.width/2 - frame.getWidth()/2;
        val py = dim.height/2 - frame.getHeight()/2;
        frame.setLocation(new Point(px, py));

        frame.setVisible(true);
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

    showImage(title, img)
  }

  def genImage(dims: inVec2i)(function: inVec2 => ReadVec3) {
    genImage("Image", dims)(function)
  }
  def genImage(title: String, dims: inVec2i)(function: inVec2 => ReadVec3) {
    val img = new BufferedImage(dims.x, dims.y, BufferedImage.TYPE_INT_RGB)

    var y = 0; while (y < dims.y) {
      var x = 0; while (x < dims.x) {

        img.setRGB(x, y, rgb(function(ConstVec2(x, y))))

        x += 1
      }
      y += 1
    }

    showImage(title, img)
  }
}
