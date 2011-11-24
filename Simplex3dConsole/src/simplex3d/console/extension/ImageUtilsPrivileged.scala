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
import simplex3d.console.script._
import conversion.Double._


/**
 * @author Aleksey Nikiforov (lex)
 */
object ImageUtilsPrivileged extends ImageUtils {

  override protected def showBufferedImage(title: String, img: BufferedImage) {
    PrivilegedRunner.queue {
      super.showBufferedImage(title, img)
    }
  }

  override protected def renderFunction
    (animate: Boolean)
    (title: String, dims: inVec2i)
    (function: (inVec2i, Double, inVec2) => ReadVec3)
  {
    PrivilegedRunner.queue {
      super.renderFunction(animate)(title, dims)(function)
    }
  }

  override protected def renderLines
    (animate: Boolean)
    (title: String, background: inVec3, dims: inVec2i)
    (function: (inVec2i, Double) => (ReadDataSeq[Vec2, RFloat], ReadDataSeq[Vec3, UByte], Int))
  {
    PrivilegedRunner.queue {
      super.renderLines(animate)(title, background, dims)(function)
    }
  }
}
