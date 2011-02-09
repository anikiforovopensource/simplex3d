/*
 * Simplex3d, MathTest package
 * Copyright (C) 2010-2011, Aleksey Nikiforov
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

package visual.math

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import visual.math.lines._


/**
 * @author Aleksey Nikiforov (lex)
 */
object FullTransformationTest {

  val len = 4.0
  val axis = Model(
    Array(
      Vec3(0, 0, 0), Vec3(len, 0, 0),
      Vec3(0, 0, 0), Vec3(0, len, 0),
      Vec3(0, 0, 0), Vec3(0, 0, len)
    )
  )

  val d = len*0.1
  val x = Vec3(len + d, d, d)
  val y = Vec3(d, len + d, d)
  val z = Vec3(d, d, len + d)
  val labels = Model(
    Array(
      // X.
      x + Vec3(-d, d, 0), x + Vec3(d, -d, 0),
      x + Vec3(d, d, 0), x + Vec3(-d, -d, 0),
      // Y.
      y + Vec3(-d, d, 0), y + Vec3(0),
      y + Vec3(d, d, 0), y + Vec3(0),
      y + Vec3(0), y + Vec3(0, -d, 0),
      // Z.
      z + Vec3(-d, d, 0), z + Vec3(d, d, 0),
      z + Vec3(d, d, 0), z + Vec3(-d, -d, 0),
      z + Vec3(-d, -d, 0), z + Vec3(d, -d, 0)
    )
  )

  val s = 8.0
  val box = Model(
    Array(
      // Front.
      Vec3(0, 0, 0), Vec3(s, 0, 0),
      Vec3(s, 0, 0), Vec3(s, s, 0),
      Vec3(s, s, 0), Vec3(0, s, 0),
      Vec3(0, s, 0), Vec3(0, 0, 0),
      // Back.
      Vec3(0, 0, s), Vec3(s, 0, s),
      Vec3(s, 0, s), Vec3(s, s, s),
      Vec3(s, s, s), Vec3(0, s, s),
      Vec3(0, s, s), Vec3(0, 0, s),
      // Sides.
      Vec3(0, 0, 0), Vec3(0, 0, s),
      Vec3(s, 0, 0), Vec3(s, 0, s),
      Vec3(s, s, 0), Vec3(s, s, s),
      Vec3(0, s, 0), Vec3(0, s, s)
    )
  )
  box.transformation.applyTranslation(Vec3(-s/2))

  val side = 8.0
  val height2d = sqrt(3.0/4.0*side*side)
  val radius2d = side/sqrt(3.0)
  val centerz = sqrt(radius2d*radius2d - 1.0/4.0*side*side)
  val height3d = side*sqrt(6.0)/3.0
  val radius3d = side*sqrt(3.0/8.0)
  val centery = sqrt(radius3d*radius3d - radius2d*radius2d)
  val simplex = Model(
    Array(
      // Base.
      Vec3(0, 0, 0), Vec3(side, 0, 0),
      Vec3(side, 0, 0), Vec3(side/2, 0, height2d),
      Vec3(side/2, 0, height2d), Vec3(0, 0, 0),
      // The remaining 3 edges.
      Vec3(0, 0, 0), Vec3(side/2, height3d, centerz),
      Vec3(side, 0, 0), Vec3(side/2, height3d, centerz),
      Vec3(side/2, 0, height2d), Vec3(side/2, height3d, centerz)
    )
  )
  simplex.transformation.applyTranslation(-Vec3(side/2, centery, centerz))

  val camRotataion = Quat4 rotateX(radians(-30))
  val cam = Camera(camRotataion, rotateVector(Vec3(0, 0, 20), camRotataion))
  val rotationSpeed = radians(30)

  def main(args: Array[String]) {
    ViewPanel.launch(
      (drawLine, viewportDim, uptime, tpf) => {
        cam.rotation := cam.rotation rotateY(rotationSpeed*tpf)
        cam.translation := rotateVector(Vec3(0, 0, 20), cam.rotation)

        LineEngine.render(drawLine, viewportDim, cam)(simplex)//(axis, labels)//(box)
      }
    )
  }
}
