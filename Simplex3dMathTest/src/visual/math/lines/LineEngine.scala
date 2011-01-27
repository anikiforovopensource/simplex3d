/*
 * Simplex3d, MathTest package
 * Copyright (C) 2010-2011, Simplex3d Team
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

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._


/**
 * @author Aleksey Nikiforov (lex)
 */
class Camera private (
  val rotation: Quat4, val translation: Vec3,
  val viewAngle: Double, val near: Double, val far: Double
)
object Camera {
  def apply(
    rotation: Quat4 = Quat4(1, 0, 0, 0),
    translation: Vec3 = Vec3(0),
    viewAngle: Double = radians(45),
    near: Double = 10,
    far: Double = 100
  ) = new Camera(rotation, translation, viewAngle, near, far)
}


class Model private (
  val mesh: Array[Vec3],
  val transformation :Mat3x4
)
object Model {
  def apply(
    mesh: Array[Vec3],
    transformation :Mat3x4 = Mat3x4(1)
  ) = new Model(mesh, transformation)
}


object LineEngine {

  /* Each line is transformed to window coordinates as follows:
   *
   * - Find the Model matrix, which transforms model coordinates to world coordinates.
   * - Find the matrix that positions camera in world coordinates.
   *   Invert that matrix to find the View matrix which transforms world
   *   coordinates to view coordinates.
   * - Mutliply the View matrix by the Model matrix to produce ModelView matrix,
   *   which transforms model coordinates to view coordinates.
   * - Find the Projection matrix, which transforms view coordinates to
   *   clip coordinates. Projection matrix can be set as perspective projection
   *   or ortho projection.
   * - Multiply the Projection matrix by the ModelView matrix to produce
   *   ModelViewProjection matrix. This matrix transforms mesh coordinates to
   *   clip coordinates.
   * - Transform line vertex coordinates to homogenous coordinates by taking
   *   x, y, and z coordinates and setting w coordinate to 1.
   * - Multiply the ModelViewProjection matrix by the homogenous vertex
   *   coordinates to produce clip coordinates.
   * - Use clip coordinates to clip the line outside the view frustrum and,
   *   optionally, clip the line against any additional clipping plains.
   * - Perform perspective division on clipped coordinates to produce
   *   normalized device coordinates.
   * - Scale and translate the normalized device coordinates according to
   *   the viewport parameters to produce the window coordinates.
   */
  def render(
    drawLine: (inVec2, inVec2) => Unit,
    viewportDim: inVec2i,
    cam: Camera
  )(models: Model*)
  {
    val cameraMat = Mat3x4 rotate(cam.rotation) translate(cam.translation)
    val viewMat = inverse(cameraMat)

    val projectionMat = perspectiveProj(
      cam.viewAngle,
      Double(viewportDim.x)/viewportDim.y, // aspect ratio
      cam.near, cam.far
    )

    val viewPortScale = Vec2(viewportDim.x, -viewportDim.y)/2 // invert y.
    val viewPortOffset = Vec2(viewportDim.x, viewportDim.y)/2


    for (model <- models) yield {
      val modelMat = model.transformation
      val modelViewMat = modelMat.concat(viewMat)
      val modelViewProjectionMat = projectionMat*Mat4(modelViewMat)

      var i = 0; while (i <= model.mesh.size - 2) {
        val lineStart = model.mesh(i)
        val lineEnd = model.mesh(i + 1)
        
        val p0 = modelViewProjectionMat*Vec4(lineStart, 1)
        val p1 = modelViewProjectionMat*Vec4(lineEnd, 1)

        clipLine(p0, p1)
        drawLine(clippedToWindowCoords(p0), clippedToWindowCoords(p1))
        
        i += 2
      }

      def clipLine(p0: outVec4, p1: outVec4) {
        // clipping is not implemented
      }

      def clippedToWindowCoords(clipCoords: inVec4) :Vec2 = {
        val normalizedDevCoords = clipCoords.xy/clipCoords.w
        Vec2(normalizedDevCoords*viewPortScale + viewPortOffset)
      }
    }
  }
}
