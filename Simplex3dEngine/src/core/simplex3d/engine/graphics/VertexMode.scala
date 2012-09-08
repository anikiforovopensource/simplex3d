/*
 * Simplex3dEngine - Core Module
 * Copyright (C) 2011, Aleksey Nikiforov
 *
 * This file is part of Simplex3dEngine.
 *
 * Simplex3dEngine is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dEngine is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.engine
package graphics

import simplex3d.math.types._
import simplex3d.math.double._


sealed abstract class VertexMode extends Accessible {
  type Read = VertexMode
  type Mutable = VertexMode
  
  final def readType = this.getClass()
}

case class Points(var size: Double = 3) extends VertexMode {
  def mutableCopy() = Points(size)
  def :=(m: VertexMode) { size = m.asInstanceOf[Points].size }
}

case class PointSprites(var size: Double) extends VertexMode {
  def mutableCopy() = PointSprites(size)
  def :=(m: VertexMode) { size = m.asInstanceOf[PointSprites].size }
}

case class Lines(var width: Double = 3) extends VertexMode {
  def mutableCopy() = Lines(width)
  def :=(m: VertexMode) { width = m.asInstanceOf[Lines].width }
}

case class LineStrip(var width: Double = 3) extends VertexMode {
  def mutableCopy() = LineStrip(width)
  def :=(m: VertexMode) { width = m.asInstanceOf[LineStrip].width }
}

case class LineLoop(var width: Double = 3) extends VertexMode {
  def mutableCopy() = LineLoop(width)
  def :=(m: VertexMode) { width = m.asInstanceOf[LineLoop].width }
}

sealed class Triangles private extends VertexMode {
  def mutableCopy() = this
  def :=(m: VertexMode) {}
}
object Triangles extends Triangles

sealed class TriangleStrip private extends VertexMode {
  def mutableCopy() = this
  def :=(m: VertexMode) {}
}
object TriangleStrip extends TriangleStrip

sealed  class TriangleFan private extends VertexMode {
  def mutableCopy() = this
  def :=(m: VertexMode) {}
}
object TriangleFan extends TriangleFan
