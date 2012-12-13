/*
 * Simplex3dEngine - Core Module
 * Copyright (C) 2012, Aleksey Nikiforov
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

import simplex3d.math.double._


sealed abstract class ReadPrimitive extends prototype.ReadStruct {
  type Read = ReadPrimitive
  type Mutable = Primitive
  final def readType = classOf[ReadPrimitive]
  
  def mode: ReadEnumRef[VertexMode.type]
  def faceCulling: ReadEnumRef[FaceCulling.type]
  
  def lineWidth: ReadDoubleRef
  def pointSize: DoubleRef
  def pointSpriteSize: DoubleRef
}

final class Primitive extends ReadPrimitive with prototype.Struct {
  protected def mkMutable() = new Primitive
  
  val mode = new EnumRef(VertexMode.Triangles)
  val faceCulling = new EnumRef(FaceCulling.Disabled)
  
  val lineWidth = new DoubleRef(1)
  val pointSize = new DoubleRef(1)
  val pointSpriteSize = new DoubleRef(1)
  
  init(classOf[Primitive])
}
