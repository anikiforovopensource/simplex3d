/*
 * Simplex3d, FloatData module
 * Copyright (C) 2010, Simplex3d Team
 *
 * This file is part of Simplex3dData.
 *
 * Simplex3dData is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dData is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.data.floatm
package impl

import java.nio._
import simplex3d.math.floatm._
import simplex3d.data._


/**
 * @author Aleksey Nikiforov (lex)
 */
private[buffer] final class ViewRFloatRFloat(
  primitive: ReadDataBuffer[RFloat, RFloat], off: Int, str: Int
) extends BaseRFloat[RFloat](
  primitive, primitive, primitive.readOnly, off, str
) with DataView[RFloat, RFloat] {
  def mkReadOnlyInstance() = new ViewRFloatRFloat(backing.asReadOnly(), offset, stride)
  def rawType: Int = RawType.RFloat
  def normalized = false

  def mkDataArray(array: Array[Float]) =
    new ArrayRFloatRFloat(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRFloatRFloat(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Float = buff.get(offset + i*stride)
  def update(i: Int, v: Float) { buff.put(offset + i*stride, v) }
}
