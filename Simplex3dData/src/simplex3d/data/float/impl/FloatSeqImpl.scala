/*
 * Simplex3d, FloatData module
 * Copyright (C) 2010-2011, Aleksey Nikiforov
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

package simplex3d.data.float
package impl

import java.nio._
import simplex3d.math.floatx._
import simplex3d.data._


/**
 * @author Aleksey Nikiforov (lex)
 */
private[data] final class ViewRFloatRFloat(
  prim: ReadDataBuffer[RFloat, RFloat], off: Int, str: Int
) extends BaseRFloat[RFloat](
  prim, prim, prim.isReadOnly, off, str
) with DataView[RFloat, RFloat] {
  def mkReadOnlyInstance() = new ViewRFloatRFloat(primitives.asReadOnly(), offset, stride)
  def rawType: Int = RawType.RFloat
  def isNormalized = false

  def mkDataArray(array: Array[Float]) =
    new ArrayRFloatRFloat(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRFloatRFloat(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Float = buff.get(offset + i*stride)
  def update(i: Int, v: Float) { buff.put(offset + i*stride, v) }
}
