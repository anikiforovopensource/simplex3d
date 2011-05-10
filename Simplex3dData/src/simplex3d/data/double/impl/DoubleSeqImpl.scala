/*
 * Simplex3d, DoubleData module
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

package simplex3d.data.double
package impl

import java.nio._
import simplex3d.math.doublex._
import simplex3d.data._


/**
 * @author Aleksey Nikiforov (lex)
 */
private[data] final class ViewRDoubleRFloat(
  prim: ReadDataBuffer[RDouble, RFloat], off: Int, str: Int
) extends BaseRDouble[RFloat](
  prim, prim, prim.isReadOnly, off, str
) with DataView[RDouble, RFloat] {
  def mkReadOnlyInstance() = new ViewRDoubleRFloat(primitives.asReadOnly(), offset, stride)
  def rawType: Int = RawType.RFloat
  def isNormalized = false

  def mkDataArray(array: Array[Float]) =
    new ArrayRDoubleRFloat(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRDoubleRFloat(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Double = buff.get(offset + i*stride)
  def update(i: Int, v: Double) { buff.put(offset + i*stride, v.toFloat) }
}
