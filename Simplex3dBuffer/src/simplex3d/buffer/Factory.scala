/*
 * Simplex3d, CoreBuffer module
 * Copyright (C) 2010, Simplex3d Team
 *
 * This file is part of Simplex3dBuffer.
 *
 * Simplex3dBuffer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dBuffer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.buffer

import java.nio._
import scala.annotation.unchecked._


/**
 * @author Aleksey Nikiforov (lex)
 */
trait Factory[E <: MetaElement, +R <: RawData] {
  def mkDataArray(array: R#ArrayType @uncheckedVariance) :DataArray[E, R]
  def mkDataArray(size: Int) :DataArray[E, R]

  def mkReadDataBuffer(byteBuffer: ByteBuffer) :ReadDataBuffer[E, R]
  def mkDataBuffer(size: Int) :DataBuffer[E, R]
  def mkDataBuffer(byteBuffer: ByteBuffer) :DataBuffer[E, R]

  def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) :ReadDataView[E, R]
  def mkDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) :DataView[E, R]

  def emptyMarker() :DataSeq[E, R] = {
    this match {
      case s: DataSeq[_, _] if (s.byteCapacity == 0) => s.asInstanceOf[DataSeq[E, R]]
      case _ => mkDataArray(0)
    }
  }
}
