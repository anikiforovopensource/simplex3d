/*
 * Simplex3d, BaseBuffer module
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
import simplex3d.math._


/**
 * @author Aleksey Nikiforov (lex)
 */
trait ReadOnlyDataBuffer[T <: ElemType, +D <: RawType]
extends ReadOnlyDataView[T, D] with ReadOnlyContiguousSeq[T, D] {
  def asReadOnly() :ReadOnlyDataBuffer[T, D]
}

trait DataBuffer[T <: ElemType, +D <: RawType]
extends DataView[T, D] with ContiguousSeq[T, D] with ReadOnlyDataBuffer[T, D]

object DataBuffer {
  def apply[T <: ElemType, D <: ReadableType](buffer: ByteBuffer)(
    implicit ref: FactoryRef[T, D]
  ) :DataBuffer[T, D] = {
    if (buffer.isReadOnly)
    throw new IllegalArgumentException(
      "The buffer must not be read-only."
    )

    ref.factory.mkDataBuffer(buffer)
  }

  def apply[T <: ElemType, D <: ReadableType](size: Int)(
    implicit ref: FactoryRef[T, D]
  ) :DataBuffer[T, D] = {
    ref.factory.mkDataBuffer(size)
  }

  def apply[T <: ElemType, D <: ReadableType](vals: T#Element*)(
    implicit ref: FactoryRef[T, D]
  ) :DataBuffer[T, D] = {
    val data = ref.factory.mkDataBuffer(vals.size)
    data.put(vals)
    data
  }

  def apply[T <: ElemType, D <: ReadableType](db: DataBuffer[_, _])(
    implicit ref: FactoryRef[T, D]
  ) :DataBuffer[T, D] = {
    if (db.isReadOnly) throw new IllegalArgumentException(
      "The argument must not be read only."
    )

    ref.factory.mkDataBuffer(db.backingSeq.sharedByteBuffer)
  }

  def apply[T <: ElemType, D <: ReadableType](db: roDataBuffer[_, _])(
    implicit ref: FactoryRef[T, D]
  ) :roDataBuffer[T, D] = {
    val res = ref.factory.mkDataBuffer(db.backingSeq.sharedByteBuffer)
    if (db.isReadOnly) res.asReadOnly() else res
  }
}
