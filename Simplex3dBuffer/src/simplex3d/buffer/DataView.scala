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
trait ReadOnlyDataView[T <: ElemType, +D <: RawType]
extends ReadOnlyDataSeq[T, D] {

  assert(buffer.position == 0)
  assert(buffer.limit == buffer.capacity)

  if (!buffer.isDirect)
    throw new IllegalArgumentException(
      "The buffer must be direct."
    )

  
  def backingSeq: roDataBuffer[T#Component, D]
  def asReadOnly() :roDataView[T, D]

  final def sharesMemory(seq: roDataSeq[_ <: ElemType, _ <: RawType]) = {
    seq match {
      case v: roDataView[_, _] =>
        backingSeq.sharedByteBuffer eq v.backingSeq.sharedByteBuffer
      case _ =>
        false
    }
  }
}

trait DataView[T <: ElemType, +D <: RawType]
extends DataSeq[T, D] with ReadOnlyDataView[T, D]{
  def backingSeq: DataBuffer[T#Component, D]
}

object DataView {
  def apply[T <: ElemType, D <: ReadableType](
    buffer: ByteBuffer, offset: Int, stride: Int
  )(implicit ref: FactoryRef[T, D]) :DataView[T, D] = {
    if (buffer.isReadOnly)
    throw new IllegalArgumentException(
      "The buffer must not be read-only."
    )

    ref.factory.mkDataView(buffer, offset, stride)
  }

  def apply[T <: ElemType, D <: ReadableType](
    db: DataBuffer[_, _], offset: Int, stride: Int
  )(implicit ref: FactoryRef[T, D]) :DataView[T, D] = {
    if (db.isReadOnly) throw new IllegalArgumentException(
      "The argument must not be read only."
    )

    ref.factory.mkDataView(db.backingSeq.sharedByteBuffer, offset, stride)
  }

  def apply[T <: ElemType, D <: ReadableType](
    db: roDataBuffer[_, _], offset: Int, stride: Int
  )(implicit ref: FactoryRef[T, D]) :roDataView[T, D] = {
    ref.factory.mkDataView(
      db.backingSeq.sharedByteBuffer, offset, stride
    ).asReadOnly()
  }
}
