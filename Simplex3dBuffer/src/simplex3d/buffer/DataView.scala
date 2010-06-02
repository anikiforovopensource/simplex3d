/*
 * Simplex3d, BaseBuffer module
 * Copyright (C) 2010 Simplex3d Team
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
  private[buffer] def sharedWrapper: ProtectedWrapper[ByteBuffer] =
    backingSeq.sharedWrapper
  
  def backingSeq: ReadOnlyDataBuffer[T#Component, D]
  def asReadOnly() :ReadOnlyDataView[T, D]

  final def sharesContent(seq: ReadOnlyDataSeq[_ <: ElemType, _ <: RawType]) {
    seq match {
      case v: ReadOnlyDataView[_, _] =>
        sharedWrapper.unwrap eq v.sharedWrapper.unwrap
      case _ =>
        false
    }
  }

  assert(buffer.position == 0)
  assert(buffer.limit == buffer.capacity)

  if (!buffer.isDirect)
    throw new IllegalArgumentException(
      "The buffer must be direct."
    )
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
    if (buffer.order != ByteOrder.nativeOrder)
    throw new IllegalArgumentException(
      "The buffer must have native byte order."
    )

    ref.factory.mkDataView(buffer, offset, stride)
  }
}