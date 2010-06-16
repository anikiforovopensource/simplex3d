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
trait ReadOnlyDataView[E <: ElemType, +R <: RawType]
extends ReadOnlyDataSeq[E, R] {

  assert(buffer.position == 0)
  assert(buffer.limit == buffer.capacity)

  if (!buffer.isDirect)
    throw new IllegalArgumentException(
      "The buffer must be direct."
    )

  
  def backingSeq: ReadOnlyDataBuffer[E#Component, R]
  def asReadOnly() :ReadOnlyDataView[E, R]

  final def sharesMemory(seq: inDataSeq[_ <: ElemType, _ <: RawType]) = {
    seq match {
      case v: ReadOnlyDataView[_, _] =>
        backingSeq.sharedByteBuffer eq v.backingSeq.sharedByteBuffer
      case _ =>
        false
    }
  }
}

trait DataView[E <: ElemType, +R <: RawType]
extends DataSeq[E, R] with ReadOnlyDataView[E, R] {
  def backingSeq: DataBuffer[E#Component, R]
}

object DataView {
  def apply[E <: ElemType, R <: ReadableType](
    buffer: ByteBuffer, offset: Int, stride: Int
  )(implicit ref: FactoryRef[E, R]) :DataView[E, R] = {
    if (buffer.isReadOnly)
    throw new IllegalArgumentException(
      "The buffer must not be read-only."
    )

    ref.factory.mkDataView(buffer, offset, stride)
  }

  def apply[E <: ElemType, R <: ReadableType](
    db: DataBuffer[_, _], offset: Int, stride: Int
  )(implicit ref: FactoryRef[E, R]) :DataView[E, R] = {
    if (db.isReadOnly) throw new IllegalArgumentException(
      "The argument must not be read only."
    )

    ref.factory.mkDataView(db.backingSeq.sharedByteBuffer, offset, stride)
  }

  def apply[E <: ElemType, R <: ReadableType](
    db: inDataBuffer[_, _], offset: Int, stride: Int
  )(implicit ref: FactoryRef[E, R]) :ReadOnlyDataView[E, R] = {
    val res = ref.factory.mkDataView(
      db.backingSeq.sharedByteBuffer, offset, stride
    )

    if (db.isReadOnly) res.asReadOnly() else res
  }
}
