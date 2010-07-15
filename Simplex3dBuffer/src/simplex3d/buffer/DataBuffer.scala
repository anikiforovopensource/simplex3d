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


/**
 * @author Aleksey Nikiforov (lex)
 */
trait ReadDataBuffer[E <: MetaElement, +R <: RawData]
extends ReadDataView[E, R] with ReadContiguousSeq[E, R] {
  def asReadOnlySeq() :ReadDataBuffer[E, R]
}

trait DataBuffer[E <: MetaElement, +R <: RawData]
extends DataView[E, R] with ContiguousSeq[E, R] with ReadDataBuffer[E, R]


object ReadDataBuffer {
  def apply[E <: MetaElement, R <: ReadableData](buffer: ByteBuffer)(
    implicit ref: FactoryRef[E, R]
  ) :ReadDataBuffer[E, R] = {
    ref.factory.mkReadDataBuffer(buffer)
  }

  def apply[E <: MetaElement, R <: ReadableData](db: ReadDataBuffer[_, _])(
    implicit ref: FactoryRef[E, R]
  ) :ReadDataBuffer[E, R] = {
    val res = ref.factory.mkReadDataBuffer(db.sharedBuffer)
    if (db.isReadOnly) res.asReadOnlySeq() else res
  }
}

object DataBuffer {
  def apply[E <: MetaElement, R <: ReadableData](buffer: ByteBuffer)(
    implicit ref: FactoryRef[E, R]
  ) :DataBuffer[E, R] = {
    ref.factory.mkDataBuffer(buffer)
  }

  def apply[E <: MetaElement, R <: ReadableData](size: Int)(
    implicit ref: FactoryRef[E, R]
  ) :DataBuffer[E, R] = {
    ref.factory.mkDataBuffer(size)
  }

  def apply[E <: MetaElement, R <: ReadableData](vals: E#Element*)(
    implicit ref: FactoryRef[E, R]
  ) :DataBuffer[E, R] = {
    val data = ref.factory.mkDataBuffer(vals.size)
    data.put(vals)
    data
  }

  def apply[E <: MetaElement, R <: ReadableData](db: DataBuffer[_, _])(
    implicit ref: FactoryRef[E, R]
  ) :DataBuffer[E, R] = {
    if (db.isReadOnly) throw new ClassCastException(
      "The DataBuffer must not be read-only."
    )
    ref.factory.mkDataBuffer(db.sharedBuffer)
  }
}
