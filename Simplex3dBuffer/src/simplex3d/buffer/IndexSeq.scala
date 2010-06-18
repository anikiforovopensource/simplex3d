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
import scala.annotation.unchecked._


/**
 * @author Aleksey Nikiforov (lex)
 */
trait ReadIndexSeq[+R <: ReadableIndex]
extends ReadContiguousSeq[Int1, R] {
  def asReadOnlySeq() :ReadIndexSeq[R]

  def mkIndexArray(size: Int) :IndexArray[R] =
    mkDataArray(size)
  def mkIndexArray(array: R#ArrayType @uncheckedVariance) :IndexArray[R] =
    mkDataArray(array)
  def mkIndexBuffer(size: Int) :IndexBuffer[R] =
    mkDataBuffer(size)
  def mkIndexBuffer(byteBuffer: ByteBuffer) :IndexBuffer[R] =
    mkDataBuffer(byteBuffer)

  def copyAsIndexArray() :IndexArray[R] = super.copyAsDataArray()
  def copyAsIndexBuffer() :IndexBuffer[R] = super.copyAsDataBuffer()
}

trait IndexSeq[+R <: ReadableIndex]
extends ContiguousSeq[Int1, R] with ReadIndexSeq[R]


trait ReadIndexArray[+R <: ReadableIndex]
extends ReadIndexSeq[R] with ReadDataArray[Int1, R] {
  def asReadOnlySeq() :ReadIndexArray[R]
}

trait IndexArray[+R <: ReadableIndex]
extends IndexSeq[R] with DataArray[Int1, R] with ReadIndexArray[R]


trait ReadIndexBuffer[+R <: ReadableIndex]
extends ReadIndexSeq[R] with ReadDataBuffer[Int1, R] {
  def asReadOnlySeq() :ReadIndexBuffer[R]
}

trait IndexBuffer[+R <: ReadableIndex]
extends IndexSeq[R] with DataBuffer[Int1, R] with ReadIndexBuffer[R]


object IndexArray {
  def apply[R <: ReadableIndex](array: R#ArrayType)(
    implicit ref: FactoryRef[Int1, R]
  ) :IndexArray[R] = {
    ref.factory.mkDataArray(array)
  }

  def apply[R <: ReadableIndex](size: Int)(
    implicit ref: FactoryRef[Int1, R]
  ) :IndexArray[R] = {
    ref.factory.mkDataArray(size)
  }

  def apply[R <: ReadableIndex](vals: Int*)(
    implicit ref: FactoryRef[Int1, R]
  ) :IndexArray[R] = {
    val data = ref.factory.mkDataArray(vals.size)
    data.put(vals)
    data
  }

  def apply[R <: ReadableIndex](da: DataArray[_, R])(
    implicit ref: FactoryRef[Int1, R]
  ) :IndexArray[R] = {
    if (da.isReadOnly) throw new IllegalArgumentException(
      "The argument must not be read only."
    )

    ref.factory.mkDataArray(da.array)
  }

  def apply[R <: ReadableIndex](da: inDataArray[_, R])(
    implicit ref: FactoryRef[Int1, R]
  ) :ReadIndexArray[R] = {
    val res = ref.factory.mkDataArray(da.sharedArray)
    
    if (da.isReadOnly) res.asReadOnlySeq() else res
  }
}

object IndexBuffer {
  def apply[R <: ReadableIndex](buffer: ByteBuffer)(
    implicit ref: FactoryRef[Int1, R]
  ) :IndexBuffer[R] = {
    ref.factory.mkDataBuffer(buffer)
  }

  def apply[R <: ReadableIndex](size: Int)(
    implicit ref: FactoryRef[Int1, R]
  ) :IndexBuffer[R] = {
    ref.factory.mkDataBuffer(size)
  }

  def apply[R <: ReadableIndex](vals: Int*)(
    implicit ref: FactoryRef[Int1, R]
  ) :IndexBuffer[R] = {
    val data = ref.factory.mkDataBuffer(vals.size)
    data.put(vals)
    data
  }

  def apply[R <: ReadableIndex](db: DataBuffer[_, _])(
    implicit ref: FactoryRef[Int1, R]
  ) :IndexBuffer[R] = {
    if (db.isReadOnly) throw new IllegalArgumentException(
      "The argument must not be read only."
    )

    ref.factory.mkDataBuffer(db.sharedBuffer)
  }

  def apply[R <: ReadableIndex](db: inDataBuffer[_, _])(
    implicit ref: FactoryRef[Int1, R]
  ) :ReadIndexBuffer[R] = {
    val res = ref.factory.mkDataBuffer(db.sharedBuffer)
    if (db.isReadOnly) res.asReadOnlySeq() else res
  }
}
