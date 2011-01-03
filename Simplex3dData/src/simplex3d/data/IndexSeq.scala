/*
 * Simplex3d, CoreData module
 * Copyright (C) 2010-2011, Simplex3d Team
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

package simplex3d.data

import java.nio._
import scala.annotation.unchecked._


/**
 * @author Aleksey Nikiforov (lex)
 */
trait ReadIndexSeq[+R <: Unsigned] extends ReadContiguous[SInt, R]
with IndexFactory[R] {
  override def asReadOnly() :ReadIndexSeq[R]

  def copyAsIndexArray() :IndexArray[R] = copyAsDataArray()
  def copyAsIndexBuffer() :IndexBuffer[R] = copyAsDataBuffer()
}

trait IndexSeq[+R <: Unsigned]
extends Contiguous[SInt, R] with ReadIndexSeq[R]


trait ReadIndexArray[+R <: Unsigned]
extends ReadIndexSeq[R] with ReadDataArray[SInt, R] {
  override def asReadOnly() = readOnlySeq.asInstanceOf[ReadIndexArray[R]]
}

trait IndexArray[+R <: Unsigned]
extends IndexSeq[R] with DataArray[SInt, R] with ReadIndexArray[R]


trait ReadIndexBuffer[+R <: Unsigned]
extends ReadIndexSeq[R] with ReadDataBuffer[SInt, R] {
  override def asReadOnly() = readOnlySeq.asInstanceOf[ReadIndexBuffer[R]]
}

trait IndexBuffer[+R <: Unsigned]
extends IndexSeq[R] with DataBuffer[SInt, R] with ReadIndexBuffer[R]


object ReadIndexArray {
  def apply[R <: DefinedIndex](da: ReadDataArray[_, R])(
    implicit factory: PrimitiveFactory[SInt, R]
  ) :ReadIndexArray[R] = {
    val res = factory.mkDataArray(da.sharedArray)
    if (da.readOnly) res.asReadOnly() else res
  }
}

object IndexArray {
  def apply[R <: DefinedIndex](array: R#Array)(
    implicit factory: PrimitiveFactory[SInt, R]
  ) :IndexArray[R] = {
    factory.mkDataArray(array)
  }

  def apply[R <: DefinedIndex](size: Int)(
    implicit factory: PrimitiveFactory[SInt, R]
  ) :IndexArray[R] = {
    factory.mkDataArray(size)
  }

  def apply(indexSize: Int, dataSize: Int) :IndexArray[Unsigned] = {
    if (dataSize < 0) throw new IllegalArgumentException("dataSize must be non-negative.")

    if (dataSize <= 256) FactorySIntUByte.mkDataArray(indexSize)
    else if (dataSize <= 65536) FactorySIntUShort.mkDataArray(indexSize)
    else FactorySIntUInt.mkDataArray(indexSize)
  }

  def apply[R <: DefinedIndex](vals: Int*)(
    implicit factory: PrimitiveFactory[SInt, R]
  ) :IndexArray[R] = {
    val data = factory.mkDataArray(vals.size)
    data.put(vals)
    data
  }

  def apply[R <: DefinedIndex](da: DataArray[_, R])(
    implicit factory: PrimitiveFactory[SInt, R]
  ) :IndexArray[R] = {
    if (da.readOnly) throw new IllegalArgumentException(
      "The DataArray must not be read-only."
    )
    factory.mkDataArray(da.array)
  }
}

object ReadIndexBuffer {
  def apply[R <: DefinedIndex](buffer: ByteBuffer)(
    implicit factory: PrimitiveFactory[SInt, R]
  ) :ReadIndexBuffer[R] = {
    factory.mkReadDataBuffer(buffer)
  }

  def apply[R <: DefinedIndex](db: ReadDataBuffer[_, _])(
    implicit factory: PrimitiveFactory[SInt, R]
  ) :ReadIndexBuffer[R] = {
    val res = factory.mkReadDataBuffer(db.sharedBuffer)
    if (db.readOnly) res.asReadOnly() else res
  }
}

object IndexBuffer {
  def apply[R <: DefinedIndex](buffer: ByteBuffer)(
    implicit factory: PrimitiveFactory[SInt, R]
  ) :IndexBuffer[R] = {
    factory.mkDataBuffer(buffer)
  }

  def apply[R <: DefinedIndex](size: Int)(
    implicit factory: PrimitiveFactory[SInt, R]
  ) :IndexBuffer[R] = {
    factory.mkDataBuffer(size)
  }

  def apply(indexSize: Int, dataSize: Int) :IndexBuffer[Unsigned] = {
    if (dataSize < 0) throw new IllegalArgumentException("dataSize must be non-negative.")

    if (dataSize <= 256) FactorySIntUByte.mkDataBuffer(indexSize)
    else if (dataSize <= 65536) FactorySIntUShort.mkDataBuffer(indexSize)
    else FactorySIntUInt.mkDataBuffer(indexSize)
  }

  def apply[R <: DefinedIndex](vals: Int*)(
    implicit factory: PrimitiveFactory[SInt, R]
  ) :IndexBuffer[R] = {
    val data = factory.mkDataBuffer(vals.size)
    data.put(vals)
    data
  }

  def apply[R <: DefinedIndex](db: DataBuffer[_, _])(
    implicit factory: PrimitiveFactory[SInt, R]
  ) :IndexBuffer[R] = {
    if (db.readOnly) throw new IllegalArgumentException(
      "The DataBuffer must not be read-only."
    )
    factory.mkDataBuffer(db.sharedBuffer)
  }
}
