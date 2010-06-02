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
import scala.annotation.unchecked._
import simplex3d.math._


/**
 * @author Aleksey Nikiforov (lex)
 */
trait ReadOnlyIndexSeq[+D <: ReadableIndex]
extends ReadOnlyContiguousSeq[Int1, D] {
  def asReadOnly() :ReadOnlyIndexSeq[D]

  def mkIndexArray(size: Int) :IndexArray[D] =
    mkDataArray(size).asInstanceOf[IndexArray[D]]
  def mkIndexArray(array: D#ArrayType @uncheckedVariance) :IndexArray[D] =
    mkDataArray(array).asInstanceOf[IndexArray[D]]
  def mkIndexBuffer(size: Int) :IndexBuffer[D] =
    mkDataBuffer(size).asInstanceOf[IndexBuffer[D]]
  def mkIndexBuffer(byteBuffer: ByteBuffer) :IndexBuffer[D] =
    mkDataBuffer(byteBuffer).asInstanceOf[IndexBuffer[D]]

  def copyAsIndexArray() :IndexArray[D] =
    super.copyAsDataArray().asInstanceOf[IndexArray[D]]
  def copyAsIndexBuffer() :IndexBuffer[D] =
    super.copyAsDataBuffer().asInstanceOf[IndexBuffer[D]]
}

trait IndexSeq[+D <: ReadableIndex]
extends ContiguousSeq[Int1, D] with ReadOnlyIndexSeq[D]


trait ReadOnlyIndexArray[+D <: ReadableIndex]
extends ReadOnlyIndexSeq[D] with ReadOnlyDataArray[Int1, D] {
  def asReadOnly() :ReadOnlyIndexArray[D]
}

trait IndexArray[+D <: ReadableIndex]
extends IndexSeq[D] with DataArray[Int1, D] with ReadOnlyIndexArray[D]


trait ReadOnlyIndexBuffer[+D <: ReadableIndex]
extends ReadOnlyIndexSeq[D] with ReadOnlyDataBuffer[Int1, D] {
  def asReadOnly() :ReadOnlyIndexBuffer[D]
}

trait IndexBuffer[+D <: ReadableIndex]
extends IndexSeq[D] with DataBuffer[Int1, D] with ReadOnlyIndexBuffer[D]


object IndexArray {
  def apply[D <: ReadableIndex](array: D#ArrayType)(
    implicit ref: FactoryRef[Int1, D]
  ) :IndexArray[D] = {
    ref.factory.mkDataArray(array).asInstanceOf[IndexArray[D]]
  }

  def apply[D <: ReadableIndex](size: Int)(
    implicit ref: FactoryRef[Int1, D]
  ) :IndexArray[D] = {
    ref.factory.mkDataArray(size).asInstanceOf[IndexArray[D]]
  }

  def apply[D <: ReadableIndex](vals: Int*)(
    implicit ref: FactoryRef[Int1, D]
  ) :IndexArray[D] = {
    val data = ref.factory.mkDataArray(vals.size).asInstanceOf[IndexArray[D]]
    data.put(vals)
    data
  }
}

object IndexBuffer {
  def apply[D <: ReadableIndex](buffer: ByteBuffer)(
    implicit ref: FactoryRef[Int1, D]
  ) :IndexBuffer[D] = {
    ref.factory.mkDataBuffer(buffer).asInstanceOf[IndexBuffer[D]]
  }

  def apply[D <: ReadableIndex](size: Int)(
    implicit ref: FactoryRef[Int1, D]
  ) :IndexBuffer[D] = {
    ref.factory.mkDataBuffer(size).asInstanceOf[IndexBuffer[D]]
  }

  def apply[D <: ReadableIndex](vals: Int*)(
    implicit ref: FactoryRef[Int1, D]
  ) :IndexBuffer[D] = {
    val data = ref.factory.mkDataBuffer(vals.size).asInstanceOf[IndexBuffer[D]]
    data.put(vals)
    data
  }
}
