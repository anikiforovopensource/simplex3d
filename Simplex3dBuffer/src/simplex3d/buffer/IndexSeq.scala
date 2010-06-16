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
import simplex3d.math._


/**
 * @author Aleksey Nikiforov (lex)
 */
trait ReadOnlyIndexSeq[+R <: ReadableIndex]
extends ReadOnlyContiguousSeq[Int1, R] {
  def asReadOnly() :ReadOnlyIndexSeq[R]

  def mkIndexArray(size: Int) :IndexArray[R] =
    mkDataArray(size).asInstanceOf[IndexArray[R]]
  def mkIndexArray(array: R#ArrayType @uncheckedVariance) :IndexArray[R] =
    mkDataArray(array).asInstanceOf[IndexArray[R]]
  def mkIndexBuffer(size: Int) :IndexBuffer[R] =
    mkDataBuffer(size).asInstanceOf[IndexBuffer[R]]
  def mkIndexBuffer(byteBuffer: ByteBuffer) :IndexBuffer[R] =
    mkDataBuffer(byteBuffer).asInstanceOf[IndexBuffer[R]]

  def copyAsIndexArray() :IndexArray[R] =
    super.copyAsDataArray().asInstanceOf[IndexArray[R]]
  def copyAsIndexBuffer() :IndexBuffer[R] =
    super.copyAsDataBuffer().asInstanceOf[IndexBuffer[R]]
}

trait IndexSeq[+R <: ReadableIndex]
extends ContiguousSeq[Int1, R] with ReadOnlyIndexSeq[R]


trait ReadOnlyIndexArray[+R <: ReadableIndex]
extends ReadOnlyIndexSeq[R] with ReadOnlyDataArray[Int1, R] {
  def asReadOnly() :ReadOnlyIndexArray[R]
}

trait IndexArray[+R <: ReadableIndex]
extends IndexSeq[R] with DataArray[Int1, R] with ReadOnlyIndexArray[R]


trait ReadOnlyIndexBuffer[+R <: ReadableIndex]
extends ReadOnlyIndexSeq[R] with ReadOnlyDataBuffer[Int1, R] {
  def asReadOnly() :ReadOnlyIndexBuffer[R]
}

trait IndexBuffer[+R <: ReadableIndex]
extends IndexSeq[R] with DataBuffer[Int1, R] with ReadOnlyIndexBuffer[R]


object IndexArray {
  def apply[R <: ReadableIndex](array: R#ArrayType)(
    implicit ref: FactoryRef[Int1, R]
  ) :IndexArray[R] = {
    ref.factory.mkDataArray(array).asInstanceOf[IndexArray[R]]
  }

  def apply[R <: ReadableIndex](size: Int)(
    implicit ref: FactoryRef[Int1, R]
  ) :IndexArray[R] = {
    ref.factory.mkDataArray(size).asInstanceOf[IndexArray[R]]
  }

  def apply[R <: ReadableIndex](vals: Int*)(
    implicit ref: FactoryRef[Int1, R]
  ) :IndexArray[R] = {
    val data = ref.factory.mkDataArray(vals.size).asInstanceOf[IndexArray[R]]
    data.put(vals)
    data
  }
}

object IndexBuffer {
  def apply[R <: ReadableIndex](buffer: ByteBuffer)(
    implicit ref: FactoryRef[Int1, R]
  ) :IndexBuffer[R] = {
    ref.factory.mkDataBuffer(buffer).asInstanceOf[IndexBuffer[R]]
  }

  def apply[R <: ReadableIndex](size: Int)(
    implicit ref: FactoryRef[Int1, R]
  ) :IndexBuffer[R] = {
    ref.factory.mkDataBuffer(size).asInstanceOf[IndexBuffer[R]]
  }

  def apply[R <: ReadableIndex](vals: Int*)(
    implicit ref: FactoryRef[Int1, R]
  ) :IndexBuffer[R] = {
    val data = ref.factory.mkDataBuffer(vals.size).asInstanceOf[IndexBuffer[R]]
    data.put(vals)
    data
  }
}
