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
trait ReadOnlyDataArray[T <: ElemType, +D <: RawType]
extends ReadOnlyDataSeq[T, D] with ReadOnlyContiguousSeq[T, D] {
  def backingSeq: roDataArray[T#Component, D]
  def asReadOnly() :roDataArray[T, D]

  final def sharesMemory(seq: roDataSeq[_ <: ElemType, _ <: RawType]) = {
    seq match {
      case a: roDataArray[_, _] =>
        backingSeq.readArray eq a.backingSeq.readArray
      case _ =>
        false
    }
  }
}

trait DataArray[T <: ElemType, +D <: RawType]
extends DataSeq[T, D] with ContiguousSeq[T, D] with ReadOnlyDataArray[T, D] {
  def array: D#ArrayType = buffer.array.asInstanceOf[D#ArrayType]
  def backingSeq: DataArray[T#Component, D]
}

object DataArray {
  def apply[T <: ElemType, D <: ReadableType](array: D#ArrayType)(
    implicit ref: FactoryRef[T, D]
  ) :DataArray[T, D] = {
    ref.factory.mkDataArray(array)
  }

  def apply[T <: ElemType, D <: ReadableType](size: Int)(
    implicit ref: FactoryRef[T, D]
  ) :DataArray[T, D] = {
    ref.factory.mkDataArray(size)
  }

  def apply[T <: ElemType, D <: ReadableType](vals: T#Element*)(
    implicit ref: FactoryRef[T, D]
  ) :DataArray[T, D] = {
    val data = ref.factory.mkDataArray(vals.size)
    data.put(vals)
    data
  }

  def apply[T <: ElemType, D <: ReadableType](da: DataArray[_, D])(
    implicit ref: FactoryRef[T, D]
  ) :DataArray[T, D] = {
    if (da.isReadOnly) throw new IllegalArgumentException(
      "The argument must not be read only."
    )

    ref.factory.mkDataArray(da.array)
  }

  def apply[T <: ElemType, D <: ReadableType](da: roDataArray[_, D])(
    implicit ref: FactoryRef[T, D]
  ) :ReadOnlyDataArray[T, D] = {
    ref.factory.mkDataArray(da.backingSeq.readArray).asReadOnly()
  }
}
