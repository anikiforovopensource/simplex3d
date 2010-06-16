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
trait ReadOnlyDataArray[E <: ElemType, +R <: RawType]
extends ReadOnlyDataSeq[E, R] with ReadOnlyContiguousSeq[E, R] {
  def backingSeq: ReadOnlyDataArray[E#Component, R]
  def asReadOnly() :ReadOnlyDataArray[E, R]

  final def sharesMemory(seq: inDataSeq[_ <: ElemType, _ <: RawType]) = {
    seq match {
      case a: ReadOnlyDataArray[_, _] =>
        backingSeq.readArray eq a.backingSeq.readArray
      case _ =>
        false
    }
  }
}

trait DataArray[E <: ElemType, +R <: RawType]
extends DataSeq[E, R] with ContiguousSeq[E, R] with ReadOnlyDataArray[E, R] {
  def array: R#ArrayType = buffer.array.asInstanceOf[R#ArrayType]
  def backingSeq: DataArray[E#Component, R]
}

object DataArray {
  def apply[E <: ElemType, R <: ReadableType](array: R#ArrayType)(
    implicit ref: FactoryRef[E, R]
  ) :DataArray[E, R] = {
    ref.factory.mkDataArray(array)
  }

  def apply[E <: ElemType, R <: ReadableType](size: Int)(
    implicit ref: FactoryRef[E, R]
  ) :DataArray[E, R] = {
    ref.factory.mkDataArray(size)
  }

  def apply[E <: ElemType, R <: ReadableType](vals: E#Element*)(
    implicit ref: FactoryRef[E, R]
  ) :DataArray[E, R] = {
    val data = ref.factory.mkDataArray(vals.size)
    data.put(vals)
    data
  }

  def apply[E <: ElemType, R <: ReadableType](da: DataArray[_, R])(
    implicit ref: FactoryRef[E, R]
  ) :DataArray[E, R] = {
    if (da.isReadOnly) throw new IllegalArgumentException(
      "The argument must not be read only."
    )

    ref.factory.mkDataArray(da.array)
  }

  def apply[E <: ElemType, R <: ReadableType](da: inDataArray[_, R])(
    implicit ref: FactoryRef[E, R]
  ) :ReadOnlyDataArray[E, R] = {
    val res = ref.factory.mkDataArray(da.backingSeq.readArray)
    if (da.isReadOnly) res.asReadOnly() else res
  }
}
