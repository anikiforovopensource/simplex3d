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
trait ReadDataArray[E <: ElemType, +R <: RawType]
extends ReadDataSeq[E, R] with ReadContiguousSeq[E, R] {
  def backingSeq: ReadDataArray[E#Component, R]
  def asReadOnlySeq() :ReadDataArray[E, R]

  final def sharesMemory(seq: inDataSeq[_ <: ElemType, _ <: RawType]) = {
    seq match {
      case a: ReadDataArray[_, _] =>
        sharedArray eq a.sharedArray
      case _ =>
        false
    }
  }
}

trait DataArray[E <: ElemType, +R <: RawType]
extends DataSeq[E, R] with ContiguousSeq[E, R] with ReadDataArray[E, R] {
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
    val res = ref.factory.mkDataArray(da.sharedArray)
    if (da.isReadOnly) res.asReadOnlySeq.asInstanceOf[DataArray[E, R]] else res
  }

  def apply[E <: ElemType, R <: ReadableType](da: inDataArray[_, R])(
    implicit ref: FactoryRef[E, R]
  ) :ReadDataArray[E, R] = {
    val res = ref.factory.mkDataArray(da.sharedArray)
    if (da.isReadOnly) res.asReadOnlySeq() else res
  }
}
