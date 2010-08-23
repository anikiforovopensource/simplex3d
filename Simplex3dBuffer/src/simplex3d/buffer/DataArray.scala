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
trait ReadDataArray[E <: MetaElement, +R <: RawData]
extends ReadDataSeq[E, R] with ReadContiguousSeq[E, R] {
  type BackingSeqType <: ReadDataArray[E#Component, R]

  protected def mkReadOnlyInstance() :ReadDataArray[E, R]
  def asReadOnlySeq() :ReadDataArray[E, R] = {
    asReadOnlySeqImpl().asInstanceOf[ReadDataArray[E, R]]
  }

  final def sharesMemory(seq: inDataSeq[_ <: MetaElement, _ <: RawData]) = {
    seq match {
      case a: ReadDataArray[_, _] =>
        sharedArray eq a.sharedArray
      case _ =>
        false
    }
  }
}

trait DataArray[E <: MetaElement, +R <: RawData]
extends DataSeq[E, R] with ContiguousSeq[E, R] with ReadDataArray[E, R] {
  def array: R#ArrayType = buffer.array.asInstanceOf[R#ArrayType]
  type BackingSeqType = DataArray[E#Component, R @uncheckedVariance]
}


object ReadDataArray {
  def apply[E <: MetaElement, R <: ReadableData](da: ReadDataArray[_, R])(
    implicit ref: FactoryRef[E, R]
  ) :ReadDataArray[E, R] = {
    val res = ref.factory.mkReadDataArray(da.sharedArray)
    if (da.isReadOnly) res.asReadOnlySeq() else res
  }
}

object DataArray {
  def apply[E <: MetaElement, R <: ReadableData](array: R#ArrayType)(
    implicit ref: FactoryRef[E, R]
  ) :DataArray[E, R] = {
    ref.factory.mkDataArray(array)
  }

  def apply[E <: MetaElement, R <: ReadableData](size: Int)(
    implicit ref: FactoryRef[E, R]
  ) :DataArray[E, R] = {
    ref.factory.mkDataArray(size)
  }

  def apply[E <: MetaElement, R <: ReadableData](vals: E#Element*)(
    implicit ref: FactoryRef[E, R]
  ) :DataArray[E, R] = {
    val data = ref.factory.mkDataArray(vals.size)
    data.put(vals)
    data
  }

  def apply[E <: MetaElement, R <: ReadableData](da: DataArray[_, R])(
    implicit ref: FactoryRef[E, R]
  ) :DataArray[E, R] = {
    if (da.isReadOnly) throw new ClassCastException(
      "The DataArray must not be read-only."
    )
    ref.factory.mkDataArray(da.sharedArray)
  }
}
