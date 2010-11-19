/*
 * Simplex3d, CoreBuffer module
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
@serializable @SerialVersionUID(8104346712419693669L)
trait ReadDataArray[E <: MetaElement, +R <: RawData]
extends ReadDataSeq[E, R] with ReadContiguousSeq[E, R] {
  type BackingSeq <: ReadDataArray[E#Component, R]
  type RawBuffer = Buffer
  override def asReadOnlySeq() = readOnlySeq.asInstanceOf[ReadDataArray[E, R]]
}

trait DataArray[E <: MetaElement, +R <: RawData]
extends DataSeq[E, R] with ContiguousSeq[E, R] with ReadDataArray[E, R] {
  def array: R#ArrayType = buff.array.asInstanceOf[R#ArrayType]
  type BackingSeq = DataArray[E#Component, R @uncheckedVariance]
}


object ReadDataArray {
  def apply[E <: MetaElement, R <: Defined](da: ReadDataArray[_, R])(
    implicit factory: DataSeqFactory[E, R]
  ) :ReadDataArray[E, R] = {
    val res = factory.mkDataArray(da.sharedArray)
    if (da.readOnly) res.asReadOnlySeq() else res
  }
}

object DataArray {
  def apply[E <: MetaElement, R <: Defined](array: R#ArrayType)(
    implicit factory: DataSeqFactory[E, R]
  ) :DataArray[E, R] = {
    factory.mkDataArray(array)
  }

  def apply[E <: MetaElement, R <: Defined](size: Int)(
    implicit factory: DataSeqFactory[E, R]
  ) :DataArray[E, R] = {
    factory.mkDataArray(size)
  }

  def apply[E <: MetaElement, R <: Defined](vals: E#Read*)(
    implicit factory: DataSeqFactory[E, R]
  ) :DataArray[E, R] = {
    val data = factory.mkDataArray(vals.size)
    data.put(vals)
    data
  }

  def apply[E <: MetaElement, R <: Defined](vals: IndexedSeq[E#Read])(
    implicit factory: DataSeqFactory[E, R]
  ) :DataArray[E, R] = {
    val data = factory.mkDataArray(vals.size)
    data.put(vals)
    data
  }

  def apply[E <: MetaElement, R <: Defined](da: DataArray[_, R])(
    implicit factory: DataSeqFactory[E, R]
  ) :DataArray[E, R] = {
    if (da.readOnly) throw new IllegalArgumentException(
      "The DataArray must not be read-only."
    )
    factory.mkDataArray(da.sharedArray)
  }
}
