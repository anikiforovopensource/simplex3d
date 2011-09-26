/*
 * Simplex3d, CoreData module
 * Copyright (C) 2010-2011, Aleksey Nikiforov
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
import simplex3d.data.common._


/**
 * @author Aleksey Nikiforov (lex)
 */
@SerialVersionUID(8104346712419693669L)
trait ReadDataArray[F <: Format, +R <: Raw]
extends ReadDataSeq[F, R] with ReadContiguous[F, R] with ArraySrc with Serializable {
  type Read <: ReadDataArray[F, R]

  type PrimitiveSeq <: ReadDataArray[F#Component, R]
  type BindingBuffer = Buffer
}

trait DataArray[F <: Format, +R <: Raw]
extends DataSeq[F, R] with Contiguous[F, R] with ReadDataArray[F, R] {
  def array: R#Array = buff.array.asInstanceOf[R#Array]
  type PrimitiveSeq = DataArray[F#Component, R @uncheckedVariance]
}


object ReadDataArray {
  def apply[F <: Format, R <: Defined](da: ReadDataArray[_, R])(
    implicit composition: CompositionFactory[F, _ >: R], primitives: PrimitiveFactory[F#Component, R]
  ) :ReadDataArray[F, R] = {
    val res = composition.mkDataArray(primitives.mkDataArray(da.sharedStorage.asInstanceOf[R#Array]))
    if (da.isReadOnly) res.asReadOnly() else res
  }
}

object DataArray {
  def apply[F <: Format, R <: Defined](array: R#Array)(
    implicit composition: CompositionFactory[F, _ >: R], primitives: PrimitiveFactory[F#Component, R]
  ) :DataArray[F, R] = {
    composition.mkDataArray(primitives.mkDataArray(array))
  }

  def apply[F <: Format, R <: Defined](size: Int)(
    implicit composition: CompositionFactory[F, _ >: R], primitives: PrimitiveFactory[F#Component, R]
  ) :DataArray[F, R] = {
    composition.mkDataArray(primitives.mkDataArray(size*composition.components))
  }

  def apply[F <: Format, R <: Defined](vals: F#Accessor#Read*)(
    implicit composition: CompositionFactory[F, _ >: R], primitives: PrimitiveFactory[F#Component, R]
  ) :DataArray[F, R] = {
    val data = composition.mkDataArray(primitives.mkDataArray(vals.size*composition.components))
    data.put(vals)
    data
  }

  def apply[F <: Format, R <: Defined](da: DataArray[_, R])(
    implicit composition: CompositionFactory[F, _ >: R], primitives: PrimitiveFactory[F#Component, R]
  ) :DataArray[F, R] = {
    if (da.isReadOnly) throw new IllegalArgumentException(
      "The DataArray must not be read-only."
    )
    composition.mkDataArray(primitives.mkDataArray(da.sharedStorage.asInstanceOf[R#Array]))
  }
}
