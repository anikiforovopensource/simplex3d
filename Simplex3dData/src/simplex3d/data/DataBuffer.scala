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


/**
 * @author Aleksey Nikiforov (lex)
 */
trait ReadDataBuffer[F <: Meta, +R <: Raw]
extends ReadDataView[F, R] with ReadContiguous[F, R] {
  type Read <: ReadDataBuffer[F, R]
}

trait DataBuffer[F <: Meta, +R <: Raw]
extends DataView[F, R] with Contiguous[F, R] with ReadDataBuffer[F, R]


object ReadDataBuffer {
  def apply[F <: Meta, R <: Defined](buffer: ByteBuffer)(
    implicit composition: CompositionFactory[F, _ >: R], primitives: PrimitiveFactory[F#Component, R]
  ) :ReadDataBuffer[F, R] = {
    composition.mkReadDataBuffer(primitives.mkReadDataBuffer(buffer))
  }

  def apply[F <: Meta, R <: Defined](db: ReadDataBuffer[_, _])(
    implicit composition: CompositionFactory[F, _ >: R], primitives: PrimitiveFactory[F#Component, R]
  ) :ReadDataBuffer[F, R] = {
    val res = composition.mkReadDataBuffer(primitives.mkReadDataBuffer(db.sharedBuffer))
    if (db.isReadOnly) res.asReadOnly() else res
  }
}

object DataBuffer {
  def apply[F <: Meta, R <: Defined](buffer: ByteBuffer)(
    implicit composition: CompositionFactory[F, _ >: R], primitives: PrimitiveFactory[F#Component, R]
  ) :DataBuffer[F, R] = {
    composition.mkDataBuffer(primitives.mkDataBuffer(buffer))
  }

  def apply[F <: Meta, R <: Defined](size: Int)(
    implicit composition: CompositionFactory[F, _ >: R], primitives: PrimitiveFactory[F#Component, R]
  ) :DataBuffer[F, R] = {
    composition.mkDataBuffer(primitives.mkDataBuffer(size*composition.components))
  }

  def apply[F <: Meta, R <: Defined](vals: F#Read*)(
    implicit composition: CompositionFactory[F, _ >: R], primitives: PrimitiveFactory[F#Component, R]
  ) :DataBuffer[F, R] = {
    val data = composition.mkDataBuffer(primitives.mkDataBuffer(vals.size*composition.components))
    data.put(vals)
    data
  }

  def apply[F <: Meta, R <: Defined](db: DataBuffer[_, _])(
    implicit composition: CompositionFactory[F, _ >: R], primitives: PrimitiveFactory[F#Component, R]
  ) :DataBuffer[F, R] = {
    if (db.isReadOnly) throw new IllegalArgumentException(
      "The DataBuffer must not be read-only."
    )
    composition.mkDataBuffer(primitives.mkDataBuffer(db.sharedBuffer))
  }
}
