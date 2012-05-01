/*
 * Simplex3dData - Core Module
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
import simplex3d.data.extension._


/**
 * @author Aleksey Nikiforov (lex)
 */
trait ReadDataView[F <: Format, +R <: Raw]
extends ReadDataSeq[F, R] with DirectSrc {
  type Read <: ReadDataView[F, R]

  type PrimitiveSeq <: ReadDataBuffer[F#Component, R]
}

trait DataView[F <: Format, +R <: Raw]
extends DataSeq[F, R] with ReadDataView[F, R] {
  type PrimitiveSeq = DataBuffer[F#Component, R @uncheckedVariance]
}


object ReadDataView {
  def apply[F <: Format, R <: Raw with Tangible](buffer: ByteBuffer, offset: Int, stride: Int)(
    implicit composition: CompositionFactory[F, _ >: R], primitives: PrimitiveFactory[F#Component, R]
  ) :ReadDataView[F, R] = {
    composition.mkReadDataView(primitives.mkReadDataBuffer(buffer), offset, stride)
  }

  def apply[F <: Format, R <: Raw with Tangible](db: inDataBuffer[_, _], offset: Int, stride: Int)(
    implicit composition: CompositionFactory[F, _ >: R], primitives: PrimitiveFactory[F#Component, R]
  ) :ReadDataView[F, R] = {
    val res = composition.mkReadDataView(primitives.mkReadDataBuffer(db.sharedBuffer), offset, stride)
    if (db.isReadOnly) res.asReadOnly() else res
  }
}

object DataView {
  def apply[F <: Format, R <: Raw with Tangible](buffer: ByteBuffer, offset: Int, stride: Int)(
    implicit composition: CompositionFactory[F, _ >: R], primitives: PrimitiveFactory[F#Component, R]
  ) :DataView[F, R] = {
    composition.mkDataView(primitives.mkDataBuffer(buffer), offset, stride)
  }

  def apply[F <: Format, R <: Raw with Tangible](db: DataBuffer[_, _], offset: Int, stride: Int)(
    implicit composition: CompositionFactory[F, _ >: R], primitives: PrimitiveFactory[F#Component, R]
  ) :DataView[F, R] = {
    if (db.isReadOnly) throw new IllegalArgumentException(
      "The DataBuffer must not be read-only."
    )
    composition.mkDataView(primitives.mkDataBuffer(db.sharedBuffer), offset, stride)
  }
}
