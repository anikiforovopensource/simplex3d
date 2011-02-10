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
trait ReadDataView[E <: Meta, +R <: Raw]
extends ReadDataSeq[E, R] {
  type Primitive <: ReadDataBuffer[E#Component, R]
  type RawBuffer = ByteBuffer
  override def asReadOnly() = readOnlySeq.asInstanceOf[ReadDataView[E, R]]
}

trait DataView[E <: Meta, +R <: Raw]
extends DataSeq[E, R] with ReadDataView[E, R] {
  type Primitive = DataBuffer[E#Component, R @uncheckedVariance]
}


object ReadDataView {
  def apply[E <: Meta, R <: Defined](buffer: ByteBuffer, offset: Int, stride: Int)(
    implicit composition: CompositionFactory[E, _ >: R], primitive: PrimitiveFactory[E#Component, R]
  ) :ReadDataView[E, R] = {
    composition.mkReadDataView(primitive.mkReadDataBuffer(buffer), offset, stride)
  }

  def apply[E <: Meta, R <: Defined](db: inDataBuffer[_, _], offset: Int, stride: Int)(
    implicit composition: CompositionFactory[E, _ >: R], primitive: PrimitiveFactory[E#Component, R]
  ) :ReadDataView[E, R] = {
    val res = composition.mkReadDataView(primitive.mkReadDataBuffer(db.sharedBuffer), offset, stride)
    if (db.isReadOnly) res.asReadOnly() else res
  }
}

object DataView {
  def apply[E <: Meta, R <: Defined](buffer: ByteBuffer, offset: Int, stride: Int)(
    implicit composition: CompositionFactory[E, _ >: R], primitive: PrimitiveFactory[E#Component, R]
  ) :DataView[E, R] = {
    composition.mkDataView(primitive.mkDataBuffer(buffer), offset, stride)
  }

  def apply[E <: Meta, R <: Defined](db: DataBuffer[_, _], offset: Int, stride: Int)(
    implicit composition: CompositionFactory[E, _ >: R], primitive: PrimitiveFactory[E#Component, R]
  ) :DataView[E, R] = {
    if (db.isReadOnly) throw new IllegalArgumentException(
      "The DataBuffer must not be read-only."
    )
    composition.mkDataView(primitive.mkDataBuffer(db.sharedBuffer), offset, stride)
  }
}
