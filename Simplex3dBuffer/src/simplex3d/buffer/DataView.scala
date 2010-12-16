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
trait ReadDataView[E <: Meta, +R <: Raw]
extends ReadDataSeq[E, R] {
  type Backing <: ReadDataBuffer[E#Component, R]
  type RawBuffer = ByteBuffer
  override def asReadOnly() = readOnlySeq.asInstanceOf[ReadDataView[E, R]]
}

trait DataView[E <: Meta, +R <: Raw]
extends DataSeq[E, R] with ReadDataView[E, R] {
  type Backing = DataBuffer[E#Component, R @uncheckedVariance]
}


object ReadDataView {
  def apply[E <: Meta, R <: Defined](
    buffer: ByteBuffer, offset: Int, stride: Int
  )(implicit factory: Factory[E, R]) :ReadDataView[E, R] = {
    factory.mkReadDataView(buffer, offset, stride)
  }

  def apply[E <: Meta, R <: Defined](
    db: inDataBuffer[_, _], offset: Int, stride: Int
  )(implicit factory: Factory[E, R]) :ReadDataView[E, R] = {
    val res = factory.mkReadDataView(db.sharedBuffer, offset, stride)
    if (db.readOnly) res.asReadOnly() else res
  }
}

object DataView {
  def apply[E <: Meta, R <: Defined](
    buffer: ByteBuffer, offset: Int, stride: Int
  )(implicit factory: Factory[E, R]) :DataView[E, R] = {
    factory.mkDataView(buffer, offset, stride)
  }

  def apply[E <: Meta, R <: Defined](
    db: DataBuffer[_, _], offset: Int, stride: Int
  )(implicit factory: Factory[E, R]) :DataView[E, R] = {
    if (db.readOnly) throw new IllegalArgumentException(
      "The DataBuffer must not be read-only."
    )
    factory.mkDataView(db.sharedBuffer, offset, stride)
  }
}
