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


/**
 * @author Aleksey Nikiforov (lex)
 */
trait ReadDataSeq[E <: MetaElement, +R <: RawData]
extends ReadBaseSeq[E, E#Element, R]

trait DataSeq[E <: MetaElement, +R <: RawData]
extends BaseSeq[E, E#Element, R] with ReadDataSeq[E, R]


object DataSeq {
  def apply[E <: MetaElement, R <: ReadableData](
    implicit ref: FactoryRef[E, R]
  ) :DataSeq[E, R] = {
    ref.factory
  }
}


trait ReadContiguousSeq[E <: MetaElement, +R <: RawData]
extends ReadDataSeq[E, R] {
  assert(offset == 0)
  assert(stride == components)
}

trait ContiguousSeq[E <: MetaElement, +R <: RawData]
extends DataSeq[E, R] with ReadContiguousSeq[E, R]
