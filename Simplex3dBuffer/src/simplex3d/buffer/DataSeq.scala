/*
 * Simplex3d, BaseBuffer module
 * Copyright (C) 2010 Simplex3d Team
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

import simplex3d.math._


/**
 * @author Aleksey Nikiforov (lex)
 */
trait ReadOnlyDataSeq[T <: ElemType, +D <: RawType]
extends ReadOnlyBaseSeq[T, T#Element, D]

trait DataSeq[T <: ElemType, +D <: RawType]
extends BaseSeq[T, T#Element, D] with ReadOnlyDataSeq[T, D]

object DataSeq {
  def apply[T <: ElemType, D <: ReadableType](
    implicit ref: FactoryRef[T, D]
  ) :DataSeq[T, D] = {
    ref.factory
  }
}

trait ReadOnlyContiguousSeq[T <: ElemType, +D <: RawType]
extends ReadOnlyDataSeq[T, D] {
  assert(offset == 0)
  assert(stride == components)
}

trait ContiguousSeq[T <: ElemType, +D <: RawType]
extends DataSeq[T, D] with ReadOnlyContiguousSeq[T, D]
