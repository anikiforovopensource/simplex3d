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

import java.nio._
import simplex3d.math._


/**
 * @author Aleksey Nikiforov (lex)
 */
trait IndexSeq[+D <: ReadInt with Unsigned]
extends ContiguousSeq[Int1, D]

trait IndexArray[+D <: ReadInt with Unsigned]
extends IndexSeq[D] with DataArray[Int1, D]

trait IndexBuffer[+D <: ReadInt with Unsigned]
extends IndexSeq[D] with DataBuffer[Int1, D]


object IndexArray {
  def apply[D <: ReadInt with Unsigned](array: D#ArrayType)(
    implicit ref: DataSeqFactoryRef[Int1, D]
  ) :IndexArray[D] = {
    ref.factory.mkDataArray(array).asInstanceOf[IndexArray[D]]
  }

  def apply[D <: ReadInt with Unsigned](size: Int)(
    implicit ref: DataSeqFactoryRef[Int1, D]
  ) :IndexArray[D] = {
    ref.factory.mkDataArray(size).asInstanceOf[IndexArray[D]]
  }

  def apply[D <: ReadInt with Unsigned](vals: Int*)(
    implicit ref: DataSeqFactoryRef[Int1, D]
  ) :IndexArray[D] = {
    val data = ref.factory.mkDataArray(vals.size).asInstanceOf[IndexArray[D]]
    data.put(vals)
    data
  }
}

object IndexBuffer {
  def apply[D <: ReadInt with Unsigned](buffer: ByteBuffer)(
    implicit ref: DataSeqFactoryRef[Int1, D]
  ) :IndexBuffer[D] = {
    ref.factory.mkDataBuffer(buffer).asInstanceOf[IndexBuffer[D]]
  }

  def apply[D <: ReadInt with Unsigned](size: Int)(
    implicit ref: DataSeqFactoryRef[Int1, D]
  ) :IndexBuffer[D] = {
    ref.factory.mkDataBuffer(size).asInstanceOf[IndexBuffer[D]]
  }

  def apply[D <: ReadInt with Unsigned](vals: Int*)(
    implicit ref: DataSeqFactoryRef[Int1, D]
  ) :IndexBuffer[D] = {
    val data = ref.factory.mkDataBuffer(vals.size).asInstanceOf[IndexBuffer[D]]
    data.put(vals)
    data
  }
}
