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
trait IndexSeq[+D <: Unsigned with NonNormalized] extends ContiguousSeq[Int1, D]
trait IndexArray[+D <: Unsigned with NonNormalized] extends IndexSeq[D]
trait IndexBuffer[+D <: Unsigned with NonNormalized] extends IndexSeq[D]


object IndexArray {
  def apply[D <: Unsigned with NonNormalized](array: Array[D#ArrayType])(
    implicit t: ((Array[D#ArrayType]) => IndexArray[D], Int, Class[D])
  ) :IndexArray[D] = {
    t._1(array)
  }

  def apply[D <: Unsigned with NonNormalized](size: Int)(
    implicit t: ((Array[D#ArrayType]) => IndexArray[D], Int, Class[D])
  ) :IndexArray[D] = {
    def cast(a: Array[_]) = a.asInstanceOf[Array[D#ArrayType]]

    t._3 match {
      case ReadAs.UByte => t._1(cast(new Array[Byte](size)))
      case ReadAs.UShort => t._1(cast(new Array[Char](size)))
      case ReadAs.UInt => t._1(cast(new Array[Int](size)))

      case _ => throw new AssertionError("Type not found.")
    }
  }
}

object IndexBuffer {
  def apply[D <: Unsigned with NonNormalized](buffer: ByteBuffer)(
    implicit t: ((ByteBuffer) => IndexBuffer[D], Int, Class[D])
  ) :IndexBuffer[D] = {
    t._1(buffer)
  }

  def apply[D <: Unsigned with NonNormalized](size: Int)(
    implicit t: ((ByteBuffer) => IndexBuffer[D], Int, Class[D])
  ) :IndexBuffer[D] = {
    def alloc(size: Int) = BufferUtil.allocateByteBuffer(size)

    t._3 match {
      case ReadAs.UByte => t._1(alloc(size))
      case ReadAs.UShort => t._1(alloc(size*2))
      case ReadAs.UInt => t._1(alloc(size*4))

      case _ => throw new AssertionError("Type not found.")
    }
  }
}
