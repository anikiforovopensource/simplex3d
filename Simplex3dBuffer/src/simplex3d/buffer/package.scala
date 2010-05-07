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

package simplex3d

import java.nio._
import simplex3d.buffer._


/**
 * @author Aleksey Nikiforov (lex)
 */
package object buffer {
  implicit val arrayInt1UByte = (
    (array: Array[Byte]) => new ArrayInt1UByte(array), 1, classOf[UByte]
  )
  implicit val arrayInt1UShort = (
    (array: Array[Char]) => new ArrayInt1UShort(array), 1, classOf[UShort]
  )
  implicit val arrayInt1UInt = (
    (array: Array[Int]) => new ArrayInt1UInt(array), 1, classOf[UInt]
  )

  implicit val bufferInt1UByte = (
    (buffer: ByteBuffer) => new BufferInt1UByte(buffer), 1, classOf[UByte]
  )
  implicit val bufferInt1UShort = (
    (buffer: ByteBuffer) => new BufferInt1UShort(buffer), 1, classOf[UShort]
  )
  implicit val bufferInt1UInt = (
    (buffer: ByteBuffer) => new BufferInt1UInt(buffer), 1, classOf[UInt]
  )
}
