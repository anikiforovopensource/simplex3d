/*
 * Simplex3d, IntBuffer module
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
import simplex3d.buffer.intm._


/**
 * @author Aleksey Nikiforov (lex)
 */
package object intm {
  
  // Int
  // Array Int
  implicit val arrayInt1SByte = (
    (array: Array[Byte]) => new ArrayInt1SByte(array), 1, classOf[SByte]
  )
  implicit val arrayInt1SShort = (
    (array: Array[Short]) => new ArrayInt1SShort(array), 1, classOf[SShort]
  )
  implicit val arrayInt1SInt = (
    (array: Array[Int]) => new ArrayInt1SInt(array), 1, classOf[SInt]
  )


  // Buffer Int
  implicit val bufferInt1SByte = (
    (buffer: ByteBuffer) => new BufferInt1SByte(buffer), 1, classOf[SByte]
  )
  implicit val bufferInt1SShort = (
    (buffer: ByteBuffer) => new BufferInt1SShort(buffer), 1, classOf[SShort]
  )
  implicit val bufferInt1SInt = (
    (buffer: ByteBuffer) => new BufferInt1SInt(buffer), 1, classOf[SInt]
  )


  // View Int
  implicit val viewInt1SByte = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewInt1SByte(buffer, offset, stride), classOf[SByte]
  )
  implicit val viewInt1UByte = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewInt1UByte(buffer, offset, stride), classOf[UByte]
  )

  implicit val viewInt1SShort = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewInt1SShort(buffer, offset, stride), classOf[SShort]
  )
  implicit val viewInt1UShort = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewInt1UShort(buffer, offset, stride), classOf[UShort]
  )

  implicit val viewInt1SInt = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewInt1SInt(buffer, offset, stride), classOf[SInt]
  )
  implicit val viewInt1UInt = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewInt1UInt(buffer, offset, stride), classOf[UInt]
  )


  // Vec2i
  // Array Vec2i
  implicit val arrayVec2iSByte = (
    (array: Array[Byte]) => new ArrayVec2i(new ArrayInt1SByte(array)),
    2, classOf[SByte]
  )
  implicit val arrayVec2iUByte = (
    (array: Array[Byte]) => new ArrayVec2i(new ArrayInt1UByte(array)),
    2, classOf[UByte]
  )

  implicit val arrayVec2iSShort = (
    (array: Array[Short]) => new ArrayVec2i(new ArrayInt1SShort(array)),
    2, classOf[SShort]
  )
  implicit val arrayVec2iUShort = (
    (array: Array[Char]) => new ArrayVec2i(new ArrayInt1UShort(array)),
    2, classOf[UShort]
  )

  implicit val arrayVec2iSInt = (
    (array: Array[Int]) => new ArrayVec2i(new ArrayInt1SInt(array)),
    2, classOf[SInt]
  )
  implicit val arrayVec2iUInt = (
    (array: Array[Int]) => new ArrayVec2i(new ArrayInt1UInt(array)),
    2, classOf[UInt]
  )
  

  // Buffer Vec2i
  implicit val bufferVec2iSByte = (
    (buffer: ByteBuffer) => new BufferVec2i(new BufferInt1SByte(buffer)),
    2, classOf[SByte]
  )
  implicit val bufferVec2iUByte = (
    (buffer: ByteBuffer) => new BufferVec2i(new BufferInt1UByte(buffer)),
    2, classOf[UByte]
  )
  
  implicit val bufferVec2iSShort = (
    (buffer: ByteBuffer) => new BufferVec2i(new BufferInt1SShort(buffer)),
    2, classOf[SShort]
  )
    implicit val bufferVec2iUShort = (
    (buffer: ByteBuffer) => new BufferVec2i(new BufferInt1UShort(buffer)),
    2, classOf[UShort]
  )
  
  implicit val bufferVec2iSInt = (
    (buffer: ByteBuffer) => new BufferVec2i(new BufferInt1SInt(buffer)),
    2, classOf[SInt]
  )
  implicit val bufferVec2iUInt = (
    (buffer: ByteBuffer) => new BufferVec2i(new BufferInt1UInt(buffer)),
    2, classOf[UInt]
  )
  

  // View Vec2i
  implicit val viewVec2iSByte = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec2i(new BufferInt1SByte(buffer), offset, stride), classOf[SByte]
  )
  implicit val viewVec2iUByte = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec2i(new BufferInt1UByte(buffer), offset, stride), classOf[UByte]
  )
  
  implicit val viewVec2iSShort = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec2i(new BufferInt1SShort(buffer), offset, stride), classOf[SShort]
  )
  implicit val viewVec2iUShort = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec2i(new BufferInt1UShort(buffer), offset, stride), classOf[UShort]
  )
  
  implicit val viewVec2iSInt = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec2i(new BufferInt1SInt(buffer), offset, stride), classOf[SInt]
  )
  implicit val viewVec2iUInt = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec2i(new BufferInt1UInt(buffer), offset, stride), classOf[UInt]
  )
  

  // Vec3i
  // Array Vec3i
  implicit val arrayVec3iSByte = (
    (array: Array[Byte]) => new ArrayVec3i(new ArrayInt1SByte(array)),
    3, classOf[SByte]
  )
  implicit val arrayVec3iUByte = (
    (array: Array[Byte]) => new ArrayVec3i(new ArrayInt1UByte(array)),
    3, classOf[UByte]
  )

  implicit val arrayVec3iSShort = (
    (array: Array[Short]) => new ArrayVec3i(new ArrayInt1SShort(array)),
    3, classOf[SShort]
  )
  implicit val arrayVec3iUShort = (
    (array: Array[Char]) => new ArrayVec3i(new ArrayInt1UShort(array)),
    3, classOf[UShort]
  )

  implicit val arrayVec3iSInt = (
    (array: Array[Int]) => new ArrayVec3i(new ArrayInt1SInt(array)),
    3, classOf[SInt]
  )
  implicit val arrayVec3iUInt = (
    (array: Array[Int]) => new ArrayVec3i(new ArrayInt1UInt(array)),
    3, classOf[UInt]
  )


  // Buffer Vec3i
  implicit val bufferVec3iSByte = (
    (buffer: ByteBuffer) => new BufferVec3i(new BufferInt1SByte(buffer)),
    3, classOf[SByte]
  )
  implicit val bufferVec3iUByte = (
    (buffer: ByteBuffer) => new BufferVec3i(new BufferInt1UByte(buffer)),
    3, classOf[UByte]
  )

  implicit val bufferVec3iSShort = (
    (buffer: ByteBuffer) => new BufferVec3i(new BufferInt1SShort(buffer)),
    3, classOf[SShort]
  )
    implicit val bufferVec3iUShort = (
    (buffer: ByteBuffer) => new BufferVec3i(new BufferInt1UShort(buffer)),
    3, classOf[UShort]
  )

  implicit val bufferVec3iSInt = (
    (buffer: ByteBuffer) => new BufferVec3i(new BufferInt1SInt(buffer)),
    3, classOf[SInt]
  )
  implicit val bufferVec3iUInt = (
    (buffer: ByteBuffer) => new BufferVec3i(new BufferInt1UInt(buffer)),
    3, classOf[UInt]
  )


  // View Vec3i
  implicit val viewVec3iSByte = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec3i(new BufferInt1SByte(buffer), offset, stride), classOf[SByte]
  )
  implicit val viewVec3iUByte = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec3i(new BufferInt1UByte(buffer), offset, stride), classOf[UByte]
  )

  implicit val viewVec3iSShort = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec3i(new BufferInt1SShort(buffer), offset, stride), classOf[SShort]
  )
  implicit val viewVec3iUShort = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec3i(new BufferInt1UShort(buffer), offset, stride), classOf[UShort]
  )

  implicit val viewVec3iSInt = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec3i(new BufferInt1SInt(buffer), offset, stride), classOf[SInt]
  )
  implicit val viewVec3iUInt = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec3i(new BufferInt1UInt(buffer), offset, stride), classOf[UInt]
  )


  // Vec4i
  // Array Vec4i
  implicit val arrayVec4iSByte = (
    (array: Array[Byte]) => new ArrayVec4i(new ArrayInt1SByte(array)),
    4, classOf[SByte]
  )
  implicit val arrayVec4iUByte = (
    (array: Array[Byte]) => new ArrayVec4i(new ArrayInt1UByte(array)),
    4, classOf[UByte]
  )

  implicit val arrayVec4iSShort = (
    (array: Array[Short]) => new ArrayVec4i(new ArrayInt1SShort(array)),
    4, classOf[SShort]
  )
  implicit val arrayVec4iUShort = (
    (array: Array[Char]) => new ArrayVec4i(new ArrayInt1UShort(array)),
    4, classOf[UShort]
  )

  implicit val arrayVec4iSInt = (
    (array: Array[Int]) => new ArrayVec4i(new ArrayInt1SInt(array)),
    4, classOf[SInt]
  )
  implicit val arrayVec4iUInt = (
    (array: Array[Int]) => new ArrayVec4i(new ArrayInt1UInt(array)),
    4, classOf[UInt]
  )


  // Buffer Vec4i
  implicit val bufferVec4iSByte = (
    (buffer: ByteBuffer) => new BufferVec4i(new BufferInt1SByte(buffer)),
    4, classOf[SByte]
  )
  implicit val bufferVec4iUByte = (
    (buffer: ByteBuffer) => new BufferVec4i(new BufferInt1UByte(buffer)),
    4, classOf[UByte]
  )

  implicit val bufferVec4iSShort = (
    (buffer: ByteBuffer) => new BufferVec4i(new BufferInt1SShort(buffer)),
    4, classOf[SShort]
  )
    implicit val bufferVec4iUShort = (
    (buffer: ByteBuffer) => new BufferVec4i(new BufferInt1UShort(buffer)),
    4, classOf[UShort]
  )

  implicit val bufferVec4iSInt = (
    (buffer: ByteBuffer) => new BufferVec4i(new BufferInt1SInt(buffer)),
    4, classOf[SInt]
  )
  implicit val bufferVec4iUInt = (
    (buffer: ByteBuffer) => new BufferVec4i(new BufferInt1UInt(buffer)),
    4, classOf[UInt]
  )


  // View Vec4i
  implicit val viewVec4iSByte = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec4i(new BufferInt1SByte(buffer), offset, stride), classOf[SByte]
  )
  implicit val viewVec4iUByte = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec4i(new BufferInt1UByte(buffer), offset, stride), classOf[UByte]
  )

  implicit val viewVec4iSShort = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec4i(new BufferInt1SShort(buffer), offset, stride), classOf[SShort]
  )
  implicit val viewVec4iUShort = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec4i(new BufferInt1UShort(buffer), offset, stride), classOf[UShort]
  )

  implicit val viewVec4iSInt = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec4i(new BufferInt1SInt(buffer), offset, stride), classOf[SInt]
  )
  implicit val viewVec4iUInt = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec4i(new BufferInt1UInt(buffer), offset, stride), classOf[UInt]
  )
}
