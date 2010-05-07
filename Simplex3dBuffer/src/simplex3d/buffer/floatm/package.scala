/*
 * Simplex3d, FloatBuffer module
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
import simplex3d.buffer.floatm._


/**
 * @author Aleksey Nikiforov (lex)
 */
package object floatm {
  
  // Float
  // Array Float
  implicit val arrayFloat1SByte = (
    (array: Array[Byte]) => new ArrayFloat1SByte(array), 1, classOf[SByte]
  )
  implicit val arrayFloat1UByte = (
    (array: Array[Byte]) => new ArrayFloat1UByte(array), 1, classOf[UByte]
  )
  implicit val arrayFloat1NSByte = (
    (array: Array[Byte]) => new ArrayFloat1NSByte(array), 1, classOf[NSByte]
  )
  implicit val arrayFloat1NUByte = (
    (array: Array[Byte]) => new ArrayFloat1NUByte(array), 1, classOf[NUByte]
  )

  implicit val arrayFloat1SShort = (
    (array: Array[Short]) => new ArrayFloat1SShort(array), 1, classOf[SShort]
  )
  implicit val arrayFloat1NSShort = (
    (array: Array[Short]) => new ArrayFloat1NSShort(array), 1, classOf[NSShort]
  )

  implicit val arrayFloat1UShort = (
    (array: Array[Char]) => new ArrayFloat1UShort(array), 1, classOf[UShort]
  )
  implicit val arrayFloat1NUShort = (
    (array: Array[Char]) => new ArrayFloat1NUShort(array), 1, classOf[NUShort]
  )

  implicit val arrayFloat1SInt = (
    (array: Array[Int]) => new ArrayFloat1SInt(array), 1, classOf[SInt]
  )
  implicit val arrayFloat1UInt = (
    (array: Array[Int]) => new ArrayFloat1UInt(array), 1, classOf[UInt]
  )
  implicit val arrayFloat1NSInt = (
    (array: Array[Int]) => new ArrayFloat1NSInt(array), 1, classOf[NSInt]
  )
  implicit val arrayFloat1NUInt = (
    (array: Array[Int]) => new ArrayFloat1NUInt(array), 1, classOf[NUInt]
  )

  implicit val arrayFloat1RawFloat = (
    (array: Array[Float]) => new ArrayFloat1RawFloat(array), 1, classOf[RawFloat]
  )


  // Buffer Float
  implicit val bufferFloat1SByte = (
    (buffer: ByteBuffer) => new BufferFloat1SByte(buffer), 1, classOf[SByte]
  )
  implicit val bufferFloat1UByte = (
    (buffer: ByteBuffer) => new BufferFloat1UByte(buffer), 1, classOf[UByte]
  )
  implicit val bufferFloat1NSByte = (
    (buffer: ByteBuffer) => new BufferFloat1NSByte(buffer), 1, classOf[NSByte]
  )
  implicit val bufferFloat1NUByte = (
    (buffer: ByteBuffer) => new BufferFloat1NUByte(buffer), 1, classOf[NUByte]
  )

  implicit val bufferFloat1SShort = (
    (buffer: ByteBuffer) => new BufferFloat1SShort(buffer), 1, classOf[SShort]
  )
  implicit val bufferFloat1NSShort = (
    (buffer: ByteBuffer) => new BufferFloat1NSShort(buffer), 1, classOf[NSShort]
  )

  implicit val bufferFloat1UShort = (
    (buffer: ByteBuffer) => new BufferFloat1UShort(buffer), 1, classOf[UShort]
  )
  implicit val bufferFloat1NUShort = (
    (buffer: ByteBuffer) => new BufferFloat1NUShort(buffer), 1, classOf[NUShort]
  )

  implicit val bufferFloat1SInt = (
    (buffer: ByteBuffer) => new BufferFloat1SInt(buffer), 1, classOf[SInt]
  )
  implicit val bufferFloat1UInt = (
    (buffer: ByteBuffer) => new BufferFloat1UInt(buffer), 1, classOf[UInt]
  )
  implicit val bufferFloat1NSInt = (
    (buffer: ByteBuffer) => new BufferFloat1NSInt(buffer), 1, classOf[NSInt]
  )
  implicit val bufferFloat1NUInt = (
    (buffer: ByteBuffer) => new BufferFloat1NUInt(buffer), 1, classOf[NUInt]
  )

  implicit val bufferFloat1RawFloat = (
    (buffer: ByteBuffer) => new BufferFloat1RawFloat(buffer), 1, classOf[RawFloat]
  )


  // View Float
  implicit val viewFloat1SByte = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewFloat1SByte(buffer, offset, stride), classOf[SByte]
  )
  implicit val viewFloat1UByte = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewFloat1UByte(buffer, offset, stride), classOf[UByte]
  )
  implicit val viewFloat1NSByte = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewFloat1NSByte(buffer, offset, stride), classOf[NSByte]
  )
  implicit val viewFloat1NUByte = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewFloat1NUByte(buffer, offset, stride), classOf[NUByte]
  )

  implicit val viewFloat1SShort = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewFloat1SShort(buffer, offset, stride), classOf[SShort]
  )
  implicit val viewFloat1NSShort = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewFloat1NSShort(buffer, offset, stride), classOf[NSShort]
  )

  implicit val viewFloat1UShort = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewFloat1UShort(buffer, offset, stride), classOf[UShort]
  )
  implicit val viewFloat1NUShort = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewFloat1NUShort(buffer, offset, stride), classOf[NUShort]
  )

  implicit val viewFloat1SInt = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewFloat1SInt(buffer, offset, stride), classOf[SInt]
  )
  implicit val viewFloat1UInt = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewFloat1UInt(buffer, offset, stride), classOf[UInt]
  )
  implicit val viewFloat1NSInt = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewFloat1NSInt(buffer, offset, stride), classOf[NSInt]
  )
  implicit val viewFloat1NUInt = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewFloat1NUInt(buffer, offset, stride), classOf[NUInt]
  )

  implicit val viewFloat1RawFloat = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewFloat1RawFloat(buffer, offset, stride), classOf[RawFloat]
  )


  // Vec2f
  // Array Vec2f
  implicit val arrayVec2fSByte = (
    (array: Array[Byte]) => new ArrayVec2f(new ArrayFloat1SByte(array)),
    2, classOf[SByte]
  )
  implicit val arrayVec2fUByte = (
    (array: Array[Byte]) => new ArrayVec2f(new ArrayFloat1UByte(array)),
    2, classOf[UByte]
  )
  implicit val arrayVec2fNSByte = (
    (array: Array[Byte]) => new ArrayVec2f(new ArrayFloat1NSByte(array)),
    2, classOf[NSByte]
  )
  implicit val arrayVec2fNUByte = (
    (array: Array[Byte]) => new ArrayVec2f(new ArrayFloat1NUByte(array)),
    2, classOf[NUByte]
  )

  implicit val arrayVec2fSShort = (
    (array: Array[Short]) => new ArrayVec2f(new ArrayFloat1SShort(array)),
    2, classOf[SShort]
  )
  implicit val arrayVec2fNSShort = (
    (array: Array[Short]) => new ArrayVec2f(new ArrayFloat1NSShort(array)),
    2, classOf[NSShort]
  )

  implicit val arrayVec2fUShort = (
    (array: Array[Char]) => new ArrayVec2f(new ArrayFloat1UShort(array)),
    2, classOf[UShort]
  )
  implicit val arrayVec2fNUShort = (
    (array: Array[Char]) => new ArrayVec2f(new ArrayFloat1NUShort(array)),
    2, classOf[NUShort]
  )

  implicit val arrayVec2fSInt = (
    (array: Array[Int]) => new ArrayVec2f(new ArrayFloat1SInt(array)),
    2, classOf[SInt]
  )
  implicit val arrayVec2fUInt = (
    (array: Array[Int]) => new ArrayVec2f(new ArrayFloat1UInt(array)),
    2, classOf[UInt]
  )
  implicit val arrayVec2fNSInt = (
    (array: Array[Int]) => new ArrayVec2f(new ArrayFloat1NSInt(array)),
    2, classOf[NSInt]
  )
  implicit val arrayVec2fNUInt = (
    (array: Array[Int]) => new ArrayVec2f(new ArrayFloat1NUInt(array)),
    2, classOf[NUInt]
  )

  implicit val arrayVec2fRawFloat = (
    (array: Array[Float]) => new ArrayVec2f(new ArrayFloat1RawFloat(array)),
    2, classOf[RawFloat]
  )


  // Buffer Vec2f
  implicit val bufferVec2fSByte = (
    (buffer: ByteBuffer) => new BufferVec2f(new BufferFloat1SByte(buffer)),
    2, classOf[SByte]
  )
  implicit val bufferVec2fUByte = (
    (buffer: ByteBuffer) => new BufferVec2f(new BufferFloat1UByte(buffer)),
    2, classOf[UByte]
  )
  implicit val bufferVec2fNSByte = (
    (buffer: ByteBuffer) => new BufferVec2f(new BufferFloat1NSByte(buffer)),
    2, classOf[NSByte]
  )
  implicit val bufferVec2fNUByte = (
    (buffer: ByteBuffer) => new BufferVec2f(new BufferFloat1NUByte(buffer)),
    2, classOf[NUByte]
  )

  implicit val bufferVec2fSShort = (
    (buffer: ByteBuffer) => new BufferVec2f(new BufferFloat1SShort(buffer)),
    2, classOf[SShort]
  )
  implicit val bufferVec2fNSShort = (
    (buffer: ByteBuffer) => new BufferVec2f(new BufferFloat1NSShort(buffer)),
    2, classOf[NSShort]
  )

  implicit val bufferVec2fUShort = (
    (buffer: ByteBuffer) => new BufferVec2f(new BufferFloat1UShort(buffer)),
    2, classOf[UShort]
  )
  implicit val bufferVec2fNUShort = (
    (buffer: ByteBuffer) => new BufferVec2f(new BufferFloat1NUShort(buffer)),
    2, classOf[NUShort]
  )

  implicit val bufferVec2fSInt = (
    (buffer: ByteBuffer) => new BufferVec2f(new BufferFloat1SInt(buffer)),
    2, classOf[SInt]
  )
  implicit val bufferVec2fUInt = (
    (buffer: ByteBuffer) => new BufferVec2f(new BufferFloat1UInt(buffer)),
    2, classOf[UInt]
  )
  implicit val bufferVec2fNSInt = (
    (buffer: ByteBuffer) => new BufferVec2f(new BufferFloat1NSInt(buffer)),
    2, classOf[NSInt]
  )
  implicit val bufferVec2fNUInt = (
    (buffer: ByteBuffer) => new BufferVec2f(new BufferFloat1NUInt(buffer)),
    2, classOf[NUInt]
  )

  implicit val bufferVec2fRawFloat = (
    (buffer: ByteBuffer) => new BufferVec2f(new BufferFloat1RawFloat(buffer)),
    2, classOf[RawFloat]
  )


  // View Vec2f
  implicit val viewVec2fSByte = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec2f(new BufferFloat1SByte(buffer), offset, stride), classOf[SByte]
  )
  implicit val viewVec2fUByte = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec2f(new BufferFloat1UByte(buffer), offset, stride), classOf[UByte]
  )
  implicit val viewVec2fNSByte = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec2f(new BufferFloat1NSByte(buffer), offset, stride), classOf[NSByte]
  )
  implicit val viewVec2fNUByte = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec2f(new BufferFloat1NUByte(buffer), offset, stride), classOf[NUByte]
  )

  implicit val viewVec2fSShort = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec2f(new BufferFloat1SShort(buffer), offset, stride), classOf[SShort]
  )
  implicit val viewVec2fNSShort = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec2f(new BufferFloat1NSShort(buffer), offset, stride), classOf[NSShort]
  )

  implicit val viewVec2fUShort = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec2f(new BufferFloat1UShort(buffer), offset, stride), classOf[UShort]
  )
  implicit val viewVec2fNUShort = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec2f(new BufferFloat1NUShort(buffer), offset, stride), classOf[NUShort]
  )

  implicit val viewVec2fSInt = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec2f(new BufferFloat1SInt(buffer), offset, stride), classOf[SInt]
  )
  implicit val viewVec2fUInt = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec2f(new BufferFloat1UInt(buffer), offset, stride), classOf[UInt]
  )
  implicit val viewVec2fNSInt = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec2f(new BufferFloat1NSInt(buffer), offset, stride), classOf[NSInt]
  )
  implicit val viewVec2fNUInt = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec2f(new BufferFloat1NUInt(buffer), offset, stride), classOf[NUInt]
  )

  implicit val viewVec2fRawFloat = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec2f(new BufferFloat1RawFloat(buffer), offset, stride), classOf[RawFloat]
  )


  // Vec3f
  // Array Vec3f
  implicit val arrayVec3fSByte = (
    (array: Array[Byte]) => new ArrayVec3f(new ArrayFloat1SByte(array)),
    3, classOf[SByte]
  )
  implicit val arrayVec3fUByte = (
    (array: Array[Byte]) => new ArrayVec3f(new ArrayFloat1UByte(array)),
    3, classOf[UByte]
  )
  implicit val arrayVec3fNSByte = (
    (array: Array[Byte]) => new ArrayVec3f(new ArrayFloat1NSByte(array)),
    3, classOf[NSByte]
  )
  implicit val arrayVec3fNUByte = (
    (array: Array[Byte]) => new ArrayVec3f(new ArrayFloat1NUByte(array)),
    3, classOf[NUByte]
  )

  implicit val arrayVec3fSShort = (
    (array: Array[Short]) => new ArrayVec3f(new ArrayFloat1SShort(array)),
    3, classOf[SShort]
  )
  implicit val arrayVec3fNSShort = (
    (array: Array[Short]) => new ArrayVec3f(new ArrayFloat1NSShort(array)),
    3, classOf[NSShort]
  )

  implicit val arrayVec3fUShort = (
    (array: Array[Char]) => new ArrayVec3f(new ArrayFloat1UShort(array)),
    3, classOf[UShort]
  )
  implicit val arrayVec3fNUShort = (
    (array: Array[Char]) => new ArrayVec3f(new ArrayFloat1NUShort(array)),
    3, classOf[NUShort]
  )

  implicit val arrayVec3fSInt = (
    (array: Array[Int]) => new ArrayVec3f(new ArrayFloat1SInt(array)),
    3, classOf[SInt]
  )
  implicit val arrayVec3fUInt = (
    (array: Array[Int]) => new ArrayVec3f(new ArrayFloat1UInt(array)),
    3, classOf[UInt]
  )
  implicit val arrayVec3fNSInt = (
    (array: Array[Int]) => new ArrayVec3f(new ArrayFloat1NSInt(array)),
    3, classOf[NSInt]
  )
  implicit val arrayVec3fNUInt = (
    (array: Array[Int]) => new ArrayVec3f(new ArrayFloat1NUInt(array)),
    3, classOf[NUInt]
  )

  implicit val arrayVec3fRawFloat = (
    (array: Array[Float]) => new ArrayVec3f(new ArrayFloat1RawFloat(array)),
    3, classOf[RawFloat]
  )


  // Buffer Vec3f
  implicit val bufferVec3fSByte = (
    (buffer: ByteBuffer) => new BufferVec3f(new BufferFloat1SByte(buffer)),
    3, classOf[SByte]
  )
  implicit val bufferVec3fUByte = (
    (buffer: ByteBuffer) => new BufferVec3f(new BufferFloat1UByte(buffer)),
    3, classOf[UByte]
  )
  implicit val bufferVec3fNSByte = (
    (buffer: ByteBuffer) => new BufferVec3f(new BufferFloat1NSByte(buffer)),
    3, classOf[NSByte]
  )
  implicit val bufferVec3fNUByte = (
    (buffer: ByteBuffer) => new BufferVec3f(new BufferFloat1NUByte(buffer)),
    3, classOf[NUByte]
  )

  implicit val bufferVec3fSShort = (
    (buffer: ByteBuffer) => new BufferVec3f(new BufferFloat1SShort(buffer)),
    3, classOf[SShort]
  )
  implicit val bufferVec3fNSShort = (
    (buffer: ByteBuffer) => new BufferVec3f(new BufferFloat1NSShort(buffer)),
    3, classOf[NSShort]
  )

  implicit val bufferVec3fUShort = (
    (buffer: ByteBuffer) => new BufferVec3f(new BufferFloat1UShort(buffer)),
    3, classOf[UShort]
  )
  implicit val bufferVec3fNUShort = (
    (buffer: ByteBuffer) => new BufferVec3f(new BufferFloat1NUShort(buffer)),
    3, classOf[NUShort]
  )

  implicit val bufferVec3fSInt = (
    (buffer: ByteBuffer) => new BufferVec3f(new BufferFloat1SInt(buffer)),
    3, classOf[SInt]
  )
  implicit val bufferVec3fUInt = (
    (buffer: ByteBuffer) => new BufferVec3f(new BufferFloat1UInt(buffer)),
    3, classOf[UInt]
  )
  implicit val bufferVec3fNSInt = (
    (buffer: ByteBuffer) => new BufferVec3f(new BufferFloat1NSInt(buffer)),
    3, classOf[NSInt]
  )
  implicit val bufferVec3fNUInt = (
    (buffer: ByteBuffer) => new BufferVec3f(new BufferFloat1NUInt(buffer)),
    3, classOf[NUInt]
  )

  implicit val bufferVec3fRawFloat = (
    (buffer: ByteBuffer) => new BufferVec3f(new BufferFloat1RawFloat(buffer)),
    3, classOf[RawFloat]
  )


  // View Vec3f
  implicit val viewVec3fSByte = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec3f(new BufferFloat1SByte(buffer), offset, stride), classOf[SByte]
  )
  implicit val viewVec3fUByte = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec3f(new BufferFloat1UByte(buffer), offset, stride), classOf[UByte]
  )
  implicit val viewVec3fNSByte = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec3f(new BufferFloat1NSByte(buffer), offset, stride), classOf[NSByte]
  )
  implicit val viewVec3fNUByte = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec3f(new BufferFloat1NUByte(buffer), offset, stride), classOf[NUByte]
  )

  implicit val viewVec3fSShort = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec3f(new BufferFloat1SShort(buffer), offset, stride), classOf[SShort]
  )
  implicit val viewVec3fNSShort = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec3f(new BufferFloat1NSShort(buffer), offset, stride), classOf[NSShort]
  )

  implicit val viewVec3fUShort = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec3f(new BufferFloat1UShort(buffer), offset, stride), classOf[UShort]
  )
  implicit val viewVec3fNUShort = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec3f(new BufferFloat1NUShort(buffer), offset, stride), classOf[NUShort]
  )

  implicit val viewVec3fSInt = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec3f(new BufferFloat1SInt(buffer), offset, stride), classOf[SInt]
  )
  implicit val viewVec3fUInt = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec3f(new BufferFloat1UInt(buffer), offset, stride), classOf[UInt]
  )
  implicit val viewVec3fNSInt = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec3f(new BufferFloat1NSInt(buffer), offset, stride), classOf[NSInt]
  )
  implicit val viewVec3fNUInt = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec3f(new BufferFloat1NUInt(buffer), offset, stride), classOf[NUInt]
  )

  implicit val viewVec3fRawFloat = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec3f(new BufferFloat1RawFloat(buffer), offset, stride), classOf[RawFloat]
  )


  // Vec4f
  // Array Vec4f
  implicit val arrayVec4fSByte = (
    (array: Array[Byte]) => new ArrayVec4f(new ArrayFloat1SByte(array)),
    4, classOf[SByte]
  )
  implicit val arrayVec4fUByte = (
    (array: Array[Byte]) => new ArrayVec4f(new ArrayFloat1UByte(array)),
    4, classOf[UByte]
  )
  implicit val arrayVec4fNSByte = (
    (array: Array[Byte]) => new ArrayVec4f(new ArrayFloat1NSByte(array)),
    4, classOf[NSByte]
  )
  implicit val arrayVec4fNUByte = (
    (array: Array[Byte]) => new ArrayVec4f(new ArrayFloat1NUByte(array)),
    4, classOf[NUByte]
  )

  implicit val arrayVec4fSShort = (
    (array: Array[Short]) => new ArrayVec4f(new ArrayFloat1SShort(array)),
    4, classOf[SShort]
  )
  implicit val arrayVec4fNSShort = (
    (array: Array[Short]) => new ArrayVec4f(new ArrayFloat1NSShort(array)),
    4, classOf[NSShort]
  )

  implicit val arrayVec4fUShort = (
    (array: Array[Char]) => new ArrayVec4f(new ArrayFloat1UShort(array)),
    4, classOf[UShort]
  )
  implicit val arrayVec4fNUShort = (
    (array: Array[Char]) => new ArrayVec4f(new ArrayFloat1NUShort(array)),
    4, classOf[NUShort]
  )

  implicit val arrayVec4fSInt = (
    (array: Array[Int]) => new ArrayVec4f(new ArrayFloat1SInt(array)),
    4, classOf[SInt]
  )
  implicit val arrayVec4fUInt = (
    (array: Array[Int]) => new ArrayVec4f(new ArrayFloat1UInt(array)),
    4, classOf[UInt]
  )
  implicit val arrayVec4fNSInt = (
    (array: Array[Int]) => new ArrayVec4f(new ArrayFloat1NSInt(array)),
    4, classOf[NSInt]
  )
  implicit val arrayVec4fNUInt = (
    (array: Array[Int]) => new ArrayVec4f(new ArrayFloat1NUInt(array)),
    4, classOf[NUInt]
  )

  implicit val arrayVec4fRawFloat = (
    (array: Array[Float]) => new ArrayVec4f(new ArrayFloat1RawFloat(array)),
    4, classOf[RawFloat]
  )


  // Buffer Vec4f
  implicit val bufferVec4fSByte = (
    (buffer: ByteBuffer) => new BufferVec4f(new BufferFloat1SByte(buffer)),
    4, classOf[SByte]
  )
  implicit val bufferVec4fUByte = (
    (buffer: ByteBuffer) => new BufferVec4f(new BufferFloat1UByte(buffer)),
    4, classOf[UByte]
  )
  implicit val bufferVec4fNSByte = (
    (buffer: ByteBuffer) => new BufferVec4f(new BufferFloat1NSByte(buffer)),
    4, classOf[NSByte]
  )
  implicit val bufferVec4fNUByte = (
    (buffer: ByteBuffer) => new BufferVec4f(new BufferFloat1NUByte(buffer)),
    4, classOf[NUByte]
  )

  implicit val bufferVec4fSShort = (
    (buffer: ByteBuffer) => new BufferVec4f(new BufferFloat1SShort(buffer)),
    4, classOf[SShort]
  )
  implicit val bufferVec4fNSShort = (
    (buffer: ByteBuffer) => new BufferVec4f(new BufferFloat1NSShort(buffer)),
    4, classOf[NSShort]
  )

  implicit val bufferVec4fUShort = (
    (buffer: ByteBuffer) => new BufferVec4f(new BufferFloat1UShort(buffer)),
    4, classOf[UShort]
  )
  implicit val bufferVec4fNUShort = (
    (buffer: ByteBuffer) => new BufferVec4f(new BufferFloat1NUShort(buffer)),
    4, classOf[NUShort]
  )

  implicit val bufferVec4fSInt = (
    (buffer: ByteBuffer) => new BufferVec4f(new BufferFloat1SInt(buffer)),
    4, classOf[SInt]
  )
  implicit val bufferVec4fUInt = (
    (buffer: ByteBuffer) => new BufferVec4f(new BufferFloat1UInt(buffer)),
    4, classOf[UInt]
  )
  implicit val bufferVec4fNSInt = (
    (buffer: ByteBuffer) => new BufferVec4f(new BufferFloat1NSInt(buffer)),
    4, classOf[NSInt]
  )
  implicit val bufferVec4fNUInt = (
    (buffer: ByteBuffer) => new BufferVec4f(new BufferFloat1NUInt(buffer)),
    4, classOf[NUInt]
  )

  implicit val bufferVec4fRawFloat = (
    (buffer: ByteBuffer) => new BufferVec4f(new BufferFloat1RawFloat(buffer)),
    4, classOf[RawFloat]
  )

  
  // View Vec4f
  implicit val viewVec4fSByte = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec4f(new BufferFloat1SByte(buffer), offset, stride), classOf[SByte]
  )
  implicit val viewVec4fUByte = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec4f(new BufferFloat1UByte(buffer), offset, stride), classOf[UByte]
  )
  implicit val viewVec4fNSByte = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec4f(new BufferFloat1NSByte(buffer), offset, stride), classOf[NSByte]
  )
  implicit val viewVec4fNUByte = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec4f(new BufferFloat1NUByte(buffer), offset, stride), classOf[NUByte]
  )

  implicit val viewVec4fSShort = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec4f(new BufferFloat1SShort(buffer), offset, stride), classOf[SShort]
  )
  implicit val viewVec4fNSShort = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec4f(new BufferFloat1NSShort(buffer), offset, stride), classOf[NSShort]
  )

  implicit val viewVec4fUShort = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec4f(new BufferFloat1UShort(buffer), offset, stride), classOf[UShort]
  )
  implicit val viewVec4fNUShort = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec4f(new BufferFloat1NUShort(buffer), offset, stride), classOf[NUShort]
  )

  implicit val viewVec4fSInt = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec4f(new BufferFloat1SInt(buffer), offset, stride), classOf[SInt]
  )
  implicit val viewVec4fUInt = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec4f(new BufferFloat1UInt(buffer), offset, stride), classOf[UInt]
  )
  implicit val viewVec4fNSInt = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec4f(new BufferFloat1NSInt(buffer), offset, stride), classOf[NSInt]
  )
  implicit val viewVec4fNUInt = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec4f(new BufferFloat1NUInt(buffer), offset, stride), classOf[NUInt]
  )

  implicit val viewVec4fRawFloat = (
    (buffer: ByteBuffer, offset: Int, stride: Int) =>
    new ViewVec4f(new BufferFloat1RawFloat(buffer), offset, stride), classOf[RawFloat]
  )
}
