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
import simplex3d.math._
import simplex3d.buffer.floatm._


/**
 * @author Aleksey Nikiforov (lex)
 */
package object floatm {

  private final type F[T <: MetaType, D <: RawType] = DataSeqFactoryRef[T, D]
  private final def ar[T <: MetaType, D <: RawType](f: DataSeqFactoryRef[T, D])=
    f.factory.asInstanceOf[DataArray[T, D]]


  // Float1
  implicit final val fFloat1SByte = new F(new ArrayFloat1SByte(Array[Byte]()))
  implicit final val fFloat1UByte = new F(new ArrayFloat1UByte(Array[Byte]()))
  implicit final val fFloat1NSByte = new F(new ArrayFloat1NSByte(Array[Byte]()))
  implicit final val fFloat1NUByte = new F(new ArrayFloat1NUByte(Array[Byte]()))
  
  implicit final val fFloat1SShort = new F(new ArrayFloat1SShort(Array[Short]()))
  implicit final val fFloat1UShort = new F(new ArrayFloat1UShort(Array[Char]()))
  implicit final val fFloat1NSShort = new F(new ArrayFloat1NSShort(Array[Short]()))
  implicit final val fFloat1NUShort = new F(new ArrayFloat1NUShort(Array[Char]()))

  implicit final val fFloat1SInt = new F(new ArrayFloat1SInt(Array[Int]()))
  implicit final val fFloat1UInt = new F(new ArrayFloat1UInt(Array[Int]()))
  implicit final val fFloat1NSInt = new F(new ArrayFloat1NSInt(Array[Int]()))
  implicit final val fFloat1NUInt = new F(new ArrayFloat1NUInt(Array[Int]()))

  implicit final val fFloat1RawFloat = new F(new ArrayFloat1RawFloat(Array[Float]()))


  // Vec2f
  implicit final val fVec2fSByte = new F(new ArrayVec2f[SByte](ar(fFloat1SByte)))
  implicit final val fVec2fUByte = new F(new ArrayVec2f[UByte](ar(fFloat1UByte)))
  implicit final val fVec2fNSByte = new F(new ArrayVec2f[NSByte](ar(fFloat1NSByte)))
  implicit final val fVec2fNUByte = new F(new ArrayVec2f[NUByte](ar(fFloat1NUByte)))

  implicit final val fVec2fSShort = new F(new ArrayVec2f[SShort](ar(fFloat1SShort)))
  implicit final val fVec2fUShort = new F(new ArrayVec2f[UShort](ar(fFloat1UShort)))
  implicit final val fVec2fNSShort = new F(new ArrayVec2f[NSShort](ar(fFloat1NSShort)))
  implicit final val fVec2fNUShort = new F(new ArrayVec2f[NUShort](ar(fFloat1NUShort)))

  implicit final val fVec2fSInt = new F(new ArrayVec2f[SInt](ar(fFloat1SInt)))
  implicit final val fVec2fUInt = new F(new ArrayVec2f[UInt](ar(fFloat1UInt)))
  implicit final val fVec2fNSInt = new F(new ArrayVec2f[NSInt](ar(fFloat1NSInt)))
  implicit final val fVec2fNUInt = new F(new ArrayVec2f[NUInt](ar(fFloat1NUInt)))
  
  implicit final val fVec2fRawFloat = new F(new ArrayVec2f[RawFloat](ar(fFloat1RawFloat)))


  // Vec3f
  implicit final val fVec3fSByte = new F(new ArrayVec3f[SByte](ar(fFloat1SByte)))
  implicit final val fVec3fUByte = new F(new ArrayVec3f[UByte](ar(fFloat1UByte)))
  implicit final val fVec3fNSByte = new F(new ArrayVec3f[NSByte](ar(fFloat1NSByte)))
  implicit final val fVec3fNUByte = new F(new ArrayVec3f[NUByte](ar(fFloat1NUByte)))

  implicit final val fVec3fSShort = new F(new ArrayVec3f[SShort](ar(fFloat1SShort)))
  implicit final val fVec3fUShort = new F(new ArrayVec3f[UShort](ar(fFloat1UShort)))
  implicit final val fVec3fNSShort = new F(new ArrayVec3f[NSShort](ar(fFloat1NSShort)))
  implicit final val fVec3fNUShort = new F(new ArrayVec3f[NUShort](ar(fFloat1NUShort)))

  implicit final val fVec3fSInt = new F(new ArrayVec3f[SInt](ar(fFloat1SInt)))
  implicit final val fVec3fUInt = new F(new ArrayVec3f[UInt](ar(fFloat1UInt)))
  implicit final val fVec3fNSInt = new F(new ArrayVec3f[NSInt](ar(fFloat1NSInt)))
  implicit final val fVec3fNUInt = new F(new ArrayVec3f[NUInt](ar(fFloat1NUInt)))

  implicit final val fVec3fRawFloat = new F(new ArrayVec3f[RawFloat](ar(fFloat1RawFloat)))


  // Vec4f
  implicit final val fVec4fSByte = new F(new ArrayVec4f[SByte](ar(fFloat1SByte)))
  implicit final val fVec4fUByte = new F(new ArrayVec4f[UByte](ar(fFloat1UByte)))
  implicit final val fVec4fNSByte = new F(new ArrayVec4f[NSByte](ar(fFloat1NSByte)))
  implicit final val fVec4fNUByte = new F(new ArrayVec4f[NUByte](ar(fFloat1NUByte)))

  implicit final val fVec4fSShort = new F(new ArrayVec4f[SShort](ar(fFloat1SShort)))
  implicit final val fVec4fUShort = new F(new ArrayVec4f[UShort](ar(fFloat1UShort)))
  implicit final val fVec4fNSShort = new F(new ArrayVec4f[NSShort](ar(fFloat1NSShort)))
  implicit final val fVec4fNUShort = new F(new ArrayVec4f[NUShort](ar(fFloat1NUShort)))

  implicit final val fVec4fSInt = new F(new ArrayVec4f[SInt](ar(fFloat1SInt)))
  implicit final val fVec4fUInt = new F(new ArrayVec4f[UInt](ar(fFloat1UInt)))
  implicit final val fVec4fNSInt = new F(new ArrayVec4f[NSInt](ar(fFloat1NSInt)))
  implicit final val fVec4fNUInt = new F(new ArrayVec4f[NUInt](ar(fFloat1NUInt)))

  implicit final val fVec4fRawFloat = new F(new ArrayVec4f[RawFloat](ar(fFloat1RawFloat)))
}
