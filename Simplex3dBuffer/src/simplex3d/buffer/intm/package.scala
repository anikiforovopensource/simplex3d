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
import simplex3d.math._
import simplex3d.buffer.intm._


/**
 * @author Aleksey Nikiforov (lex)
 */
package object intm extends UnsignedImplicits {

  private final type F[T <: MetaType, D <: RawType] = DataSeqFactoryRef[T, D]
  private final def ar[T <: MetaType, D <: RawType](f: DataSeqFactoryRef[T, D])=
    f.factory.asInstanceOf[DataArray[T, D]]


  // Int1
  implicit final val fInt1SByte = new F(new ArrayInt1SByte(Array[Byte]()))
  implicit final val fInt1SShort = new F(new ArrayInt1SShort(Array[Short]()))
  implicit final val fInt1SInt = new F(new ArrayInt1SInt(Array[Int]()))


  // Vec2i
  implicit final val fVec2iSByte = new F(new ArrayVec2i[SByte](ar(fInt1SByte)))
  implicit final val fVec2iUByte = new F(new ArrayVec2i[UByte](ar(fInt1UByte)))

  implicit final val fVec2iSShort = new F(new ArrayVec2i[SShort](ar(fInt1SShort)))
  implicit final val fVec2iUShort = new F(new ArrayVec2i[UShort](ar(fInt1UShort)))

  implicit final val fVec2iSInt = new F(new ArrayVec2i[SInt](ar(fInt1SInt)))
  implicit final val fVec2iUInt = new F(new ArrayVec2i[UInt](ar(fInt1UInt)))


  // Vec3i
  implicit final val fVec3iSByte = new F(new ArrayVec3i[SByte](ar(fInt1SByte)))
  implicit final val fVec3iUByte = new F(new ArrayVec3i[UByte](ar(fInt1UByte)))

  implicit final val fVec3iSShort = new F(new ArrayVec3i[SShort](ar(fInt1SShort)))
  implicit final val fVec3iUShort = new F(new ArrayVec3i[UShort](ar(fInt1UShort)))

  implicit final val fVec3iSInt = new F(new ArrayVec3i[SInt](ar(fInt1SInt)))
  implicit final val fVec3iUInt = new F(new ArrayVec3i[UInt](ar(fInt1UInt)))


  // Vec4i
  implicit final val fVec4iSByte = new F(new ArrayVec4i[SByte](ar(fInt1SByte)))
  implicit final val fVec4iUByte = new F(new ArrayVec4i[UByte](ar(fInt1UByte)))

  implicit final val fVec4iSShort = new F(new ArrayVec4i[SShort](ar(fInt1SShort)))
  implicit final val fVec4iUShort = new F(new ArrayVec4i[UShort](ar(fInt1UShort)))

  implicit final val fVec4iSInt = new F(new ArrayVec4i[SInt](ar(fInt1SInt)))
  implicit final val fVec4iUInt = new F(new ArrayVec4i[UInt](ar(fInt1UInt)))
}
