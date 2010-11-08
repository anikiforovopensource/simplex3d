/*
 * Simplex3d, FloatBuffer module
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
import simplex3d.math.floatm._
import simplex3d.buffer.floatm._
import simplex3d.buffer.floatm.impl._


/**
 * @author Aleksey Nikiforov (lex)
 */
package object floatm {

  private final def primitiveFactory[R <: DefinedFloat](s: DataSeq[Float1, R]) :Factory[Float1, R] = s
  private final def factory[E <: Composite, R <: DefinedFloat](s: DataSeq[E, R]) :Factory[E, R] = s
  private final def cast[R <: DefinedFloat](f: Factory[Float1, R]) = f.asInstanceOf[DataArray[Float1, R]]


  // Float1
  implicit final lazy val FactoryFloat1SByte = primitiveFactory[SByte](new ArrayFloat1SByte)
  implicit final lazy val FactoryFloat1UByte = primitiveFactory[UByte](new ArrayFloat1UByte)
  implicit final lazy val FactoryFloat1SShort = primitiveFactory[SShort](new ArrayFloat1SShort)
  implicit final lazy val FactoryFloat1UShort = primitiveFactory[UShort](new ArrayFloat1UShort)
  implicit final lazy val FactoryFloat1SInt = primitiveFactory[SInt](new ArrayFloat1SInt)
  implicit final lazy val FactoryFloat1UInt = primitiveFactory[UInt](new ArrayFloat1UInt)
  implicit final lazy val FactoryFloat1HalfFloat = primitiveFactory[HalfFloat](new ArrayFloat1HalfFloat)
  implicit final lazy val FactoryFloat1RawFloat = primitiveFactory[RawFloat](new ArrayFloat1RawFloat)


  // Vec2f
  implicit final lazy val FactoryVec2fSByte = factory[Vec2f, SByte](new ArrayVec2f(cast(FactoryFloat1SByte)))
  implicit final lazy val FactoryVec2fUByte = factory[Vec2f, UByte](new ArrayVec2f(cast(FactoryFloat1UByte)))
  implicit final lazy val FactoryVec2fSShort = factory[Vec2f, SShort](new ArrayVec2f(cast(FactoryFloat1SShort)))
  implicit final lazy val FactoryVec2fUShort = factory[Vec2f, UShort](new ArrayVec2f(cast(FactoryFloat1UShort)))
  implicit final lazy val FactoryVec2fSInt = factory[Vec2f, SInt](new ArrayVec2f(cast(FactoryFloat1SInt)))
  implicit final lazy val FactoryVec2fUInt = factory[Vec2f, UInt](new ArrayVec2f(cast(FactoryFloat1UInt)))
  implicit final lazy val FactoryVec2fHalfFloat = factory[Vec2f, HalfFloat](new ArrayVec2f(cast(FactoryFloat1HalfFloat)))
  implicit final lazy val FactoryVec2fRawFloat = factory[Vec2f, RawFloat](new ArrayVec2fRawFloat())

  // Vec3f
  implicit final lazy val FactoryVec3fSByte = factory[Vec3f, SByte](new ArrayVec3f(cast(FactoryFloat1SByte)))
  implicit final lazy val FactoryVec3fUByte = factory[Vec3f, UByte](new ArrayVec3fUByte())
  implicit final lazy val FactoryVec3fSShort = factory[Vec3f, SShort](new ArrayVec3f(cast(FactoryFloat1SShort)))
  implicit final lazy val FactoryVec3fUShort = factory[Vec3f, UShort](new ArrayVec3f(cast(FactoryFloat1UShort)))
  implicit final lazy val FactoryVec3fSInt = factory[Vec3f, SInt](new ArrayVec3f(cast(FactoryFloat1SInt)))
  implicit final lazy val FactoryVec3fUInt = factory[Vec3f, UInt](new ArrayVec3f(cast(FactoryFloat1UInt)))
  implicit final lazy val FactoryVec3fHalfFloat = factory[Vec3f, HalfFloat](new ArrayVec3f(cast(FactoryFloat1HalfFloat)))
  implicit final lazy val FactoryVec3fRawFloat = factory[Vec3f, RawFloat](new ArrayVec3fRawFloat())
  
  // Vec4f
  implicit final lazy val FactoryVec4fSByte = factory[Vec4f, SByte](new ArrayVec4f(cast(FactoryFloat1SByte)))
  implicit final lazy val FactoryVec4fUByte = factory[Vec4f, UByte](new ArrayVec4fUByte())
  implicit final lazy val FactoryVec4fSShort = factory[Vec4f, SShort](new ArrayVec4f(cast(FactoryFloat1SShort)))
  implicit final lazy val FactoryVec4fUShort = factory[Vec4f, UShort](new ArrayVec4f(cast(FactoryFloat1UShort)))
  implicit final lazy val FactoryVec4fSInt = factory[Vec4f, SInt](new ArrayVec4f(cast(FactoryFloat1SInt)))
  implicit final lazy val FactoryVec4fUInt = factory[Vec4f, UInt](new ArrayVec4f(cast(FactoryFloat1UInt)))
  implicit final lazy val FactoryVec4fHalfFloat = factory[Vec4f, HalfFloat](new ArrayVec4f(cast(FactoryFloat1HalfFloat)))
  implicit final lazy val FactoryVec4fRawFloat = factory[Vec4f, RawFloat](new ArrayVec4fRawFloat())
}
