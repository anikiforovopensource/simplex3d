/*
 * Simplex3d, DoubleBuffer module
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
import simplex3d.math.doublem._
import simplex3d.buffer.doublem._
import simplex3d.buffer.doublem.impl._


/**
 * @author Aleksey Nikiforov (lex)
 */
package object doublem {

  private final def primitiveFactory[R <: DefinedDouble](s: DataSeq[Double1, R]) :Factory[Double1, R] = s
  private final def factory[E <: Composite, R <: DefinedDouble](s: DataSeq[E, R]) :Factory[E, R] = s
  private final def cast[R <: DefinedDouble](f: Factory[Double1, R]) = f.asInstanceOf[DataArray[Double1, R]]


  // Double1
  implicit final lazy val FactoryDouble1SByte = primitiveFactory[SByte](new ArrayDouble1SByte)
  implicit final lazy val FactoryDouble1UByte = primitiveFactory[UByte](new ArrayDouble1UByte)
  implicit final lazy val FactoryDouble1SShort = primitiveFactory[SShort](new ArrayDouble1SShort)
  implicit final lazy val FactoryDouble1UShort = primitiveFactory[UShort](new ArrayDouble1UShort)
  implicit final lazy val FactoryDouble1SInt = primitiveFactory[SInt](new ArrayDouble1SInt)
  implicit final lazy val FactoryDouble1UInt = primitiveFactory[UInt](new ArrayDouble1UInt)
  implicit final lazy val FactoryDouble1HalfFloat = primitiveFactory[HalfFloat](new ArrayDouble1HalfFloat)
  implicit final lazy val FactoryDouble1RawFloat = primitiveFactory[RawFloat](new ArrayDouble1RawFloat)
  implicit final lazy val FactoryDouble1RawDouble = primitiveFactory[RawDouble](new ArrayDouble1RawDouble)


  // Vec2d
  implicit final lazy val FactoryVec2dSByte = factory[Vec2d, SByte](new ArrayVec2d(cast(FactoryDouble1SByte)))
  implicit final lazy val FactoryVec2dUByte = factory[Vec2d, UByte](new ArrayVec2d(cast(FactoryDouble1UByte)))
  implicit final lazy val FactoryVec2dSShort = factory[Vec2d, SShort](new ArrayVec2d(cast(FactoryDouble1SShort)))
  implicit final lazy val FactoryVec2dUShort = factory[Vec2d, UShort](new ArrayVec2d(cast(FactoryDouble1UShort)))
  implicit final lazy val FactoryVec2dSInt = factory[Vec2d, SInt](new ArrayVec2d(cast(FactoryDouble1SInt)))
  implicit final lazy val FactoryVec2dUInt = factory[Vec2d, UInt](new ArrayVec2d(cast(FactoryDouble1UInt)))
  implicit final lazy val FactoryVec2dHalfFloat = factory[Vec2d, HalfFloat](new ArrayVec2d(cast(FactoryDouble1HalfFloat)))
  implicit final lazy val FactoryVec2dRawFloat = factory[Vec2d, RawFloat](new ArrayVec2dRawFloat())
  implicit final lazy val FactoryVec2dRawDouble = factory[Vec2d, RawDouble](new ArrayVec2d(cast(FactoryDouble1RawDouble)))
  
  // Vec3d
  implicit final lazy val FactoryVec3dSByte = factory[Vec3d, SByte](new ArrayVec3d(cast(FactoryDouble1SByte)))
  implicit final lazy val FactoryVec3dUByte = factory[Vec3d, UByte](new ArrayVec3dUByte())
  implicit final lazy val FactoryVec3dSShort = factory[Vec3d, SShort](new ArrayVec3d(cast(FactoryDouble1SShort)))
  implicit final lazy val FactoryVec3dUShort = factory[Vec3d, UShort](new ArrayVec3d(cast(FactoryDouble1UShort)))
  implicit final lazy val FactoryVec3dSInt = factory[Vec3d, SInt](new ArrayVec3d(cast(FactoryDouble1SInt)))
  implicit final lazy val FactoryVec3dUInt = factory[Vec3d, UInt](new ArrayVec3d(cast(FactoryDouble1UInt)))
  implicit final lazy val FactoryVec3dHalfFloat = factory[Vec3d, HalfFloat](new ArrayVec3d(cast(FactoryDouble1HalfFloat)))
  implicit final lazy val FactoryVec3dRawFloat = factory[Vec3d, RawFloat](new ArrayVec3dRawFloat())
  implicit final lazy val FactoryVec3dRawDouble = factory[Vec3d, RawDouble](new ArrayVec3d(cast(FactoryDouble1RawDouble)))
  
  // Vec4d
  implicit final lazy val FactoryVec4dSByte = factory[Vec4d, SByte](new ArrayVec4d(cast(FactoryDouble1SByte)))
  implicit final lazy val FactoryVec4dUByte = factory[Vec4d, UByte](new ArrayVec4dUByte())
  implicit final lazy val FactoryVec4dSShort = factory[Vec4d, SShort](new ArrayVec4d(cast(FactoryDouble1SShort)))
  implicit final lazy val FactoryVec4dUShort = factory[Vec4d, UShort](new ArrayVec4d(cast(FactoryDouble1UShort)))
  implicit final lazy val FactoryVec4dSInt = factory[Vec4d, SInt](new ArrayVec4d(cast(FactoryDouble1SInt)))
  implicit final lazy val FactoryVec4dUInt = factory[Vec4d, UInt](new ArrayVec4d(cast(FactoryDouble1UInt)))
  implicit final lazy val FactoryVec4dHalfFloat = factory[Vec4d, HalfFloat](new ArrayVec4d(cast(FactoryDouble1HalfFloat)))
  implicit final lazy val FactoryVec4dRawFloat = factory[Vec4d, RawFloat](new ArrayVec4dRawFloat())
  implicit final lazy val FactoryVec4dRawDouble = factory[Vec4d, RawDouble](new ArrayVec4d(cast(FactoryDouble1RawDouble)))
}
