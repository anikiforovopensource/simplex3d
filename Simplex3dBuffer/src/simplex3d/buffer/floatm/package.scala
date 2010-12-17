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

  private final def primitiveFactory[R <: DefinedFloat](s: DataSeq[RFloat, R]) :DataFactory[RFloat, R] = s
  private final def factory[E <: Composite, R <: DefinedFloat](s: DataSeq[E, R]) :DataFactory[E, R] = s
  private final def cast[R <: DefinedFloat](f: DataFactory[RFloat, R]) = f.asInstanceOf[DataArray[RFloat, R]]


  // RFloat
  implicit final lazy val FactoryRFloatSByte = primitiveFactory[SByte](new ArrayRFloatSByte)
  implicit final lazy val FactoryRFloatUByte = primitiveFactory[UByte](new ArrayRFloatUByte)
  implicit final lazy val FactoryRFloatSShort = primitiveFactory[SShort](new ArrayRFloatSShort)
  implicit final lazy val FactoryRFloatUShort = primitiveFactory[UShort](new ArrayRFloatUShort)
  implicit final lazy val FactoryRFloatSInt = primitiveFactory[SInt](new ArrayRFloatSInt)
  implicit final lazy val FactoryRFloatUInt = primitiveFactory[UInt](new ArrayRFloatUInt)
  implicit final lazy val FactoryRFloatHFloat = primitiveFactory[HFloat](new ArrayRFloatHFloat)
  implicit final lazy val FactoryRFloatRFloat = primitiveFactory[RFloat](new ArrayRFloatRFloat)


  // Vec2f
  implicit final lazy val FactoryVec2fSByte = factory[Vec2f, SByte](new ArrayVec2f(cast(FactoryRFloatSByte)))
  implicit final lazy val FactoryVec2fUByte = factory[Vec2f, UByte](new ArrayVec2f(cast(FactoryRFloatUByte)))
  implicit final lazy val FactoryVec2fSShort = factory[Vec2f, SShort](new ArrayVec2f(cast(FactoryRFloatSShort)))
  implicit final lazy val FactoryVec2fUShort = factory[Vec2f, UShort](new ArrayVec2f(cast(FactoryRFloatUShort)))
  implicit final lazy val FactoryVec2fSInt = factory[Vec2f, SInt](new ArrayVec2f(cast(FactoryRFloatSInt)))
  implicit final lazy val FactoryVec2fUInt = factory[Vec2f, UInt](new ArrayVec2f(cast(FactoryRFloatUInt)))
  implicit final lazy val FactoryVec2fHFloat = factory[Vec2f, HFloat](new ArrayVec2f(cast(FactoryRFloatHFloat)))
  implicit final lazy val FactoryVec2fRFloat = factory[Vec2f, RFloat](new ArrayVec2fRFloat())

  // Vec3f
  implicit final lazy val FactoryVec3fSByte = factory[Vec3f, SByte](new ArrayVec3f(cast(FactoryRFloatSByte)))
  implicit final lazy val FactoryVec3fUByte = factory[Vec3f, UByte](new ArrayVec3fUByte())
  implicit final lazy val FactoryVec3fSShort = factory[Vec3f, SShort](new ArrayVec3f(cast(FactoryRFloatSShort)))
  implicit final lazy val FactoryVec3fUShort = factory[Vec3f, UShort](new ArrayVec3f(cast(FactoryRFloatUShort)))
  implicit final lazy val FactoryVec3fSInt = factory[Vec3f, SInt](new ArrayVec3f(cast(FactoryRFloatSInt)))
  implicit final lazy val FactoryVec3fUInt = factory[Vec3f, UInt](new ArrayVec3f(cast(FactoryRFloatUInt)))
  implicit final lazy val FactoryVec3fHFloat = factory[Vec3f, HFloat](new ArrayVec3f(cast(FactoryRFloatHFloat)))
  implicit final lazy val FactoryVec3fRFloat = factory[Vec3f, RFloat](new ArrayVec3fRFloat())
  
  // Vec4f
  implicit final lazy val FactoryVec4fSByte = factory[Vec4f, SByte](new ArrayVec4f(cast(FactoryRFloatSByte)))
  implicit final lazy val FactoryVec4fUByte = factory[Vec4f, UByte](new ArrayVec4fUByte())
  implicit final lazy val FactoryVec4fSShort = factory[Vec4f, SShort](new ArrayVec4f(cast(FactoryRFloatSShort)))
  implicit final lazy val FactoryVec4fUShort = factory[Vec4f, UShort](new ArrayVec4f(cast(FactoryRFloatUShort)))
  implicit final lazy val FactoryVec4fSInt = factory[Vec4f, SInt](new ArrayVec4f(cast(FactoryRFloatSInt)))
  implicit final lazy val FactoryVec4fUInt = factory[Vec4f, UInt](new ArrayVec4f(cast(FactoryRFloatUInt)))
  implicit final lazy val FactoryVec4fHFloat = factory[Vec4f, HFloat](new ArrayVec4f(cast(FactoryRFloatHFloat)))
  implicit final lazy val FactoryVec4fRFloat = factory[Vec4f, RFloat](new ArrayVec4fRFloat())
}
