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

  private final def primitiveFactory[R <: DefinedDouble](s: DataSeq[RDouble, R]) :DataFactory[RDouble, R] = s
  private final def factory[E <: Composite, R <: DefinedDouble](s: DataSeq[E, R]) :DataFactory[E, R] = s
  private final def cast[R <: DefinedDouble](f: DataFactory[RDouble, R]) = f.asInstanceOf[DataArray[RDouble, R]]


  // RDouble
  implicit final lazy val FactoryRDoubleSByte = primitiveFactory[SByte](new ArrayRDoubleSByte)
  implicit final lazy val FactoryRDoubleUByte = primitiveFactory[UByte](new ArrayRDoubleUByte)
  implicit final lazy val FactoryRDoubleSShort = primitiveFactory[SShort](new ArrayRDoubleSShort)
  implicit final lazy val FactoryRDoubleUShort = primitiveFactory[UShort](new ArrayRDoubleUShort)
  implicit final lazy val FactoryRDoubleSInt = primitiveFactory[SInt](new ArrayRDoubleSInt)
  implicit final lazy val FactoryRDoubleUInt = primitiveFactory[UInt](new ArrayRDoubleUInt)
  implicit final lazy val FactoryRDoubleHFloat = primitiveFactory[HFloat](new ArrayRDoubleHFloat)
  implicit final lazy val FactoryRDoubleRFloat = primitiveFactory[RFloat](new ArrayRDoubleRFloat)
  implicit final lazy val FactoryRDoubleRDouble = primitiveFactory[RDouble](new ArrayRDoubleRDouble)


  // Vec2d
  implicit final lazy val FactoryVec2dSByte = factory[Vec2d, SByte](new ArrayVec2d(cast(FactoryRDoubleSByte)))
  implicit final lazy val FactoryVec2dUByte = factory[Vec2d, UByte](new ArrayVec2d(cast(FactoryRDoubleUByte)))
  implicit final lazy val FactoryVec2dSShort = factory[Vec2d, SShort](new ArrayVec2d(cast(FactoryRDoubleSShort)))
  implicit final lazy val FactoryVec2dUShort = factory[Vec2d, UShort](new ArrayVec2d(cast(FactoryRDoubleUShort)))
  implicit final lazy val FactoryVec2dSInt = factory[Vec2d, SInt](new ArrayVec2d(cast(FactoryRDoubleSInt)))
  implicit final lazy val FactoryVec2dUInt = factory[Vec2d, UInt](new ArrayVec2d(cast(FactoryRDoubleUInt)))
  implicit final lazy val FactoryVec2dHFloat = factory[Vec2d, HFloat](new ArrayVec2d(cast(FactoryRDoubleHFloat)))
  implicit final lazy val FactoryVec2dRFloat = factory[Vec2d, RFloat](new ArrayVec2dRFloat())
  implicit final lazy val FactoryVec2dRDouble = factory[Vec2d, RDouble](new ArrayVec2d(cast(FactoryRDoubleRDouble)))
  
  // Vec3d
  implicit final lazy val FactoryVec3dSByte = factory[Vec3d, SByte](new ArrayVec3d(cast(FactoryRDoubleSByte)))
  implicit final lazy val FactoryVec3dUByte = factory[Vec3d, UByte](new ArrayVec3dUByte())
  implicit final lazy val FactoryVec3dSShort = factory[Vec3d, SShort](new ArrayVec3d(cast(FactoryRDoubleSShort)))
  implicit final lazy val FactoryVec3dUShort = factory[Vec3d, UShort](new ArrayVec3d(cast(FactoryRDoubleUShort)))
  implicit final lazy val FactoryVec3dSInt = factory[Vec3d, SInt](new ArrayVec3d(cast(FactoryRDoubleSInt)))
  implicit final lazy val FactoryVec3dUInt = factory[Vec3d, UInt](new ArrayVec3d(cast(FactoryRDoubleUInt)))
  implicit final lazy val FactoryVec3dHFloat = factory[Vec3d, HFloat](new ArrayVec3d(cast(FactoryRDoubleHFloat)))
  implicit final lazy val FactoryVec3dRFloat = factory[Vec3d, RFloat](new ArrayVec3dRFloat())
  implicit final lazy val FactoryVec3dRDouble = factory[Vec3d, RDouble](new ArrayVec3d(cast(FactoryRDoubleRDouble)))
  
  // Vec4d
  implicit final lazy val FactoryVec4dSByte = factory[Vec4d, SByte](new ArrayVec4d(cast(FactoryRDoubleSByte)))
  implicit final lazy val FactoryVec4dUByte = factory[Vec4d, UByte](new ArrayVec4dUByte())
  implicit final lazy val FactoryVec4dSShort = factory[Vec4d, SShort](new ArrayVec4d(cast(FactoryRDoubleSShort)))
  implicit final lazy val FactoryVec4dUShort = factory[Vec4d, UShort](new ArrayVec4d(cast(FactoryRDoubleUShort)))
  implicit final lazy val FactoryVec4dSInt = factory[Vec4d, SInt](new ArrayVec4d(cast(FactoryRDoubleSInt)))
  implicit final lazy val FactoryVec4dUInt = factory[Vec4d, UInt](new ArrayVec4d(cast(FactoryRDoubleUInt)))
  implicit final lazy val FactoryVec4dHFloat = factory[Vec4d, HFloat](new ArrayVec4d(cast(FactoryRDoubleHFloat)))
  implicit final lazy val FactoryVec4dRFloat = factory[Vec4d, RFloat](new ArrayVec4dRFloat())
  implicit final lazy val FactoryVec4dRDouble = factory[Vec4d, RDouble](new ArrayVec4d(cast(FactoryRDoubleRDouble)))
}
