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
import simplex3d.buffer.optimize._
import simplex3d.buffer.doublem._
import simplex3d.buffer.doublem.optimized._


/**
 * @author Aleksey Nikiforov (lex)
 */
package object doublem {

  private final type PrimitiveFactory[R <: DefinedDouble] = SimpleFactoryRef[Double1, R]
  private final type SimpleFactory[E <: Composite, R <: DefinedDouble] = SimpleFactoryRef[E, R]
  private final type GenFactory[E <: Composite, R <: DefinedDouble] = TemplateGenFactoryRef[E, R]

  private final def dataArray[R <: DefinedDouble](f: SimpleFactoryRef[Double1, R]) =
    f.factory.asInstanceOf[DataArray[Double1, R]]


  // Double1
  implicit final lazy val FactoryDouble1SByte = new PrimitiveFactory[SByte](new ArrayDouble1SByte)
  implicit final lazy val FactoryDouble1UByte = new PrimitiveFactory[UByte](new ArrayDouble1UByte)
  implicit final lazy val FactoryDouble1SShort = new PrimitiveFactory[SShort](new ArrayDouble1SShort)
  implicit final lazy val FactoryDouble1UShort = new PrimitiveFactory[UShort](new ArrayDouble1UShort)
  implicit final lazy val FactoryDouble1SInt = new PrimitiveFactory[SInt](new ArrayDouble1SInt)
  implicit final lazy val FactoryDouble1UInt = new PrimitiveFactory[UInt](new ArrayDouble1UInt)
  implicit final lazy val FactoryDouble1HalfFloat = new PrimitiveFactory[HalfFloat](new ArrayDouble1HalfFloat)
  implicit final lazy val FactoryDouble1RawFloat = new PrimitiveFactory[RawFloat](new ArrayDouble1RawFloat)
  implicit final val FactoryDouble1RawDouble = new PrimitiveFactory[RawDouble](new ArrayDouble1RawDouble)


  // Vec2d
  private val vec2dTemplateClass = "simplex3d.buffer.doublem.optimized.ArrayVec2dRawFloat"
  private val vec2dTemplateString = "RawFloat"

  implicit final lazy val FactoryVec2dSByte = new GenFactory[Vec2d, SByte](
    vec2dTemplateClass,
    vec2dTemplateString,
    new ArrayVec2d(dataArray(FactoryDouble1SByte))
  )
  implicit final lazy val FactoryVec2dUByte = new GenFactory[Vec2d, UByte](
    vec2dTemplateClass,
    vec2dTemplateString,
    new ArrayVec2d(dataArray(FactoryDouble1UByte))
  )

  implicit final lazy val FactoryVec2dSShort = new GenFactory[Vec2d, SShort](
    vec2dTemplateClass,
    vec2dTemplateString,
    new ArrayVec2d(dataArray(FactoryDouble1SShort))
  )
  implicit final lazy val FactoryVec2dUShort = new GenFactory[Vec2d, UShort](
    vec2dTemplateClass,
    vec2dTemplateString,
    new ArrayVec2d(dataArray(FactoryDouble1UShort))
  )

  implicit final lazy val FactoryVec2dSInt = new GenFactory[Vec2d, SInt](
    vec2dTemplateClass,
    vec2dTemplateString,
    new ArrayVec2d(dataArray(FactoryDouble1SInt))
  )
  implicit final lazy val FactoryVec2dUInt = new GenFactory[Vec2d, UInt](
    vec2dTemplateClass,
    vec2dTemplateString,
    new ArrayVec2d(dataArray(FactoryDouble1UInt))
  )

  implicit final lazy val FactoryVec2dHalfFloat = new GenFactory[Vec2d, HalfFloat](
    vec2dTemplateClass,
    vec2dTemplateString,
    new ArrayVec2d(dataArray(FactoryDouble1HalfFloat))
  )

  implicit final lazy val FactoryVec2dRawFloat = new SimpleFactory[Vec2d, RawFloat](new ArrayVec2dRawFloat)
  
  implicit final val FactoryVec2dRawDouble = new GenFactory[Vec2d, RawDouble](
    vec2dTemplateClass,
    vec2dTemplateString,
    new ArrayVec2d(dataArray(FactoryDouble1RawDouble))
  )


  // Vec3d
  private val vec3dTemplateClass = "simplex3d.buffer.doublem.optimized.ArrayVec3dRawFloat"
  private val vec3dTemplateString = "RawFloat"

  implicit final lazy val FactoryVec3dSByte = new GenFactory[Vec3d, SByte](
    vec3dTemplateClass,
    vec3dTemplateString,
    new ArrayVec3d(dataArray(FactoryDouble1SByte))
  )
  implicit final lazy val FactoryVec3dUByte = new GenFactory[Vec3d, UByte](
    vec3dTemplateClass,
    vec3dTemplateString,
    new ArrayVec3d(dataArray(FactoryDouble1UByte))
  )

  implicit final lazy val FactoryVec3dSShort = new GenFactory[Vec3d, SShort](
    vec3dTemplateClass,
    vec3dTemplateString,
    new ArrayVec3d(dataArray(FactoryDouble1SShort))
  )
  implicit final lazy val FactoryVec3dUShort = new GenFactory[Vec3d, UShort](
    vec3dTemplateClass,
    vec3dTemplateString,
    new ArrayVec3d(dataArray(FactoryDouble1UShort))
  )

  implicit final lazy val FactoryVec3dSInt = new GenFactory[Vec3d, SInt](
    vec3dTemplateClass,
    vec3dTemplateString,
    new ArrayVec3d(dataArray(FactoryDouble1SInt))
  )
  implicit final lazy val FactoryVec3dUInt = new GenFactory[Vec3d, UInt](
    vec3dTemplateClass,
    vec3dTemplateString,
    new ArrayVec3d(dataArray(FactoryDouble1UInt))
  )

  implicit final lazy val FactoryVec3dHalfFloat = new GenFactory[Vec3d, HalfFloat](
    vec3dTemplateClass,
    vec3dTemplateString,
    new ArrayVec3d(dataArray(FactoryDouble1HalfFloat))
  )

  implicit final lazy val FactoryVec3dRawFloat = new SimpleFactory[Vec3d, RawFloat](new ArrayVec3dRawFloat)
  
  implicit final val FactoryVec3dRawDouble = new GenFactory[Vec3d, RawDouble](
    vec3dTemplateClass,
    vec3dTemplateString,
    new ArrayVec3d(dataArray(FactoryDouble1RawDouble))
  )


  // Vec4d
  private val vec4dTemplateClass = "simplex3d.buffer.doublem.optimized.ArrayVec4dUByte"
  private val vec4dTemplateString = "UByte"

  implicit final lazy val FactoryVec4dSByte = new GenFactory[Vec4d, SByte](
    vec4dTemplateClass,
    vec4dTemplateString,
    new ArrayVec4d(dataArray(FactoryDouble1SByte))
  )
  implicit final lazy val FactoryVec4dUByte = new SimpleFactory[Vec4d, UByte](new ArrayVec4dUByte)

  implicit final lazy val FactoryVec4dSShort = new GenFactory[Vec4d, SShort](
    vec4dTemplateClass,
    vec4dTemplateString,
    new ArrayVec4d(dataArray(FactoryDouble1SShort))
  )
  implicit final lazy val FactoryVec4dUShort = new GenFactory[Vec4d, UShort](
    vec4dTemplateClass,
    vec4dTemplateString,
    new ArrayVec4d(dataArray(FactoryDouble1UShort))
  )

  implicit final lazy val FactoryVec4dSInt = new GenFactory[Vec4d, SInt](
    vec4dTemplateClass,
    vec4dTemplateString,
    new ArrayVec4d(dataArray(FactoryDouble1SInt))
  )
  implicit final lazy val FactoryVec4dUInt = new GenFactory[Vec4d, UInt](
    vec4dTemplateClass,
    vec4dTemplateString,
    new ArrayVec4d(dataArray(FactoryDouble1UInt))
  )

  implicit final lazy val FactoryVec4dHalfFloat = new GenFactory[Vec4d, HalfFloat](
    vec4dTemplateClass,
    vec4dTemplateString,
    new ArrayVec4d(dataArray(FactoryDouble1HalfFloat))
  )

  implicit final val FactoryVec4dRawFloat = new GenFactory[Vec4d, RawFloat](
    vec4dTemplateClass,
    vec4dTemplateString,
    new ArrayVec4d(dataArray(FactoryDouble1RawFloat))
  )

  implicit final val FactoryVec4dRawDouble = new GenFactory[Vec4d, RawDouble](
    vec4dTemplateClass,
    vec4dTemplateString,
    new ArrayVec4d(dataArray(FactoryDouble1RawDouble))
  )
}
