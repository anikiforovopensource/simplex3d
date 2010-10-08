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
import simplex3d.buffer.optimize._
import simplex3d.buffer.floatm._
import simplex3d.buffer.floatm.optimized._


/**
 * @author Aleksey Nikiforov (lex)
 */
package object floatm {

  private final type PrimitiveFactory[R <: DefinedFloat] = SimpleFactoryRef[Float1, R]
  private final type SimpleFactory[E <: Composite, R <: DefinedFloat] = SimpleFactoryRef[E, R]
  private final type GenFactory[E <: Composite, R <: DefinedFloat] = TemplateGenFactoryRef[E, R]

  private final def dataArray[R <: DefinedFloat](f: SimpleFactoryRef[Float1, R]) =
    f.factory.asInstanceOf[DataArray[Float1, R]]


  // Float1
  implicit final lazy val FactoryFloat1SByte = new PrimitiveFactory[SByte](new ArrayFloat1SByte)
  implicit final lazy val FactoryFloat1UByte = new PrimitiveFactory[UByte](new ArrayFloat1UByte)
  implicit final lazy val FactoryFloat1SShort = new PrimitiveFactory[SShort](new ArrayFloat1SShort)
  implicit final lazy val FactoryFloat1UShort = new PrimitiveFactory[UShort](new ArrayFloat1UShort)
  implicit final lazy val FactoryFloat1SInt = new PrimitiveFactory[SInt](new ArrayFloat1SInt)
  implicit final lazy val FactoryFloat1UInt = new PrimitiveFactory[UInt](new ArrayFloat1UInt)
  implicit final lazy val FactoryFloat1HalfFloat = new PrimitiveFactory[HalfFloat](new ArrayFloat1HalfFloat)
  implicit final lazy val FactoryFloat1RawFloat = new PrimitiveFactory[RawFloat](new ArrayFloat1RawFloat)


  // Vec2f
  private val vec2fTemplateClass = "simplex3d.buffer.floatm.optimized.ArrayVec2fRawFloat"
  private val vec2fTemplateString = "RawFloat"

  implicit final lazy val FactoryVec2fSByte = new GenFactory[Vec2f, SByte](
    vec2fTemplateClass,
    vec2fTemplateString,
    new ArrayVec2f(dataArray(FactoryFloat1SByte))
  )
  implicit final lazy val FactoryVec2fUByte = new GenFactory[Vec2f, UByte](
    vec2fTemplateClass,
    vec2fTemplateString,
    new ArrayVec2f(dataArray(FactoryFloat1UByte))
  )

  implicit final lazy val FactoryVec2fSShort = new GenFactory[Vec2f, SShort](
    vec2fTemplateClass,
    vec2fTemplateString,
    new ArrayVec2f(dataArray(FactoryFloat1SShort))
  )
  implicit final lazy val FactoryVec2fUShort = new GenFactory[Vec2f, UShort](
    vec2fTemplateClass,
    vec2fTemplateString,
    new ArrayVec2f(dataArray(FactoryFloat1UShort))
  )

  implicit final lazy val FactoryVec2fSInt = new GenFactory[Vec2f, SInt](
    vec2fTemplateClass,
    vec2fTemplateString,
    new ArrayVec2f(dataArray(FactoryFloat1SInt))
  )
  implicit final lazy val FactoryVec2fUInt = new GenFactory[Vec2f, UInt](
    vec2fTemplateClass,
    vec2fTemplateString,
    new ArrayVec2f(dataArray(FactoryFloat1UInt))
  )

  implicit final lazy val FactoryVec2fHalfFloat = new GenFactory[Vec2f, HalfFloat](
    vec2fTemplateClass,
    vec2fTemplateString,
    new ArrayVec2f(dataArray(FactoryFloat1HalfFloat))
  )

  implicit final lazy val FactoryVec2fRawFloat = new SimpleFactory[Vec2f, RawFloat](new ArrayVec2fRawFloat)


  // Vec3f
  private val vec3fTemplateClass = "simplex3d.buffer.floatm.optimized.ArrayVec3fRawFloat"
  private val vec3fTemplateString = "RawFloat"

  implicit final lazy val FactoryVec3fSByte = new GenFactory[Vec3f, SByte](
    vec3fTemplateClass,
    vec3fTemplateString,
    new ArrayVec3f(dataArray(FactoryFloat1SByte))
  )
  implicit final lazy val FactoryVec3fUByte = new GenFactory[Vec3f, UByte](
    vec3fTemplateClass,
    vec3fTemplateString,
    new ArrayVec3f(dataArray(FactoryFloat1UByte))
  )

  implicit final lazy val FactoryVec3fSShort = new GenFactory[Vec3f, SShort](
    vec3fTemplateClass,
    vec3fTemplateString,
    new ArrayVec3f(dataArray(FactoryFloat1SShort))
  )
  implicit final lazy val FactoryVec3fUShort = new GenFactory[Vec3f, UShort](
    vec3fTemplateClass,
    vec3fTemplateString,
    new ArrayVec3f(dataArray(FactoryFloat1UShort))
  )

  implicit final lazy val FactoryVec3fSInt = new GenFactory[Vec3f, SInt](
    vec3fTemplateClass,
    vec3fTemplateString,
    new ArrayVec3f(dataArray(FactoryFloat1SInt))
  )
  implicit final lazy val FactoryVec3fUInt = new GenFactory[Vec3f, UInt](
    vec3fTemplateClass,
    vec3fTemplateString,
    new ArrayVec3f(dataArray(FactoryFloat1UInt))
  )

  implicit final lazy val FactoryVec3fHalfFloat = new GenFactory[Vec3f, HalfFloat](
    vec3fTemplateClass,
    vec3fTemplateString,
    new ArrayVec3f(dataArray(FactoryFloat1HalfFloat))
  )

  implicit final lazy val FactoryVec3fRawFloat = new SimpleFactory[Vec3f, RawFloat](new ArrayVec3fRawFloat)


  // Vec4f
  private val vec4fTemplateClass = "simplex3d.buffer.floatm.optimized.ArrayVec4fUByte"
  private val vec4fTemplateString = "UByte"

  implicit final lazy val FactoryVec4fSByte = new GenFactory[Vec4f, SByte](
    vec4fTemplateClass,
    vec4fTemplateString,
    new ArrayVec4f(dataArray(FactoryFloat1SByte))
  )
  implicit final lazy val FactoryVec4fUByte = new SimpleFactory[Vec4f, UByte](new ArrayVec4fUByte)

  implicit final lazy val FactoryVec4fSShort = new GenFactory[Vec4f, SShort](
    vec4fTemplateClass,
    vec4fTemplateString,
    new ArrayVec4f(dataArray(FactoryFloat1SShort))
  )
  implicit final lazy val FactoryVec4fUShort = new GenFactory[Vec4f, UShort](
    vec4fTemplateClass,
    vec4fTemplateString,
    new ArrayVec4f(dataArray(FactoryFloat1UShort))
  )

  implicit final lazy val FactoryVec4fSInt = new GenFactory[Vec4f, SInt](
    vec4fTemplateClass,
    vec4fTemplateString,
    new ArrayVec4f(dataArray(FactoryFloat1SInt))
  )
  implicit final lazy val FactoryVec4fUInt = new GenFactory[Vec4f, UInt](
    vec4fTemplateClass,
    vec4fTemplateString,
    new ArrayVec4f(dataArray(FactoryFloat1UInt))
  )

  implicit final lazy val FactoryVec4fHalfFloat = new GenFactory[Vec4f, HalfFloat](
    vec4fTemplateClass,
    vec4fTemplateString,
    new ArrayVec4f(dataArray(FactoryFloat1HalfFloat))
  )

  implicit final lazy val FactoryVec4fRawFloat = new GenFactory[Vec4f, RawFloat](
    vec4fTemplateClass,
    vec4fTemplateString,
    new ArrayVec4f(dataArray(FactoryFloat1RawFloat))
  )
}
