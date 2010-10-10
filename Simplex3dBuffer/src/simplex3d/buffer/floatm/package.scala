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

  private final def primitiveFactory[R <: DefinedFloat](s: DataSeq[Float1, R]) :FactoryRef[Float1, R] = {
    new SimpleFactoryRef[Float1, R](s)
  }
  private final def simpleFactory[E <: Composite, R <: DefinedFloat](s: DataSeq[E, R]) :FactoryRef[E, R] = {
    new SimpleFactoryRef[E, R](s)
  }
  private final def genFactory[E <: Composite, R <: DefinedFloat](
    template: String, replace: String, fallback: DataSeq[E, R]
  ) :FactoryRef[E, R] = {
    new TemplateGenFactoryRef[E, R](template, replace, fallback)
  }

  private final def dataArray[R <: DefinedFloat](f: FactoryRef[Float1, R]) = {
    f.factory.asInstanceOf[DataArray[Float1, R]]
  }


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
  private val vec2fTemplateClass = "simplex3d.buffer.floatm.optimized.ArrayVec2fRawFloat"
  private val vec2fTemplateString = "RawFloat"

  implicit final lazy val FactoryVec2fSByte = genFactory[Vec2f, SByte](
    vec2fTemplateClass,
    vec2fTemplateString,
    new ArrayVec2f(dataArray(FactoryFloat1SByte))
  )
  implicit final lazy val FactoryVec2fUByte = genFactory[Vec2f, UByte](
    vec2fTemplateClass,
    vec2fTemplateString,
    new ArrayVec2f(dataArray(FactoryFloat1UByte))
  )

  implicit final lazy val FactoryVec2fSShort = genFactory[Vec2f, SShort](
    vec2fTemplateClass,
    vec2fTemplateString,
    new ArrayVec2f(dataArray(FactoryFloat1SShort))
  )
  implicit final lazy val FactoryVec2fUShort = genFactory[Vec2f, UShort](
    vec2fTemplateClass,
    vec2fTemplateString,
    new ArrayVec2f(dataArray(FactoryFloat1UShort))
  )

  implicit final lazy val FactoryVec2fSInt = genFactory[Vec2f, SInt](
    vec2fTemplateClass,
    vec2fTemplateString,
    new ArrayVec2f(dataArray(FactoryFloat1SInt))
  )
  implicit final lazy val FactoryVec2fUInt = genFactory[Vec2f, UInt](
    vec2fTemplateClass,
    vec2fTemplateString,
    new ArrayVec2f(dataArray(FactoryFloat1UInt))
  )

  implicit final lazy val FactoryVec2fHalfFloat = genFactory[Vec2f, HalfFloat](
    vec2fTemplateClass,
    vec2fTemplateString,
    new ArrayVec2f(dataArray(FactoryFloat1HalfFloat))
  )

  implicit final lazy val FactoryVec2fRawFloat = simpleFactory[Vec2f, RawFloat](new ArrayVec2fRawFloat)


  // Vec3f
  private val vec3fTemplateClass = "simplex3d.buffer.floatm.optimized.ArrayVec3fRawFloat"
  private val vec3fTemplateString = "RawFloat"

  implicit final lazy val FactoryVec3fSByte = genFactory[Vec3f, SByte](
    vec3fTemplateClass,
    vec3fTemplateString,
    new ArrayVec3f(dataArray(FactoryFloat1SByte))
  )
  implicit final lazy val FactoryVec3fUByte = genFactory[Vec3f, UByte](
    vec3fTemplateClass,
    vec3fTemplateString,
    new ArrayVec3f(dataArray(FactoryFloat1UByte))
  )

  implicit final lazy val FactoryVec3fSShort = genFactory[Vec3f, SShort](
    vec3fTemplateClass,
    vec3fTemplateString,
    new ArrayVec3f(dataArray(FactoryFloat1SShort))
  )
  implicit final lazy val FactoryVec3fUShort = genFactory[Vec3f, UShort](
    vec3fTemplateClass,
    vec3fTemplateString,
    new ArrayVec3f(dataArray(FactoryFloat1UShort))
  )

  implicit final lazy val FactoryVec3fSInt = genFactory[Vec3f, SInt](
    vec3fTemplateClass,
    vec3fTemplateString,
    new ArrayVec3f(dataArray(FactoryFloat1SInt))
  )
  implicit final lazy val FactoryVec3fUInt = genFactory[Vec3f, UInt](
    vec3fTemplateClass,
    vec3fTemplateString,
    new ArrayVec3f(dataArray(FactoryFloat1UInt))
  )

  implicit final lazy val FactoryVec3fHalfFloat = genFactory[Vec3f, HalfFloat](
    vec3fTemplateClass,
    vec3fTemplateString,
    new ArrayVec3f(dataArray(FactoryFloat1HalfFloat))
  )

  implicit final lazy val FactoryVec3fRawFloat = simpleFactory[Vec3f, RawFloat](new ArrayVec3fRawFloat)


  // Vec4f
  private val vec4fTemplateClass = "simplex3d.buffer.floatm.optimized.ArrayVec4fUByte"
  private val vec4fTemplateString = "UByte"

  implicit final lazy val FactoryVec4fSByte = genFactory[Vec4f, SByte](
    vec4fTemplateClass,
    vec4fTemplateString,
    new ArrayVec4f(dataArray(FactoryFloat1SByte))
  )
  implicit final lazy val FactoryVec4fUByte = simpleFactory[Vec4f, UByte](new ArrayVec4fUByte)

  implicit final lazy val FactoryVec4fSShort = genFactory[Vec4f, SShort](
    vec4fTemplateClass,
    vec4fTemplateString,
    new ArrayVec4f(dataArray(FactoryFloat1SShort))
  )
  implicit final lazy val FactoryVec4fUShort = genFactory[Vec4f, UShort](
    vec4fTemplateClass,
    vec4fTemplateString,
    new ArrayVec4f(dataArray(FactoryFloat1UShort))
  )

  implicit final lazy val FactoryVec4fSInt = genFactory[Vec4f, SInt](
    vec4fTemplateClass,
    vec4fTemplateString,
    new ArrayVec4f(dataArray(FactoryFloat1SInt))
  )
  implicit final lazy val FactoryVec4fUInt = genFactory[Vec4f, UInt](
    vec4fTemplateClass,
    vec4fTemplateString,
    new ArrayVec4f(dataArray(FactoryFloat1UInt))
  )

  implicit final lazy val FactoryVec4fHalfFloat = genFactory[Vec4f, HalfFloat](
    vec4fTemplateClass,
    vec4fTemplateString,
    new ArrayVec4f(dataArray(FactoryFloat1HalfFloat))
  )

  implicit final lazy val FactoryVec4fRawFloat = genFactory[Vec4f, RawFloat](
    vec4fTemplateClass,
    vec4fTemplateString,
    new ArrayVec4f(dataArray(FactoryFloat1RawFloat))
  )
}
