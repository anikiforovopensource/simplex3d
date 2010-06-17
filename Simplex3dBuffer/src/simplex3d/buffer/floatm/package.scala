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
import simplex3d.buffer.optimize._
import simplex3d.buffer.floatm._
import simplex3d.buffer.floatm.optimized._


/**
 * @author Aleksey Nikiforov (lex)
 */
package object floatm {

  private final type F[T <: ElemType, D <: RawType] = SimpleFactoryRef[T, D]
  private final type G[T <: ElemType, D <: RawType] =TemplateGenFactoryRef[T, D]
  private final def da[T <: ElemType, D <: RawType](f: FactoryRef[T, D]) =
    f.factory.asInstanceOf[DataArray[T, D]]


  // Float1
  implicit final val FactoryFloat1SByte = new F(new ArrayFloat1SByte)
  implicit final val FactoryFloat1UByte = new F(new ArrayFloat1UByte)
  implicit final val FactoryFloat1NSByte = new F(new ArrayFloat1NSByte)
  implicit final val FactoryFloat1NUByte = new F(new ArrayFloat1NUByte)
  
  implicit final val FactoryFloat1SShort = new F(new ArrayFloat1SShort)
  implicit final val FactoryFloat1UShort = new F(new ArrayFloat1UShort)
  implicit final val FactoryFloat1NSShort = new F(new ArrayFloat1NSShort)
  implicit final val FactoryFloat1NUShort = new F(new ArrayFloat1NUShort)

  implicit final val FactoryFloat1SInt = new F(new ArrayFloat1SInt)
  implicit final val FactoryFloat1UInt = new F(new ArrayFloat1UInt)
  implicit final val FactoryFloat1NSInt = new F(new ArrayFloat1NSInt)
  implicit final val FactoryFloat1NUInt = new F(new ArrayFloat1NUInt)

  implicit final val FactoryFloat1HalfFloat = new F(new ArrayFloat1HalfFloat)
  implicit final val FactoryFloat1RawFloat = new F(new ArrayFloat1RawFloat)


  // Vec2f
  private val vec2fTemplateClass =
    "simplex3d.buffer.floatm.optimized.ArrayVec2fRawFloat"
  private val vec2fTemplate = "RawFloat"

  implicit final val FactoryVec2fSByte = new G(
    vec2fTemplateClass,
    vec2fTemplate,
    new ArrayVec2f[SByte](da(FactoryFloat1SByte))
  )
  implicit final val FactoryVec2fUByte = new G(
    vec2fTemplateClass,
    vec2fTemplate,
    new ArrayVec2f[UByte](da(FactoryFloat1UByte))
  )
  implicit final val FactoryVec2fNSByte = new G(
    vec2fTemplateClass,
    vec2fTemplate,
    new ArrayVec2f[NSByte](da(FactoryFloat1NSByte))
  )
  implicit final val FactoryVec2fNUByte = new G(
    vec2fTemplateClass,
    vec2fTemplate,
    new ArrayVec2f[NUByte](da(FactoryFloat1NUByte))
  )

  implicit final val FactoryVec2fSShort = new G(
    vec2fTemplateClass,
    vec2fTemplate,
    new ArrayVec2f[SShort](da(FactoryFloat1SShort))
  )
  implicit final val FactoryVec2fUShort = new G(
    vec2fTemplateClass,
    vec2fTemplate,
    new ArrayVec2f[UShort](da(FactoryFloat1UShort))
  )
  implicit final val FactoryVec2fNSShort = new G(
    vec2fTemplateClass,
    vec2fTemplate,
    new ArrayVec2f[NSShort](da(FactoryFloat1NSShort))
  )
  implicit final val FactoryVec2fNUShort = new G(
    vec2fTemplateClass,
    vec2fTemplate,
    new ArrayVec2f[NUShort](da(FactoryFloat1NUShort))
  )

  implicit final val FactoryVec2fSInt = new G(
    vec2fTemplateClass,
    vec2fTemplate,
    new ArrayVec2f[SInt](da(FactoryFloat1SInt))
  )
  implicit final val FactoryVec2fUInt = new G(
    vec2fTemplateClass,
    vec2fTemplate,
    new ArrayVec2f[UInt](da(FactoryFloat1UInt))
  )
  implicit final val FactoryVec2fNSInt = new G(
    vec2fTemplateClass,
    vec2fTemplate,
    new ArrayVec2f[NSInt](da(FactoryFloat1NSInt))
  )
  implicit final val FactoryVec2fNUInt = new G(
    vec2fTemplateClass,
    vec2fTemplate,
    new ArrayVec2f[NUInt](da(FactoryFloat1NUInt))
  )

  implicit final val FactoryVec2fHalfFloat = new G(
    vec2fTemplateClass,
    vec2fTemplate,
    new ArrayVec2f[HalfFloat](da(FactoryFloat1HalfFloat))
  )
  implicit final val FactoryVec2fRawFloat = new F(new ArrayVec2fRawFloat)


  // Vec3f
  private val vec3fTemplateClass =
    "simplex3d.buffer.floatm.optimized.ArrayVec3fRawFloat"
  private val vec3fTemplate = "RawFloat"

  implicit final val FactoryVec3fSByte = new G(
    vec3fTemplateClass,
    vec3fTemplate,
    new ArrayVec3f[SByte](da(FactoryFloat1SByte))
  )
  implicit final val FactoryVec3fUByte = new G(
    vec3fTemplateClass,
    vec3fTemplate,
    new ArrayVec3f[UByte](da(FactoryFloat1UByte))
  )
  implicit final val FactoryVec3fNSByte = new G(
    vec3fTemplateClass,
    vec3fTemplate,
    new ArrayVec3f[NSByte](da(FactoryFloat1NSByte))
  )
  implicit final val FactoryVec3fNUByte = new G(
    vec3fTemplateClass,
    vec3fTemplate,
    new ArrayVec3f[NUByte](da(FactoryFloat1NUByte))
  )

  implicit final val FactoryVec3fSShort = new G(
    vec3fTemplateClass,
    vec3fTemplate,
    new ArrayVec3f[SShort](da(FactoryFloat1SShort))
  )
  implicit final val FactoryVec3fUShort = new G(
    vec3fTemplateClass,
    vec3fTemplate,
    new ArrayVec3f[UShort](da(FactoryFloat1UShort))
  )
  implicit final val FactoryVec3fNSShort = new G(
    vec3fTemplateClass,
    vec3fTemplate,
    new ArrayVec3f[NSShort](da(FactoryFloat1NSShort))
  )
  implicit final val FactoryVec3fNUShort = new G(
    vec3fTemplateClass,
    vec3fTemplate,
    new ArrayVec3f[NUShort](da(FactoryFloat1NUShort))
  )

  implicit final val FactoryVec3fSInt = new G(
    vec3fTemplateClass,
    vec3fTemplate,
    new ArrayVec3f[SInt](da(FactoryFloat1SInt))
  )
  implicit final val FactoryVec3fUInt = new G(
    vec3fTemplateClass,
    vec3fTemplate,
    new ArrayVec3f[UInt](da(FactoryFloat1UInt))
  )
  implicit final val FactoryVec3fNSInt = new G(
    vec3fTemplateClass,
    vec3fTemplate,
    new ArrayVec3f[NSInt](da(FactoryFloat1NSInt))
  )
  implicit final val FactoryVec3fNUInt = new G(
    vec3fTemplateClass,
    vec3fTemplate,
    new ArrayVec3f[NUInt](da(FactoryFloat1NUInt))
  )

  implicit final val FactoryVec3fHalfFloat = new G(
    vec3fTemplateClass,
    vec3fTemplate,
    new ArrayVec3f[HalfFloat](da(FactoryFloat1HalfFloat))
  )
  implicit final val FactoryVec3fRawFloat = new F(new ArrayVec3fRawFloat)


  // Vec4f
  private val vec4fTemplateClass =
    "simplex3d.buffer.floatm.optimized.ArrayVec4fNUByte"
  private val vec4fTemplate = "NUByte"

  implicit final val FactoryVec4fSByte = new G(
    vec4fTemplateClass,
    vec4fTemplate,
    new ArrayVec4f[SByte](da(FactoryFloat1SByte))
  )
  implicit final val FactoryVec4fUByte = new G(
    vec4fTemplateClass,
    vec4fTemplate,
    new ArrayVec4f[UByte](da(FactoryFloat1UByte))
  )
  implicit final val FactoryVec4fNSByte = new G(
    vec4fTemplateClass,
    vec4fTemplate,
    new ArrayVec4f[NSByte](da(FactoryFloat1NSByte))
  )
  implicit final val FactoryVec4fNUByte = new F(new ArrayVec4fNUByte)

  implicit final val FactoryVec4fSShort = new G(
    vec4fTemplateClass,
    vec4fTemplate,
    new ArrayVec4f[SShort](da(FactoryFloat1SShort))
  )
  implicit final val FactoryVec4fUShort = new G(
    vec4fTemplateClass,
    vec4fTemplate,
    new ArrayVec4f[UShort](da(FactoryFloat1UShort))
  )
  implicit final val FactoryVec4fNSShort = new G(
    vec4fTemplateClass,
    vec4fTemplate,
    new ArrayVec4f[NSShort](da(FactoryFloat1NSShort))
  )
  implicit final val FactoryVec4fNUShort = new G(
    vec4fTemplateClass,
    vec4fTemplate,
    new ArrayVec4f[NUShort](da(FactoryFloat1NUShort))
  )

  implicit final val FactoryVec4fSInt = new G(
    vec4fTemplateClass,
    vec4fTemplate,
    new ArrayVec4f[SInt](da(FactoryFloat1SInt))
  )
  implicit final val FactoryVec4fUInt = new G(
    vec4fTemplateClass,
    vec4fTemplate,
    new ArrayVec4f[UInt](da(FactoryFloat1UInt))
  )
  implicit final val FactoryVec4fNSInt = new G(
    vec4fTemplateClass,
    vec4fTemplate,
    new ArrayVec4f[NSInt](da(FactoryFloat1NSInt))
  )
  implicit final val FactoryVec4fNUInt = new G(
    vec4fTemplateClass,
    vec4fTemplate,
    new ArrayVec4f[NUInt](da(FactoryFloat1NUInt))
  )

  implicit final val FactoryVec4fHalfFloat = new G(
    vec4fTemplateClass,
    vec4fTemplate,
    new ArrayVec4f[HalfFloat](da(FactoryFloat1HalfFloat))
  )
  implicit final val FactoryVec4fRawFloat = new G(
    vec4fTemplateClass,
    vec4fTemplate,
    new ArrayVec4f[RawFloat](da(FactoryFloat1RawFloat))
  )
}
