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
import simplex3d.buffer.optimize._
import simplex3d.buffer.floatm._
import simplex3d.buffer.floatm.optimized._


/**
 * @author Aleksey Nikiforov (lex)
 */
package object floatm {

  private final type F[T <: MetaType, D <: RawType] = SimpleFactoryRef[T, D]
  private final type G[T <: MetaType, D <: RawType] =TemplateGenFactoryRef[T, D]
  private final def da[T <: MetaType, D <: RawType](f: FactoryRef[T, D]) =
    f.factory.asInstanceOf[DataArray[T, D]]


  // Float1
  implicit final val fFloat1SByte = new F(new ArrayFloat1SByte)
  implicit final val fFloat1UByte = new F(new ArrayFloat1UByte)
  implicit final val fFloat1NSByte = new F(new ArrayFloat1NSByte)
  implicit final val fFloat1NUByte = new F(new ArrayFloat1NUByte)
  
  implicit final val fFloat1SShort = new F(new ArrayFloat1SShort)
  implicit final val fFloat1UShort = new F(new ArrayFloat1UShort)
  implicit final val fFloat1NSShort = new F(new ArrayFloat1NSShort)
  implicit final val fFloat1NUShort = new F(new ArrayFloat1NUShort)

  implicit final val fFloat1SInt = new F(new ArrayFloat1SInt)
  implicit final val fFloat1UInt = new F(new ArrayFloat1UInt)
  implicit final val fFloat1NSInt = new F(new ArrayFloat1NSInt)
  implicit final val fFloat1NUInt = new F(new ArrayFloat1NUInt)

  implicit final val fFloat1HalfFloat = new F(new ArrayFloat1HalfFloat)
  implicit final val fFloat1RawFloat = new F(new ArrayFloat1RawFloat)


  // Vec2f
  private val vec2fTemplateClass =
    "simplex3d.buffer.floatm.optimized.ArrayVec2fRawFloat"
  private val vec2fTemplate = "RawFloat"

  implicit final val fVec2fSByte = new G(
    vec2fTemplateClass,
    vec2fTemplate,
    new ArrayVec2f[SByte](da(fFloat1SByte))
  )
  implicit final val fVec2fUByte = new G(
    vec2fTemplateClass,
    vec2fTemplate,
    new ArrayVec2f[UByte](da(fFloat1UByte))
  )
  implicit final val fVec2fNSByte = new G(
    vec2fTemplateClass,
    vec2fTemplate,
    new ArrayVec2f[NSByte](da(fFloat1NSByte))
  )
  implicit final val fVec2fNUByte = new G(
    vec2fTemplateClass,
    vec2fTemplate,
    new ArrayVec2f[NUByte](da(fFloat1NUByte))
  )

  implicit final val fVec2fSShort = new G(
    vec2fTemplateClass,
    vec2fTemplate,
    new ArrayVec2f[SShort](da(fFloat1SShort))
  )
  implicit final val fVec2fUShort = new G(
    vec2fTemplateClass,
    vec2fTemplate,
    new ArrayVec2f[UShort](da(fFloat1UShort))
  )
  implicit final val fVec2fNSShort = new G(
    vec2fTemplateClass,
    vec2fTemplate,
    new ArrayVec2f[NSShort](da(fFloat1NSShort))
  )
  implicit final val fVec2fNUShort = new G(
    vec2fTemplateClass,
    vec2fTemplate,
    new ArrayVec2f[NUShort](da(fFloat1NUShort))
  )

  implicit final val fVec2fSInt = new G(
    vec2fTemplateClass,
    vec2fTemplate,
    new ArrayVec2f[SInt](da(fFloat1SInt))
  )
  implicit final val fVec2fUInt = new G(
    vec2fTemplateClass,
    vec2fTemplate,
    new ArrayVec2f[UInt](da(fFloat1UInt))
  )
  implicit final val fVec2fNSInt = new G(
    vec2fTemplateClass,
    vec2fTemplate,
    new ArrayVec2f[NSInt](da(fFloat1NSInt))
  )
  implicit final val fVec2fNUInt = new G(
    vec2fTemplateClass,
    vec2fTemplate,
    new ArrayVec2f[NUInt](da(fFloat1NUInt))
  )

  implicit final val fVec2fHalfFloat = new G(
    vec2fTemplateClass,
    vec2fTemplate,
    new ArrayVec2f[HalfFloat](da(fFloat1HalfFloat))
  )
  implicit final val fVec2fRawFloat = new F(new ArrayVec2fRawFloat)


  // Vec3f
  private val vec3fTemplateClass =
    "simplex3d.buffer.floatm.optimized.ArrayVec3fRawFloat"
  private val vec3fTemplate = "RawFloat"

  implicit final val fVec3fSByte = new G(
    vec3fTemplateClass,
    vec3fTemplate,
    new ArrayVec3f[SByte](da(fFloat1SByte))
  )
  implicit final val fVec3fUByte = new G(
    vec3fTemplateClass,
    vec3fTemplate,
    new ArrayVec3f[UByte](da(fFloat1UByte))
  )
  implicit final val fVec3fNSByte = new G(
    vec3fTemplateClass,
    vec3fTemplate,
    new ArrayVec3f[NSByte](da(fFloat1NSByte))
  )
  implicit final val fVec3fNUByte = new G(
    vec3fTemplateClass,
    vec3fTemplate,
    new ArrayVec3f[NUByte](da(fFloat1NUByte))
  )

  implicit final val fVec3fSShort = new G(
    vec3fTemplateClass,
    vec3fTemplate,
    new ArrayVec3f[SShort](da(fFloat1SShort))
  )
  implicit final val fVec3fUShort = new G(
    vec3fTemplateClass,
    vec3fTemplate,
    new ArrayVec3f[UShort](da(fFloat1UShort))
  )
  implicit final val fVec3fNSShort = new G(
    vec3fTemplateClass,
    vec3fTemplate,
    new ArrayVec3f[NSShort](da(fFloat1NSShort))
  )
  implicit final val fVec3fNUShort = new G(
    vec3fTemplateClass,
    vec3fTemplate,
    new ArrayVec3f[NUShort](da(fFloat1NUShort))
  )

  implicit final val fVec3fSInt = new G(
    vec3fTemplateClass,
    vec3fTemplate,
    new ArrayVec3f[SInt](da(fFloat1SInt))
  )
  implicit final val fVec3fUInt = new G(
    vec3fTemplateClass,
    vec3fTemplate,
    new ArrayVec3f[UInt](da(fFloat1UInt))
  )
  implicit final val fVec3fNSInt = new G(
    vec3fTemplateClass,
    vec3fTemplate,
    new ArrayVec3f[NSInt](da(fFloat1NSInt))
  )
  implicit final val fVec3fNUInt = new G(
    vec3fTemplateClass,
    vec3fTemplate,
    new ArrayVec3f[NUInt](da(fFloat1NUInt))
  )

  implicit final val fVec3fHalfFloat = new G(
    vec3fTemplateClass,
    vec3fTemplate,
    new ArrayVec3f[HalfFloat](da(fFloat1HalfFloat))
  )
  implicit final val fVec3fRawFloat = new F(new ArrayVec3fRawFloat)


  // Vec4f
  private val vec4fTemplateClass =
    "simplex3d.buffer.floatm.optimized.ArrayVec4fNUByte"
  private val vec4fTemplate = "NUByte"

  implicit final val fVec4fSByte = new G(
    vec4fTemplateClass,
    vec4fTemplate,
    new ArrayVec4f[SByte](da(fFloat1SByte))
  )
  implicit final val fVec4fUByte = new G(
    vec4fTemplateClass,
    vec4fTemplate,
    new ArrayVec4f[UByte](da(fFloat1UByte))
  )
  implicit final val fVec4fNSByte = new G(
    vec4fTemplateClass,
    vec4fTemplate,
    new ArrayVec4f[NSByte](da(fFloat1NSByte))
  )
  implicit final val fVec4fNUByte = new F(new ArrayVec4fNUByte)

  implicit final val fVec4fSShort = new G(
    vec4fTemplateClass,
    vec4fTemplate,
    new ArrayVec4f[SShort](da(fFloat1SShort))
  )
  implicit final val fVec4fUShort = new G(
    vec4fTemplateClass,
    vec4fTemplate,
    new ArrayVec4f[UShort](da(fFloat1UShort))
  )
  implicit final val fVec4fNSShort = new G(
    vec4fTemplateClass,
    vec4fTemplate,
    new ArrayVec4f[NSShort](da(fFloat1NSShort))
  )
  implicit final val fVec4fNUShort = new G(
    vec4fTemplateClass,
    vec4fTemplate,
    new ArrayVec4f[NUShort](da(fFloat1NUShort))
  )

  implicit final val fVec4fSInt = new G(
    vec4fTemplateClass,
    vec4fTemplate,
    new ArrayVec4f[SInt](da(fFloat1SInt))
  )
  implicit final val fVec4fUInt = new G(
    vec4fTemplateClass,
    vec4fTemplate,
    new ArrayVec4f[UInt](da(fFloat1UInt))
  )
  implicit final val fVec4fNSInt = new G(
    vec4fTemplateClass,
    vec4fTemplate,
    new ArrayVec4f[NSInt](da(fFloat1NSInt))
  )
  implicit final val fVec4fNUInt = new G(
    vec4fTemplateClass,
    vec4fTemplate,
    new ArrayVec4f[NUInt](da(fFloat1NUInt))
  )

  implicit final val fVec4fHalfFloat = new G(
    vec4fTemplateClass,
    vec4fTemplate,
    new ArrayVec4f[HalfFloat](da(fFloat1HalfFloat))
  )
  implicit final val fVec4fRawFloat = new G(
    vec4fTemplateClass,
    vec4fTemplate,
    new ArrayVec4f[RawFloat](da(fFloat1RawFloat))
  )
}
