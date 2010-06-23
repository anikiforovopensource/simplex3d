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

  private final type PrimitiveFactory[R <: ReadableFloat] =
    PrimitiveFactoryRef[Float1, R]
  private final type GenFactory[E <: Composite, R <: ReadableFloat] =
    TemplateGenFactoryRef[E, R]
  private final type CompositeFactory[E <: Composite, R <: ReadableFloat] =
    CompositeFactoryRef[E, R]


  // Float1
  implicit final val FactoryFloat1SByte = new PrimitiveFactory[SByte](
    "simplex3d.buffer.floatm.ArrayFloat1SByte"
  )
  implicit final val FactoryFloat1UByte = new PrimitiveFactory[UByte](
    "simplex3d.buffer.floatm.ArrayFloat1UByte"
  )

  implicit final val FactoryFloat1SShort = new PrimitiveFactory[SShort](
    "simplex3d.buffer.floatm.ArrayFloat1SShort"
  )
  implicit final val FactoryFloat1UShort = new PrimitiveFactory[UShort](
    "simplex3d.buffer.floatm.ArrayFloat1UShort"
  )

  implicit final val FactoryFloat1SInt = new PrimitiveFactory[SInt](
    "simplex3d.buffer.floatm.ArrayFloat1SInt"
  )
  implicit final val FactoryFloat1UInt = new PrimitiveFactory[UInt](
    "simplex3d.buffer.floatm.ArrayFloat1UInt"
  )

  implicit final val FactoryFloat1HalfFloat = new PrimitiveFactory[HalfFloat](
    "simplex3d.buffer.floatm.ArrayFloat1HalfFloat"
  )
  implicit final val FactoryFloat1RawFloat = new PrimitiveFactory[RawFloat](
    "simplex3d.buffer.floatm.ArrayFloat1RawFloat"
  )


  // Vec2f
  private val vec2fTemplateClass =
    "simplex3d.buffer.floatm.optimized.ArrayVec2fRawFloat"
  private val vec2fTemplateString = "RawFloat"
  private val vec2fFallbackClass = "simplex3d.buffer.floatm.ArrayVec2f"

  implicit final val FactoryVec2fSByte = new GenFactory[Vec2f, SByte](
    vec2fTemplateClass,
    vec2fTemplateString,
    new CompositeFactory[Vec2f, SByte](
      vec2fFallbackClass,
      FactoryFloat1SByte
    )
  )
  implicit final val FactoryVec2fUByte = new GenFactory[Vec2f, UByte](
    vec2fTemplateClass,
    vec2fTemplateString,
    new CompositeFactory[Vec2f, UByte](
      vec2fFallbackClass,
      FactoryFloat1UByte
    )
  )

  implicit final val FactoryVec2fSShort = new GenFactory[Vec2f, SShort](
    vec2fTemplateClass,
    vec2fTemplateString,
    new CompositeFactory[Vec2f, SShort](
      vec2fFallbackClass,
      FactoryFloat1SShort
    )
  )
  implicit final val FactoryVec2fUShort = new GenFactory[Vec2f, UShort](
    vec2fTemplateClass,
    vec2fTemplateString,
    new CompositeFactory[Vec2f, UShort](
      vec2fFallbackClass,
      FactoryFloat1UShort
    )
  )

  implicit final val FactoryVec2fSInt = new GenFactory[Vec2f, SInt](
    vec2fTemplateClass,
    vec2fTemplateString,
    new CompositeFactory[Vec2f, SInt](
      vec2fFallbackClass,
      FactoryFloat1SInt
    )
  )
  implicit final val FactoryVec2fUInt = new GenFactory[Vec2f, UInt](
    vec2fTemplateClass,
    vec2fTemplateString,
    new CompositeFactory[Vec2f, UInt](
      vec2fFallbackClass,
      FactoryFloat1UInt
    )
  )

  implicit final val FactoryVec2fHalfFloat = new GenFactory[Vec2f, HalfFloat](
    vec2fTemplateClass,
    vec2fTemplateString,
    new CompositeFactory[Vec2f, HalfFloat](
      vec2fFallbackClass,
      FactoryFloat1HalfFloat
    )
  )

  implicit final val FactoryVec2fRawFloat =
  new CompositeFactory[Vec2f, RawFloat](
    vec2fTemplateClass,
    FactoryFloat1RawFloat
  )


  // Vec3f
  private val vec3fTemplateClass =
    "simplex3d.buffer.floatm.optimized.ArrayVec3fRawFloat"
  private val vec3fTemplateString = "RawFloat"
  private val vec3fFallbackClass = "simplex3d.buffer.floatm.ArrayVec3f"

  implicit final val FactoryVec3fSByte = new GenFactory[Vec3f, SByte](
    vec3fTemplateClass,
    vec3fTemplateString,
    new CompositeFactory[Vec3f, SByte](
      vec3fFallbackClass,
      FactoryFloat1SByte
    )
  )
  implicit final val FactoryVec3fUByte = new GenFactory[Vec3f, UByte](
    vec3fTemplateClass,
    vec3fTemplateString,
    new CompositeFactory[Vec3f, UByte](
      vec3fFallbackClass,
      FactoryFloat1UByte
    )
  )

  implicit final val FactoryVec3fSShort = new GenFactory[Vec3f, SShort](
    vec3fTemplateClass,
    vec3fTemplateString,
    new CompositeFactory[Vec3f, SShort](
      vec3fFallbackClass,
      FactoryFloat1SShort
    )
  )
  implicit final val FactoryVec3fUShort = new GenFactory[Vec3f, UShort](
    vec3fTemplateClass,
    vec3fTemplateString,
    new CompositeFactory[Vec3f, UShort](
      vec3fFallbackClass,
      FactoryFloat1UShort
    )
  )

  implicit final val FactoryVec3fSInt = new GenFactory[Vec3f, SInt](
    vec3fTemplateClass,
    vec3fTemplateString,
    new CompositeFactory[Vec3f, SInt](
      vec3fFallbackClass,
      FactoryFloat1SInt
    )
  )
  implicit final val FactoryVec3fUInt = new GenFactory[Vec3f, UInt](
    vec3fTemplateClass,
    vec3fTemplateString,
    new CompositeFactory[Vec3f, UInt](
      vec3fFallbackClass,
      FactoryFloat1UInt
    )
  )

  implicit final val FactoryVec3fHalfFloat = new GenFactory[Vec3f, HalfFloat](
    vec3fTemplateClass,
    vec3fTemplateString,
    new CompositeFactory[Vec3f, HalfFloat](
      vec3fFallbackClass,
      FactoryFloat1HalfFloat
    )
  )

  implicit final val FactoryVec3fRawFloat =
  new CompositeFactory[Vec3f, RawFloat](
    vec3fTemplateClass,
    FactoryFloat1RawFloat
  )


  // Vec4f
  private val vec4fTemplateClass =
    "simplex3d.buffer.floatm.optimized.ArrayVec4fUByte"
  private val vec4fTemplateString = "UByte"
  private val vec4fFallbackClass = "simplex3d.buffer.floatm.ArrayVec4f"

  implicit final val FactoryVec4fSByte = new GenFactory[Vec4f, SByte](
    vec4fTemplateClass,
    vec4fTemplateString,
    new CompositeFactory[Vec4f, SByte](
      vec4fFallbackClass,
      FactoryFloat1SByte
    )
  )
  implicit final val FactoryVec4fUByte = new CompositeFactory[Vec4f, UByte](
    vec4fTemplateClass,
    FactoryFloat1UByte
  )

  implicit final val FactoryVec4fSShort = new GenFactory[Vec4f, SShort](
    vec4fTemplateClass,
    vec4fTemplateString,
    new CompositeFactory[Vec4f, SShort](
      vec4fFallbackClass,
      FactoryFloat1SShort
    )
  )
  implicit final val FactoryVec4fUShort = new GenFactory[Vec4f, UShort](
    vec4fTemplateClass,
    vec4fTemplateString,
    new CompositeFactory[Vec4f, UShort](
      vec4fFallbackClass,
      FactoryFloat1UShort
    )
  )

  implicit final val FactoryVec4fSInt = new GenFactory[Vec4f, SInt](
    vec4fTemplateClass,
    vec4fTemplateString,
    new CompositeFactory[Vec4f, SInt](
      vec4fFallbackClass,
      FactoryFloat1SInt
    )
  )
  implicit final val FactoryVec4fUInt = new GenFactory[Vec4f, UInt](
    vec4fTemplateClass,
    vec4fTemplateString,
    new CompositeFactory[Vec4f, UInt](
      vec4fFallbackClass,
      FactoryFloat1UInt
    )
  )

  implicit final val FactoryVec4fHalfFloat = new GenFactory[Vec4f, HalfFloat](
    vec4fTemplateClass,
    vec4fTemplateString,
    new CompositeFactory[Vec4f, HalfFloat](
      vec4fFallbackClass,
      FactoryFloat1HalfFloat
    )
  )

  implicit final val FactoryVec4fRawFloat = new GenFactory[Vec4f, RawFloat](
    vec4fTemplateClass,
    vec4fTemplateString,
    new CompositeFactory[Vec4f, RawFloat](
      vec4fFallbackClass,
      FactoryFloat1RawFloat
    )
  )
}
