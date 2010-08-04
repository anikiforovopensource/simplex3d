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

  private final type PrimitiveFactory[R <: ReadableDouble] =
    PrimitiveFactoryRef[Double1, R]
  private final type GenFactory[E <: Composite, R <: ReadableDouble] =
    TemplateGenFactoryRef[E, R]
  private final type CompositeFactory[E <: Composite, R <: ReadableDouble] =
    CompositeFactoryRef[E, R]


  // Double1
  implicit final val FactoryDouble1SByte = new PrimitiveFactory[SByte](
    "simplex3d.buffer.doublem.ArrayDouble1SByte"
  )
  implicit final val FactoryDouble1UByte = new PrimitiveFactory[UByte](
    "simplex3d.buffer.doublem.ArrayDouble1UByte"
  )

  implicit final val FactoryDouble1SShort = new PrimitiveFactory[SShort](
    "simplex3d.buffer.doublem.ArrayDouble1SShort"
  )
  implicit final val FactoryDouble1UShort = new PrimitiveFactory[UShort](
    "simplex3d.buffer.doublem.ArrayDouble1UShort"
  )

  implicit final val FactoryDouble1SInt = new PrimitiveFactory[SInt](
    "simplex3d.buffer.doublem.ArrayDouble1SInt"
  )
  implicit final val FactoryDouble1UInt = new PrimitiveFactory[UInt](
    "simplex3d.buffer.doublem.ArrayDouble1UInt"
  )

  implicit final val FactoryDouble1HalfFloat = new PrimitiveFactory[HalfFloat](
    "simplex3d.buffer.doublem.ArrayDouble1HalfFloat"
  )
  implicit final val FactoryDouble1RawFloat = new PrimitiveFactory[RawFloat](
    "simplex3d.buffer.doublem.ArrayDouble1RawFloat"
  )
  implicit final val FactoryDouble1RawDouble = new PrimitiveFactory[RawDouble](
    "simplex3d.buffer.doublem.ArrayDouble1RawDouble"
  )


  // Vec2d
  private val vec2fTemplateClass =
    "simplex3d.buffer.doublem.optimized.ArrayVec2dRawFloat"
  private val vec2fTemplateString = "RawFloat"
  private val vec2fFallbackClass = "simplex3d.buffer.doublem.ArrayVec2d"

  implicit final val FactoryVec2dSByte = new GenFactory[Vec2d, SByte](
    vec2fTemplateClass,
    vec2fTemplateString,
    new CompositeFactory[Vec2d, SByte](
      vec2fFallbackClass,
      FactoryDouble1SByte
    )
  )
  implicit final val FactoryVec2dUByte = new GenFactory[Vec2d, UByte](
    vec2fTemplateClass,
    vec2fTemplateString,
    new CompositeFactory[Vec2d, UByte](
      vec2fFallbackClass,
      FactoryDouble1UByte
    )
  )

  implicit final val FactoryVec2dSShort = new GenFactory[Vec2d, SShort](
    vec2fTemplateClass,
    vec2fTemplateString,
    new CompositeFactory[Vec2d, SShort](
      vec2fFallbackClass,
      FactoryDouble1SShort
    )
  )
  implicit final val FactoryVec2dUShort = new GenFactory[Vec2d, UShort](
    vec2fTemplateClass,
    vec2fTemplateString,
    new CompositeFactory[Vec2d, UShort](
      vec2fFallbackClass,
      FactoryDouble1UShort
    )
  )

  implicit final val FactoryVec2dSInt = new GenFactory[Vec2d, SInt](
    vec2fTemplateClass,
    vec2fTemplateString,
    new CompositeFactory[Vec2d, SInt](
      vec2fFallbackClass,
      FactoryDouble1SInt
    )
  )
  implicit final val FactoryVec2dUInt = new GenFactory[Vec2d, UInt](
    vec2fTemplateClass,
    vec2fTemplateString,
    new CompositeFactory[Vec2d, UInt](
      vec2fFallbackClass,
      FactoryDouble1UInt
    )
  )

  implicit final val FactoryVec2dHalfFloat = new GenFactory[Vec2d, HalfFloat](
    vec2fTemplateClass,
    vec2fTemplateString,
    new CompositeFactory[Vec2d, HalfFloat](
      vec2fFallbackClass,
      FactoryDouble1HalfFloat
    )
  )

  implicit final val FactoryVec2dRawFloat =
  new CompositeFactory[Vec2d, RawFloat](
    vec2fTemplateClass,
    FactoryDouble1RawFloat
  )

  implicit final val FactoryVec2dRawDouble = new GenFactory[Vec2d, RawDouble](
    vec2fTemplateClass,
    vec2fTemplateString,
    new CompositeFactory[Vec2d, RawDouble](
      vec2fFallbackClass,
      FactoryDouble1RawDouble
    )
  )


  // Vec3d
  private val vec3fTemplateClass =
    "simplex3d.buffer.doublem.optimized.ArrayVec3dRawFloat"
  private val vec3fTemplateString = "RawFloat"
  private val vec3fFallbackClass = "simplex3d.buffer.doublem.ArrayVec3d"

  implicit final val FactoryVec3dSByte = new GenFactory[Vec3d, SByte](
    vec3fTemplateClass,
    vec3fTemplateString,
    new CompositeFactory[Vec3d, SByte](
      vec3fFallbackClass,
      FactoryDouble1SByte
    )
  )
  implicit final val FactoryVec3dUByte = new GenFactory[Vec3d, UByte](
    vec3fTemplateClass,
    vec3fTemplateString,
    new CompositeFactory[Vec3d, UByte](
      vec3fFallbackClass,
      FactoryDouble1UByte
    )
  )

  implicit final val FactoryVec3dSShort = new GenFactory[Vec3d, SShort](
    vec3fTemplateClass,
    vec3fTemplateString,
    new CompositeFactory[Vec3d, SShort](
      vec3fFallbackClass,
      FactoryDouble1SShort
    )
  )
  implicit final val FactoryVec3dUShort = new GenFactory[Vec3d, UShort](
    vec3fTemplateClass,
    vec3fTemplateString,
    new CompositeFactory[Vec3d, UShort](
      vec3fFallbackClass,
      FactoryDouble1UShort
    )
  )

  implicit final val FactoryVec3dSInt = new GenFactory[Vec3d, SInt](
    vec3fTemplateClass,
    vec3fTemplateString,
    new CompositeFactory[Vec3d, SInt](
      vec3fFallbackClass,
      FactoryDouble1SInt
    )
  )
  implicit final val FactoryVec3dUInt = new GenFactory[Vec3d, UInt](
    vec3fTemplateClass,
    vec3fTemplateString,
    new CompositeFactory[Vec3d, UInt](
      vec3fFallbackClass,
      FactoryDouble1UInt
    )
  )

  implicit final val FactoryVec3dHalfFloat = new GenFactory[Vec3d, HalfFloat](
    vec3fTemplateClass,
    vec3fTemplateString,
    new CompositeFactory[Vec3d, HalfFloat](
      vec3fFallbackClass,
      FactoryDouble1HalfFloat
    )
  )

  implicit final val FactoryVec3dRawFloat =
  new CompositeFactory[Vec3d, RawFloat](
    vec3fTemplateClass,
    FactoryDouble1RawFloat
  )

  implicit final val FactoryVec3dRawDouble = new GenFactory[Vec3d, RawDouble](
    vec3fTemplateClass,
    vec3fTemplateString,
    new CompositeFactory[Vec3d, RawDouble](
      vec3fFallbackClass,
      FactoryDouble1RawDouble
    )
  )


  // Vec4d
  private val vec4fTemplateClass =
    "simplex3d.buffer.doublem.optimized.ArrayVec4dUByte"
  private val vec4fTemplateString = "UByte"
  private val vec4fFallbackClass = "simplex3d.buffer.doublem.ArrayVec4d"

  implicit final val FactoryVec4dSByte = new GenFactory[Vec4d, SByte](
    vec4fTemplateClass,
    vec4fTemplateString,
    new CompositeFactory[Vec4d, SByte](
      vec4fFallbackClass,
      FactoryDouble1SByte
    )
  )
  implicit final val FactoryVec4dUByte = new CompositeFactory[Vec4d, UByte](
    vec4fTemplateClass,
    FactoryDouble1UByte
  )

  implicit final val FactoryVec4dSShort = new GenFactory[Vec4d, SShort](
    vec4fTemplateClass,
    vec4fTemplateString,
    new CompositeFactory[Vec4d, SShort](
      vec4fFallbackClass,
      FactoryDouble1SShort
    )
  )
  implicit final val FactoryVec4dUShort = new GenFactory[Vec4d, UShort](
    vec4fTemplateClass,
    vec4fTemplateString,
    new CompositeFactory[Vec4d, UShort](
      vec4fFallbackClass,
      FactoryDouble1UShort
    )
  )

  implicit final val FactoryVec4dSInt = new GenFactory[Vec4d, SInt](
    vec4fTemplateClass,
    vec4fTemplateString,
    new CompositeFactory[Vec4d, SInt](
      vec4fFallbackClass,
      FactoryDouble1SInt
    )
  )
  implicit final val FactoryVec4dUInt = new GenFactory[Vec4d, UInt](
    vec4fTemplateClass,
    vec4fTemplateString,
    new CompositeFactory[Vec4d, UInt](
      vec4fFallbackClass,
      FactoryDouble1UInt
    )
  )

  implicit final val FactoryVec4dHalfFloat = new GenFactory[Vec4d, HalfFloat](
    vec4fTemplateClass,
    vec4fTemplateString,
    new CompositeFactory[Vec4d, HalfFloat](
      vec4fFallbackClass,
      FactoryDouble1HalfFloat
    )
  )

  implicit final val FactoryVec4dRawFloat = new GenFactory[Vec4d, RawFloat](
    vec4fTemplateClass,
    vec4fTemplateString,
    new CompositeFactory[Vec4d, RawFloat](
      vec4fFallbackClass,
      FactoryDouble1RawFloat
    )
  )

  implicit final val FactoryVec4dRawDouble = new GenFactory[Vec4d, RawDouble](
    vec4fTemplateClass,
    vec4fTemplateString,
    new CompositeFactory[Vec4d, RawDouble](
      vec4fFallbackClass,
      FactoryDouble1RawDouble
    )
  )
}
