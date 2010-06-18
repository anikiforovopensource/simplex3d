/*
 * Simplex3d, IntBuffer module
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
import simplex3d.math.intm._
import simplex3d.buffer.optimize._
import simplex3d.buffer.intm._
import simplex3d.buffer.intm.optimized._


/**
 * @author Aleksey Nikiforov (lex)
 */
package object intm extends UnsignedImplicits {

  private final type GenFactory[E <: Composite, R <: ReadableFloat] =
    TemplateGenFactoryRef[E, R]
  private final type CompositeFactory[E <: Composite, R <: ReadableFloat] =
    CompositeFactoryRef[E, R]


  // Vec2i
  private val vec2iTemplateClass =
    "simplex3d.buffer.intm.optimized.ArrayVec2iSInt"
  private val vec2iTemplateString = "SInt"
  private val vec2iFallbackClass = "simplex3d.buffer.intm.ArrayVec2i"

  implicit final val FactoryVec2iSByte = new GenFactory[Vec2i, SByte](
    vec2iTemplateClass,
    vec2iTemplateString,
    new CompositeFactory[Vec2i, SByte](
      vec2iFallbackClass,
      FactoryInt1SByte
    )
  )
  implicit final val FactoryVec2iUByte = new GenFactory[Vec2i, UByte](
    vec2iTemplateClass,
    vec2iTemplateString,
    new CompositeFactory[Vec2i, UByte](
      vec2iFallbackClass,
      FactoryInt1UByte
    )
  )

  implicit final val FactoryVec2iSShort = new GenFactory[Vec2i, SShort](
    vec2iTemplateClass,
    vec2iTemplateString,
    new CompositeFactory[Vec2i, SShort](
      vec2iFallbackClass,
      FactoryInt1SShort
    )
  )
  implicit final val FactoryVec2iUShort = new GenFactory[Vec2i, UShort](
    vec2iTemplateClass,
    vec2iTemplateString,
    new CompositeFactory[Vec2i, UShort](
      vec2iFallbackClass,
      FactoryInt1UShort
    )
  )

  implicit final val FactoryVec2iSInt = new CompositeFactory[Vec2i, SInt](
    vec2iTemplateClass,
    FactoryInt1SInt
  )
  implicit final val FactoryVec2iUInt = new GenFactory[Vec2i, UInt](
    vec2iTemplateClass,
    vec2iTemplateString,
    new CompositeFactory[Vec2i, UInt](
      vec2iFallbackClass,
      FactoryInt1UInt
    )
  )


  // Vec3i
  private val vec3iTemplateClass =
    "simplex3d.buffer.intm.optimized.ArrayVec3iSInt"
  private val vec3iTemplateString = "SInt"
  private val vec3iFallbackClass = "simplex3d.buffer.intm.ArrayVec3i"

  implicit final val FactoryVec3iSByte = new GenFactory[Vec3i, SByte](
    vec3iTemplateClass,
    vec3iTemplateString,
    new CompositeFactory[Vec3i, SByte](
      vec3iFallbackClass,
      FactoryInt1SByte
    )
  )
  implicit final val FactoryVec3iUByte = new GenFactory[Vec3i, UByte](
    vec3iTemplateClass,
    vec3iTemplateString,
    new CompositeFactory[Vec3i, UByte](
      vec3iFallbackClass,
      FactoryInt1UByte
    )
  )

  implicit final val FactoryVec3iSShort = new GenFactory[Vec3i, SShort](
    vec3iTemplateClass,
    vec3iTemplateString,
    new CompositeFactory[Vec3i, SShort](
      vec3iFallbackClass,
      FactoryInt1SShort
    )
  )
  implicit final val FactoryVec3iUShort = new GenFactory[Vec3i, UShort](
    vec3iTemplateClass,
    vec3iTemplateString,
    new CompositeFactory[Vec3i, UShort](
      vec3iFallbackClass,
      FactoryInt1UShort
    )
  )

  implicit final val FactoryVec3iSInt = new CompositeFactory[Vec3i, SInt](
    vec3iTemplateClass,
    FactoryInt1SInt
  )
  implicit final val FactoryVec3iUInt = new GenFactory[Vec3i, UInt](
    vec3iTemplateClass,
    vec3iTemplateString,
    new CompositeFactory[Vec3i, UInt](
      vec3iFallbackClass,
      FactoryInt1UInt
    )
  )


  // Vec4i
  private val vec4iTemplateClass =
    "simplex3d.buffer.intm.optimized.ArrayVec4iSInt"
  private val vec4iTemplateString = "SInt"
  private val vec4iFallbackClass = "simplex3d.buffer.intm.ArrayVec4i"

  implicit final val FactoryVec4iSByte = new GenFactory[Vec4i, SByte](
    vec4iTemplateClass,
    vec4iTemplateString,
    new CompositeFactory[Vec4i, SByte](
      vec4iFallbackClass,
      FactoryInt1SByte
    )
  )
  implicit final val FactoryVec4iUByte = new GenFactory[Vec4i, UByte](
    vec4iTemplateClass,
    vec4iTemplateString,
    new CompositeFactory[Vec4i, UByte](
      vec4iFallbackClass,
      FactoryInt1UByte
    )
  )

  implicit final val FactoryVec4iSShort = new GenFactory[Vec4i, SShort](
    vec4iTemplateClass,
    vec4iTemplateString,
    new CompositeFactory[Vec4i, SShort](
      vec4iFallbackClass,
      FactoryInt1SShort
    )
  )
  implicit final val FactoryVec4iUShort = new GenFactory[Vec4i, UShort](
    vec4iTemplateClass,
    vec4iTemplateString,
    new CompositeFactory[Vec4i, UShort](
      vec4iFallbackClass,
      FactoryInt1UShort
    )
  )

  implicit final val FactoryVec4iSInt = new CompositeFactory[Vec4i, SInt](
    vec4iTemplateClass,
    FactoryInt1SInt
  )
  implicit final val FactoryVec4iUInt = new GenFactory[Vec4i, UInt](
    vec4iTemplateClass,
    vec4iTemplateString,
    new CompositeFactory[Vec4i, UInt](
      vec4iFallbackClass,
      FactoryInt1UInt
    )
  )
}
