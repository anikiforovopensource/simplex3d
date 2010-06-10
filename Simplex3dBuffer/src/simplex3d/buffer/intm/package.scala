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
import simplex3d.math._
import simplex3d.buffer.optimize._
import simplex3d.buffer.intm._
import simplex3d.buffer.intm.optimized._


/**
 * @author Aleksey Nikiforov (lex)
 */
package object intm extends UnsignedImplicits {

  private final type F[T <: ElemType, D <: RawType] = SimpleFactoryRef[T, D]
  private final type G[T <: ElemType, D <: RawType] =TemplateGenFactoryRef[T, D]
  private final def da[T <: ElemType, D <: RawType](f: FactoryRef[T, D]) =
    f.factory.asInstanceOf[DataArray[T, D]]


  // Vec2i
  private val vec2iTemplateClass =
    "simplex3d.buffer.intm.optimized.ArrayVec2iSInt"
  private val vec2iTemplate = "SInt"

  implicit final val FactoryVec2iSByte = new G(
    vec2iTemplateClass,
    vec2iTemplate,
    new ArrayVec2i[SByte](da(FactoryInt1SByte))
  )
  implicit final val FactoryVec2iUByte = new G(
    vec2iTemplateClass,
    vec2iTemplate,
    new ArrayVec2i[UByte](da(FactoryInt1UByte))
  )

  implicit final val FactoryVec2iSShort = new G(
    vec2iTemplateClass,
    vec2iTemplate,
    new ArrayVec2i[SShort](da(FactoryInt1SShort))
  )
  implicit final val FactoryVec2iUShort = new G(
    vec2iTemplateClass,
    vec2iTemplate,
    new ArrayVec2i[UShort](da(FactoryInt1UShort))
  )

  implicit final val FactoryVec2iSInt = new F(
    new ArrayVec2i[SInt](da(FactoryInt1SInt))
  )
  implicit final val FactoryVec2iUInt = new G(
    vec2iTemplateClass,
    vec2iTemplate,
    new ArrayVec2i[UInt](da(FactoryInt1UInt))
  )


  // Vec3i
  private val vec3iTemplateClass =
    "simplex3d.buffer.intm.optimized.ArrayVec3iSInt"
  private val vec3iTemplate = "SInt"

  implicit final val FactoryVec3iSByte = new G(
    vec3iTemplateClass,
    vec3iTemplate,
    new ArrayVec3i[SByte](da(FactoryInt1SByte))
  )
  implicit final val FactoryVec3iUByte = new G(
    vec3iTemplateClass,
    vec3iTemplate,
    new ArrayVec3i[UByte](da(FactoryInt1UByte))
  )

  implicit final val FactoryVec3iSShort = new G(
    vec3iTemplateClass,
    vec3iTemplate,
    new ArrayVec3i[SShort](da(FactoryInt1SShort))
  )
  implicit final val FactoryVec3iUShort = new G(
    vec3iTemplateClass,
    vec3iTemplate,
    new ArrayVec3i[UShort](da(FactoryInt1UShort))
  )

  implicit final val FactoryVec3iSInt = new F(
    new ArrayVec3i[SInt](da(FactoryInt1SInt))
  )
  implicit final val FactoryVec3iUInt = new G(
    vec3iTemplateClass,
    vec3iTemplate,
    new ArrayVec3i[UInt](da(FactoryInt1UInt))
  )


  // Vec4i
  private val vec4iTemplateClass =
    "simplex3d.buffer.intm.optimized.ArrayVec4iSInt"
  private val vec4iTemplate = "SInt"

  implicit final val FactoryVec4iSByte = new G(
    vec4iTemplateClass,
    vec4iTemplate,
    new ArrayVec4i[SByte](da(FactoryInt1SByte))
  )
  implicit final val FactoryVec4iUByte = new G(
    vec4iTemplateClass,
    vec4iTemplate,
    new ArrayVec4i[UByte](da(FactoryInt1UByte))
  )

  implicit final val FactoryVec4iSShort = new G(
    vec4iTemplateClass,
    vec4iTemplate,
    new ArrayVec4i[SShort](da(FactoryInt1SShort))
  )
  implicit final val FactoryVec4iUShort = new G(
    vec4iTemplateClass,
    vec4iTemplate,
    new ArrayVec4i[UShort](da(FactoryInt1UShort))
  )

  implicit final val FactoryVec4iSInt = new F(
    new ArrayVec4i[SInt](da(FactoryInt1SInt))
  )
  implicit final val FactoryVec4iUInt = new G(
    vec4iTemplateClass,
    vec4iTemplate,
    new ArrayVec4i[UInt](da(FactoryInt1UInt))
  )
}
