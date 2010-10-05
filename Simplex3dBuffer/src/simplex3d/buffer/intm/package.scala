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

  private final type SimpleFactory[E <: Composite, R <: ReadableInt] = SimpleFactoryRef[E, R]
  private final type GenFactory[E <: Composite, R <: ReadableInt] = TemplateGenFactoryRef[E, R]
  
  private final def dataArray[R <: ReadableInt](f: SimpleFactoryRef[Int1, R]) =
    f.factory.asInstanceOf[DataArray[Int1, R]]


  // Vec2i
  private val vec2iTemplateClass = "simplex3d.buffer.intm.optimized.ArrayVec2iSInt"
  private val vec2iTemplateString = "SInt"

  implicit final lazy val FactoryVec2iSByte = new GenFactory[Vec2i, SByte](
    vec2iTemplateClass,
    vec2iTemplateString,
    new ArrayVec2i(dataArray(FactoryInt1SByte))
  )
  implicit final lazy val FactoryVec2iUByte = new GenFactory[Vec2i, UByte](
    vec2iTemplateClass,
    vec2iTemplateString,
    new ArrayVec2i(dataArray(FactoryInt1UByte))
  )

  implicit final lazy val FactoryVec2iSShort = new GenFactory[Vec2i, SShort](
    vec2iTemplateClass,
    vec2iTemplateString,
    new ArrayVec2i(dataArray(FactoryInt1SShort))
  )
  implicit final lazy val FactoryVec2iUShort = new GenFactory[Vec2i, UShort](
    vec2iTemplateClass,
    vec2iTemplateString,
    new ArrayVec2i(dataArray(FactoryInt1UShort))
  )

  implicit final lazy val FactoryVec2iSInt = new SimpleFactory[Vec2i, SInt](new ArrayVec2iSInt)
  
  implicit final lazy val FactoryVec2iUInt = new GenFactory[Vec2i, UInt](
    vec2iTemplateClass,
    vec2iTemplateString,
    new ArrayVec2i(dataArray(FactoryInt1UInt))
  )


  // Vec3i
  private val vec3iTemplateClass = "simplex3d.buffer.intm.optimized.ArrayVec3iSInt"
  private val vec3iTemplateString = "SInt"

  implicit final lazy val FactoryVec3iSByte = new GenFactory[Vec3i, SByte](
    vec3iTemplateClass,
    vec3iTemplateString,
    new ArrayVec3i(dataArray(FactoryInt1SByte))
  )
  implicit final lazy val FactoryVec3iUByte = new GenFactory[Vec3i, UByte](
    vec3iTemplateClass,
    vec3iTemplateString,
    new ArrayVec3i(dataArray(FactoryInt1UByte))
  )

  implicit final lazy val FactoryVec3iSShort = new GenFactory[Vec3i, SShort](
    vec3iTemplateClass,
    vec3iTemplateString,
    new ArrayVec3i(dataArray(FactoryInt1SShort))
  )
  implicit final lazy val FactoryVec3iUShort = new GenFactory[Vec3i, UShort](
    vec3iTemplateClass,
    vec3iTemplateString,
    new ArrayVec3i(dataArray(FactoryInt1UShort))
  )

  implicit final lazy val FactoryVec3iSInt = new SimpleFactory[Vec3i, SInt](new ArrayVec3iSInt)
  
  implicit final lazy val FactoryVec3iUInt = new GenFactory[Vec3i, UInt](
    vec3iTemplateClass,
    vec3iTemplateString,
    new ArrayVec3i(dataArray(FactoryInt1UInt))
  )


  // Vec4i
  private val vec4iTemplateClass = "simplex3d.buffer.intm.optimized.ArrayVec4iSInt"
  private val vec4iTemplateString = "SInt"

  implicit final lazy val FactoryVec4iSByte = new GenFactory[Vec4i, SByte](
    vec4iTemplateClass,
    vec4iTemplateString,
    new ArrayVec4i(dataArray(FactoryInt1SByte))
  )
  implicit final lazy val FactoryVec4iUByte = new GenFactory[Vec4i, UByte](
    vec4iTemplateClass,
    vec4iTemplateString,
    new ArrayVec4i(dataArray(FactoryInt1UByte))
  )

  implicit final lazy val FactoryVec4iSShort = new GenFactory[Vec4i, SShort](
    vec4iTemplateClass,
    vec4iTemplateString,
    new ArrayVec4i(dataArray(FactoryInt1SShort))
  )
  implicit final lazy val FactoryVec4iUShort = new GenFactory[Vec4i, UShort](
    vec4iTemplateClass,
    vec4iTemplateString,
    new ArrayVec4i(dataArray(FactoryInt1UShort))
  )

  implicit final lazy val FactoryVec4iSInt = new SimpleFactory[Vec4i, SInt](new ArrayVec4iSInt)
  
  implicit final lazy val FactoryVec4iUInt = new GenFactory[Vec4i, UInt](
    vec4iTemplateClass,
    vec4iTemplateString,
    new ArrayVec4i(dataArray(FactoryInt1UInt))
  )
}
