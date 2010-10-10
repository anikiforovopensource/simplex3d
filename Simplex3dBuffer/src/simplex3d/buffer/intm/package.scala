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
package object intm extends PrimitiveIntImplicits {

  private final def simpleFactory[E <: Composite, R <: DefinedInt](s: DataSeq[E, R]) :FactoryRef[E, R] = {
    new SimpleFactoryRef[E, R](s)
  }
  private final def genFactory[E <: Composite, R <: DefinedInt](
    template: String, replace: String, fallback: DataSeq[E, R]
  ) :FactoryRef[E, R] = {
    new TemplateGenFactoryRef[E, R](template, replace, fallback)
  }
  
  private final def dataArray[R <: DefinedInt](ref: FactoryRef[Int1, R]) = {
    ref.factory.asInstanceOf[DataArray[Int1, R]]
  }


  // Vec2i
  private val vec2iTemplateClass = "simplex3d.buffer.intm.optimized.ArrayVec2iSInt"
  private val vec2iTemplateString = "SInt"

  implicit final lazy val FactoryVec2iSByte = genFactory[Vec2i, SByte](
    vec2iTemplateClass,
    vec2iTemplateString,
    new ArrayVec2i(dataArray(FactoryInt1SByte))
  )
  implicit final lazy val FactoryVec2iUByte = genFactory[Vec2i, UByte](
    vec2iTemplateClass,
    vec2iTemplateString,
    new ArrayVec2i(dataArray(FactoryInt1UByte))
  )

  implicit final lazy val FactoryVec2iSShort = genFactory[Vec2i, SShort](
    vec2iTemplateClass,
    vec2iTemplateString,
    new ArrayVec2i(dataArray(FactoryInt1SShort))
  )
  implicit final lazy val FactoryVec2iUShort = genFactory[Vec2i, UShort](
    vec2iTemplateClass,
    vec2iTemplateString,
    new ArrayVec2i(dataArray(FactoryInt1UShort))
  )

  implicit final lazy val FactoryVec2iSInt = simpleFactory[Vec2i, SInt](new ArrayVec2iSInt)
  
  implicit final lazy val FactoryVec2iUInt = genFactory[Vec2i, UInt](
    vec2iTemplateClass,
    vec2iTemplateString,
    new ArrayVec2i(dataArray(FactoryInt1UInt))
  )


  // Vec3i
  private val vec3iTemplateClass = "simplex3d.buffer.intm.optimized.ArrayVec3iSInt"
  private val vec3iTemplateString = "SInt"

  implicit final lazy val FactoryVec3iSByte = genFactory[Vec3i, SByte](
    vec3iTemplateClass,
    vec3iTemplateString,
    new ArrayVec3i(dataArray(FactoryInt1SByte))
  )
  implicit final lazy val FactoryVec3iUByte = genFactory[Vec3i, UByte](
    vec3iTemplateClass,
    vec3iTemplateString,
    new ArrayVec3i(dataArray(FactoryInt1UByte))
  )

  implicit final lazy val FactoryVec3iSShort = genFactory[Vec3i, SShort](
    vec3iTemplateClass,
    vec3iTemplateString,
    new ArrayVec3i(dataArray(FactoryInt1SShort))
  )
  implicit final lazy val FactoryVec3iUShort = genFactory[Vec3i, UShort](
    vec3iTemplateClass,
    vec3iTemplateString,
    new ArrayVec3i(dataArray(FactoryInt1UShort))
  )

  implicit final lazy val FactoryVec3iSInt = simpleFactory[Vec3i, SInt](new ArrayVec3iSInt)
  
  implicit final lazy val FactoryVec3iUInt = genFactory[Vec3i, UInt](
    vec3iTemplateClass,
    vec3iTemplateString,
    new ArrayVec3i(dataArray(FactoryInt1UInt))
  )


  // Vec4i
  private val vec4iTemplateClass = "simplex3d.buffer.intm.optimized.ArrayVec4iSInt"
  private val vec4iTemplateString = "SInt"

  implicit final lazy val FactoryVec4iSByte = genFactory[Vec4i, SByte](
    vec4iTemplateClass,
    vec4iTemplateString,
    new ArrayVec4i(dataArray(FactoryInt1SByte))
  )
  implicit final lazy val FactoryVec4iUByte = genFactory[Vec4i, UByte](
    vec4iTemplateClass,
    vec4iTemplateString,
    new ArrayVec4i(dataArray(FactoryInt1UByte))
  )

  implicit final lazy val FactoryVec4iSShort = genFactory[Vec4i, SShort](
    vec4iTemplateClass,
    vec4iTemplateString,
    new ArrayVec4i(dataArray(FactoryInt1SShort))
  )
  implicit final lazy val FactoryVec4iUShort = genFactory[Vec4i, UShort](
    vec4iTemplateClass,
    vec4iTemplateString,
    new ArrayVec4i(dataArray(FactoryInt1UShort))
  )

  implicit final lazy val FactoryVec4iSInt = simpleFactory[Vec4i, SInt](new ArrayVec4iSInt)
  
  implicit final lazy val FactoryVec4iUInt = genFactory[Vec4i, UInt](
    vec4iTemplateClass,
    vec4iTemplateString,
    new ArrayVec4i(dataArray(FactoryInt1UInt))
  )
}
