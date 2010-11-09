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
import simplex3d.buffer._
import simplex3d.buffer.intm._
import simplex3d.buffer.intm.impl._


/**
 * @author Aleksey Nikiforov (lex)
 */
package object intm {

  private final def factory[E <: Composite, R <: DefinedInt](s: DataSeq[E, R]) :Factory[E, R] = s
  private final def cast[R <: DefinedInt](f: Factory[Int1, R]) = f.asInstanceOf[DataArray[Int1, R]]


  // Vec2i
  implicit final lazy val FactoryVec2iSByte = factory[Vec2i, SByte](new ArrayVec2i(cast(FactoryInt1SByte)))
  implicit final lazy val FactoryVec2iUByte = factory[Vec2i, UByte](new ArrayVec2i(cast(FactoryInt1UByte)))
  implicit final lazy val FactoryVec2iSShort = factory[Vec2i, SShort](new ArrayVec2i(cast(FactoryInt1SShort)))
  implicit final lazy val FactoryVec2iUShort = factory[Vec2i, UShort](new ArrayVec2i(cast(FactoryInt1UShort)))
  implicit final lazy val FactoryVec2iSInt = factory[Vec2i, SInt](new ArrayVec2iSInt())
  implicit final lazy val FactoryVec2iUInt = factory[Vec2i, UInt](new ArrayVec2i(cast(FactoryInt1UInt)))
  
  // Vec3i
  implicit final lazy val FactoryVec3iSByte = factory[Vec3i, SByte](new ArrayVec3i(cast(FactoryInt1SByte)))
  implicit final lazy val FactoryVec3iUByte = factory[Vec3i, UByte](new ArrayVec3i(cast(FactoryInt1UByte)))
  implicit final lazy val FactoryVec3iSShort = factory[Vec3i, SShort](new ArrayVec3i(cast(FactoryInt1SShort)))
  implicit final lazy val FactoryVec3iUShort = factory[Vec3i, UShort](new ArrayVec3i(cast(FactoryInt1UShort)))
  implicit final lazy val FactoryVec3iSInt = factory[Vec3i, SInt](new ArrayVec3iSInt())
  implicit final lazy val FactoryVec3iUInt = factory[Vec3i, UInt](new ArrayVec3i(cast(FactoryInt1UInt)))

  // Vec4i
  implicit final lazy val FactoryVec4iSByte = factory[Vec4i, SByte](new ArrayVec4i(cast(FactoryInt1SByte)))
  implicit final lazy val FactoryVec4iUByte = factory[Vec4i, UByte](new ArrayVec4i(cast(FactoryInt1UByte)))
  implicit final lazy val FactoryVec4iSShort = factory[Vec4i, SShort](new ArrayVec4i(cast(FactoryInt1SShort)))
  implicit final lazy val FactoryVec4iUShort = factory[Vec4i, UShort](new ArrayVec4i(cast(FactoryInt1UShort)))
  implicit final lazy val FactoryVec4iSInt = factory[Vec4i, SInt](new ArrayVec4iSInt())
  implicit final lazy val FactoryVec4iUInt = factory[Vec4i, UInt](new ArrayVec4i(cast(FactoryInt1UInt)))
}
