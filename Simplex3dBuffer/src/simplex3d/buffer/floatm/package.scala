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
import simplex3d.buffer.floatm._
import simplex3d.buffer.floatm.impl._


/**
 * @author Aleksey Nikiforov (lex)
 */
package object floatm {

  private final def primitiveFactory[R <: DefinedFloat](f: DataFactory[RFloat, R]) = f
  private final def factory[E <: Meta](f: CompositionFactory[E, DefinedFloat]) = f
  private[this] final val array = new ArrayRFloatRFloat

  // RFloat
  implicit final val FactoryRFloatSByte = primitiveFactory[SByte](new ArrayRFloatSByte)
  implicit final val FactoryRFloatUByte = primitiveFactory[UByte](new ArrayRFloatUByte)
  implicit final val FactoryRFloatSShort = primitiveFactory[SShort](new ArrayRFloatSShort)
  implicit final val FactoryRFloatUShort = primitiveFactory[UShort](new ArrayRFloatUShort)
  implicit final val FactoryRFloatSInt = primitiveFactory[SInt](new ArrayRFloatSInt)
  implicit final val FactoryRFloatUInt = primitiveFactory[UInt](new ArrayRFloatUInt)
  implicit final val FactoryRFloatHFloat = primitiveFactory[HFloat](new ArrayRFloatHFloat)
  implicit final val FactoryRFloatRFloat = primitiveFactory[RFloat](array)

  // Composition
  implicit final val FactoryRFloat = factory[RFloat](array)
  implicit final val FactoryVec2f = factory[Vec2f](new ArrayVec2f(array))
  implicit final val FactoryVec3f = factory[Vec3f](new ArrayVec3f(array))
  implicit final val FactoryVec4f = factory[Vec4f](new ArrayVec4f(array))
}
