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
import simplex3d.buffer.doublem._
import simplex3d.buffer.doublem.impl._


/**
 * @author Aleksey Nikiforov (lex)
 */
package object doublem {

  private final def primitiveFactory[R <: DefinedDouble](f: DataFactory[RDouble, R]) = f
  private final def factory[E <: Meta](f: CompositionFactory[E, DefinedDouble]) = f
  private[this] final val array = new ArrayRDoubleRFloat

  // RDouble
  implicit final val FactoryRDoubleSByte = primitiveFactory[SByte](new ArrayRDoubleSByte)
  implicit final val FactoryRDoubleUByte = primitiveFactory[UByte](new ArrayRDoubleUByte)
  implicit final val FactoryRDoubleSShort = primitiveFactory[SShort](new ArrayRDoubleSShort)
  implicit final val FactoryRDoubleUShort = primitiveFactory[UShort](new ArrayRDoubleUShort)
  implicit final val FactoryRDoubleSInt = primitiveFactory[SInt](new ArrayRDoubleSInt)
  implicit final val FactoryRDoubleUInt = primitiveFactory[UInt](new ArrayRDoubleUInt)
  implicit final val FactoryRDoubleHFloat = primitiveFactory[HFloat](new ArrayRDoubleHFloat)
  implicit final val FactoryRDoubleRFloat = primitiveFactory[RFloat](array)
  implicit final val FactoryRDoubleRDouble = primitiveFactory[RDouble](new ArrayRDoubleRDouble)

  // Composition
  implicit final val FactoryRDouble = factory[RDouble](array)
  implicit final val FactoryVec2d = factory[Vec2d](new ArrayVec2d(array))
  implicit final val FactoryVec3d = factory[Vec3d](new ArrayVec3d(array))
  implicit final val FactoryVec4d = factory[Vec4d](new ArrayVec4d(array))
}
