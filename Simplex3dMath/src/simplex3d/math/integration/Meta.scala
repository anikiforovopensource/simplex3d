/*
 * Simplex3d, CoreMath module
 * Copyright (C) 2010-2011, Aleksey Nikiforov
 *
 * This file is part of Simplex3dMath.
 *
 * Simplex3dMath is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dMath is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.math.integration

import java.nio
import scala.reflect.ClassManifest.{classType}


/** <code>Meta</code> is used to integrate the math package with buffers.
 *
 * @author Aleksey Nikiforov (lex)
 */
trait Meta {
  type Read
  type Const <: Read
  type Component <: PrimitiveMeta
}

/** <code>PrimitiveMeta</code> marker indicates primitive elements/components.
 *
 * @author Aleksey Nikiforov (lex)
 */
sealed trait PrimitiveMeta extends Meta {
  type Read <: AnyVal
  type Const = Read
}

/** <code>CompositeMeta</code> marker indicates elements composed of
 * primitive components.
 *
 * @author Aleksey Nikiforov (lex)
 */
trait CompositeMeta extends Meta {
  type Read <: AnyRef
}

sealed trait Bool extends PrimitiveMeta {
  type Read = Boolean
  type Component = Bool
}


object MetaManifest {
  final val Bool = classType[Bool](classOf[Bool])
  final val SInt = classType[SInt](classOf[SInt])
  final val RFloat = classType[RFloat](classOf[RFloat])
  final val RDouble = classType[RDouble](classOf[RDouble])
}


sealed trait Raw {
  type Array <: AnyRef // Scalac cannot handle the bound "<: Array[_]"
  type Buffer <: nio.Buffer
}

sealed trait Defined extends Raw
sealed trait DefinedInt extends Defined
sealed trait DefinedIndex extends DefinedInt with Unsigned
sealed trait DefinedFloat extends Defined
sealed trait DefinedDouble extends Defined

sealed trait Integral extends Raw

sealed trait Signed extends Integral
sealed trait Unsigned extends Integral


sealed trait RawByte extends Integral {
  type Array = scala.Array[Byte]
  type Buffer = nio.ByteBuffer
}

sealed trait SByte extends RawByte with Signed
with DefinedInt with DefinedFloat with DefinedDouble

sealed trait UByte extends RawByte with Unsigned
with DefinedIndex with DefinedFloat with DefinedDouble


sealed trait RawShort extends Integral

sealed trait SShort extends RawShort with Signed
with DefinedInt with DefinedFloat with DefinedDouble {
  type Array = scala.Array[Short]
  type Buffer = nio.ShortBuffer
}

sealed trait UShort extends RawShort with Unsigned
with DefinedIndex with DefinedFloat with DefinedDouble {
  type Array = scala.Array[Char]
  type Buffer = nio.CharBuffer
}


sealed trait RawInt extends Integral {
  type Array = scala.Array[Int]
  type Buffer = nio.IntBuffer
}

sealed trait SInt extends PrimitiveMeta with RawInt with Signed
with DefinedInt with DefinedFloat with DefinedDouble {
  type Read = Int
  type Component = SInt
}

sealed trait UInt extends RawInt with Unsigned
with DefinedIndex with DefinedFloat with DefinedDouble


sealed trait FloatingPoint extends Raw

/** System floating point: either float or double.
 */
sealed trait SysFP extends FloatingPoint

sealed trait HFloat extends FloatingPoint
with DefinedFloat with DefinedDouble {
  type Array = scala.Array[Short]
  type Buffer = nio.ShortBuffer
}

sealed trait RFloat extends PrimitiveMeta with SysFP
with DefinedFloat with DefinedDouble {
  type Read = Float
  type Component = RFloat

  type Array = scala.Array[Float]
  type Buffer = nio.FloatBuffer
}

sealed trait RDouble extends PrimitiveMeta with SysFP
with DefinedDouble {
  type Read = Double
  type Component = RDouble

  type Array = scala.Array[Double]
  type Buffer = nio.DoubleBuffer
}
