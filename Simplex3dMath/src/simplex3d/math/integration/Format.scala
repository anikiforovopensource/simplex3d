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


/** <code>Meta</code> declares distinct types for reading and writing data.
 *
 * @author Aleksey Nikiforov (lex)
 */
trait Meta {
  type Read
  type Const <: Read
}

/** <code>Format</code> indicates format.
 *
 * @author Aleksey Nikiforov (lex)
 */
sealed trait Format {
  type Meta <: simplex3d.math.integration.Meta
  type Component <: PrimitiveFormat
}

/** <code>PrimitiveFormat</code>.
 *
 * @author Aleksey Nikiforov (lex)
 */
sealed trait PrimitiveFormat extends Format {
  type Meta <: simplex3d.math.integration.Meta { type Read <: AnyVal }
}

/** <code>CompositeFormat</code>.
 *
 * @author Aleksey Nikiforov (lex)
 */
trait CompositeFormat extends Format {
  type Meta <: simplex3d.math.integration.Meta { type Read <: AnyRef }
}


sealed trait Compressed extends Meta with PrimitiveFormat {
  type Read = Unit
  type Const = Read
  
  type Meta = Compressed
  type Component = Compressed
}

sealed trait Bool extends Meta with PrimitiveFormat {
  type Read = Boolean
  type Const = Read
  
  type Meta = Bool
  type Component = Bool
}


object PrimitiveFormat {
  final val Compressed = classType[Compressed](classOf[Compressed])
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

sealed trait SInt extends Meta with PrimitiveFormat with RawInt with Signed
with DefinedInt with DefinedFloat with DefinedDouble {
  type Read = Int
  type Const = Read
  
  type Meta = SInt
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

sealed trait RFloat extends Meta with PrimitiveFormat with SysFP
with DefinedFloat with DefinedDouble {
  type Read = Float
  type Const = Read
  
  type Meta = RFloat
  type Component = RFloat

  type Array = scala.Array[Float]
  type Buffer = nio.FloatBuffer
}

sealed trait RDouble extends Meta with PrimitiveFormat with SysFP
with DefinedDouble {
  type Read = Double
  type Const = Read
  
  type Meta = RDouble
  type Component = RDouble

  type Array = scala.Array[Double]
  type Buffer = nio.DoubleBuffer
}
