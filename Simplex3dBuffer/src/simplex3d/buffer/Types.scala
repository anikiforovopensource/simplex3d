/*
 * Simplex3d, BaseBuffer module
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
import scala.annotation._


/**
 * @author Aleksey Nikiforov (lex)
 */
object RawType {
  final val SByte = 5120
  final val UByte = 5121
  final val SShort = 5122
  final val UShort = 5123
  final val SInt = 5124
  final val UInt = 5125
  final val HalfFloat = 5131
  final val RawFloat = 5126
  final val RawDouble = 5130

  def byteLength(binding: Int) :Byte = {
    (binding: @switch) match {
      case SByte => 1
      case UByte => 1
      case SShort => 2
      case UShort => 2
      case SInt => 4
      case UInt => 4
      case HalfFloat => 2
      case RawFloat => 4
      case RawDouble => 8
      case _ => throw new AssertionError("Binding not found.")
    }
  }
}

sealed trait RawType {
  type ArrayType <: Array[_]
  type BufferType <: Buffer
}

sealed trait ReadableType extends RawType
sealed trait ReadableInt extends ReadableType
sealed trait ReadableIndex extends ReadableInt with Unsigned
sealed trait ReadableFloat extends ReadableType
sealed trait ReadableDouble extends ReadableType

sealed trait Integral extends RawType

sealed trait Signed extends Integral
sealed trait Unsigned extends Integral
sealed trait Normalized extends Integral
sealed trait NonNormalized extends Integral


sealed trait RawByte extends Integral {
  type ArrayType = Array[Byte]
  type BufferType = ByteBuffer
}

sealed trait SByte extends RawByte with Signed with NonNormalized
with ReadableInt with ReadableFloat with ReadableDouble

sealed trait UByte extends RawByte with Unsigned with NonNormalized
with ReadableIndex with ReadableFloat with ReadableDouble

sealed trait NSByte extends RawByte with Signed with Normalized
with ReadableFloat with ReadableDouble

sealed trait NUByte extends RawByte with Unsigned with Normalized
with ReadableFloat with ReadableDouble


sealed trait RawShort extends Integral

sealed trait SShort extends RawShort with Signed with NonNormalized
with ReadableInt with ReadableFloat with ReadableDouble {
  type ArrayType = Array[Short]
  type BufferType = ShortBuffer
}

sealed trait UShort extends RawShort with Unsigned with NonNormalized
with ReadableIndex with ReadableFloat with ReadableDouble {
  type ArrayType = Array[Char]
  type BufferType = CharBuffer
}

sealed trait NSShort extends RawShort with Signed with Normalized
with ReadableFloat with ReadableDouble {
  type ArrayType = Array[Short]
  type BufferType = ShortBuffer
}

sealed trait NUShort extends RawShort with Unsigned with Normalized
with ReadableFloat with ReadableDouble {
  type ArrayType = Array[Char]
  type BufferType = CharBuffer
}


sealed trait RawInt extends Integral {
  type ArrayType = Array[Int]
  type BufferType = IntBuffer
}

sealed trait SInt extends RawInt with Signed with NonNormalized
with ReadableInt with ReadableFloat with ReadableDouble

sealed trait UInt extends RawInt with Unsigned with NonNormalized
with ReadableIndex with ReadableFloat with ReadableDouble

sealed trait NSInt extends RawInt with Signed with Normalized
with ReadableFloat with ReadableDouble

sealed trait NUInt extends RawInt with Unsigned with Normalized
with ReadableFloat with ReadableDouble


sealed trait Fractional extends RawType

sealed trait HalfFloat extends Fractional
with ReadableFloat with ReadableDouble {
  type ArrayType = Array[Short]
  type BufferType = ShortBuffer
}

sealed trait RawFloat extends Fractional
with ReadableFloat with ReadableDouble {
  type ArrayType = Array[Float]
  type BufferType = FloatBuffer
}

sealed trait RawDouble extends Fractional
with ReadableDouble {
  type ArrayType = Array[Double]
  type BufferType = DoubleBuffer
}
