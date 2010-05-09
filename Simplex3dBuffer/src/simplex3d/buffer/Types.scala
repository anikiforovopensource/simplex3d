/*
 * Simplex3d, BaseBuffer module
 * Copyright (C) 2010 Simplex3d Team
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
object Binding {
  final val SByte = 5120
  final val UByte = 5121
  final val SShort = 5122
  final val UShort = 5123
  final val SInt = 5124
  final val UInt = 5125
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

sealed trait ReadType extends RawType
sealed trait ReadInt extends ReadType
sealed trait ReadFloat extends ReadType
sealed trait ReadDouble extends ReadType

sealed trait IntegerType extends RawType

sealed trait Signed extends IntegerType
sealed trait Unsigned extends IntegerType
sealed trait Normalized extends IntegerType
sealed trait NonNormalized extends IntegerType


sealed trait RawByte extends IntegerType {
  type ArrayType = Array[Byte]
  type BufferType = ByteBuffer
}

sealed trait SByte extends RawByte with Signed with NonNormalized
with ReadInt with ReadFloat with ReadDouble

sealed trait UByte extends RawByte with Unsigned with NonNormalized
with ReadInt with ReadFloat with ReadDouble

sealed trait NSByte extends RawByte with Signed with Normalized
with ReadFloat with ReadDouble

sealed trait NUByte extends RawByte with Unsigned with Normalized
with ReadFloat with ReadDouble


sealed trait RawShort extends IntegerType

sealed trait SShort extends RawShort with Signed with NonNormalized
with ReadInt with ReadFloat with ReadDouble {
  type ArrayType = Array[Short]
  type BufferType = ShortBuffer
}

sealed trait UShort extends RawShort with Unsigned with NonNormalized
with ReadInt with ReadFloat with ReadDouble {
  type ArrayType = Array[Char]
  type BufferType = CharBuffer
}

sealed trait NSShort extends RawShort with Signed with Normalized
with ReadFloat with ReadDouble {
  type ArrayType = Array[Short]
  type BufferType = ShortBuffer
}

sealed trait NUShort extends RawShort with Unsigned with Normalized
with ReadFloat with ReadDouble {
  type ArrayType = Array[Char]
  type BufferType = CharBuffer
}


sealed trait RawInt extends IntegerType {
  type ArrayType = Array[Int]
  type BufferType = IntBuffer
}

sealed trait SInt extends RawInt with Signed with NonNormalized
with ReadInt with ReadFloat with ReadDouble

sealed trait UInt extends RawInt with Unsigned with NonNormalized
with ReadInt with ReadFloat with ReadDouble

sealed trait NSInt extends RawInt with Signed with Normalized
with ReadFloat with ReadDouble

sealed trait NUInt extends RawInt with Unsigned with Normalized
with ReadFloat with ReadDouble


sealed trait FloatingPointType extends RawType

sealed trait RawFloat extends FloatingPointType
with ReadFloat with ReadDouble {
  type ArrayType = Array[Float]
  type BufferType = FloatBuffer
}

sealed trait RawDouble extends FloatingPointType
with ReadDouble {
  type ArrayType = Array[Double]
  type BufferType = DoubleBuffer
}

private[buffer] object ReadAs {
  val SByte = classOf[SByte]
  val UByte = classOf[UByte]
  val NSByte = classOf[NSByte]
  val NUByte = classOf[NUByte]

  val SShort = classOf[SShort]
  val UShort = classOf[UShort]
  val NSShort = classOf[NSShort]
  val NUShort = classOf[NUShort]

  val SInt = classOf[SInt]
  val UInt = classOf[UInt]
  val NSInt = classOf[NSInt]
  val NUInt = classOf[NUInt]

  val RawFloat = classOf[RawFloat]
  val RawDouble = classOf[RawDouble]
}
