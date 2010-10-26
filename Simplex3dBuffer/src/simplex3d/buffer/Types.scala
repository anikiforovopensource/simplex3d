/*
 * Simplex3d, CoreBuffer module
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

  def byteLength(rawType: Int) :Int = {
    (rawType: @switch) match {
      case SByte => 1
      case UByte => 1
      case SShort => 2
      case UShort => 2
      case SInt => 4
      case UInt => 4
      case HalfFloat => 2
      case RawFloat => 4
      case RawDouble => 8
    }
  }

  def name(rawType: Int) :String = {
    (rawType: @switch) match {
      case SByte => "SByte"
      case UByte => "UByte"
      case SShort => "SShort"
      case UShort => "UShort"
      case SInt => "SInt"
      case UInt => "UInt"
      case HalfFloat => "HalfFloat"
      case RawFloat => "RawFloat"
      case RawDouble => "RawDouble"
    }
  }
}

private[buffer] object StoreType {
  final val ByteStore = 0
  final val ShortStore = 1
  final val CharStore = 2
  final val IntStore = 3
  final val FloatStore = 4
  final val DoubleStore = 5

  def storeFromRaw(rawType: Int) = {
    import RawType._
    (rawType: @switch) match {
      case SByte => ByteStore
      case UByte => ByteStore
      case SShort => ShortStore
      case UShort => CharStore
      case SInt => IntStore
      case UInt => IntStore
      case HalfFloat => ShortStore
      case RawFloat => FloatStore
      case RawDouble => DoubleStore
    }
  }
}

sealed trait RawData {
  type ArrayType <: AnyRef // Scalac is too buggy to handle the bound "<: Array[_]"
  type BufferType <: Buffer
}

sealed trait Defined extends RawData
sealed trait DefinedInt extends Defined
sealed trait DefinedIndex extends DefinedInt with Unsigned
sealed trait DefinedFloat extends Defined
sealed trait DefinedDouble extends Defined

sealed trait IntegralData extends RawData

sealed trait Signed extends IntegralData
sealed trait Unsigned extends IntegralData


sealed trait RawByte extends IntegralData {
  type ArrayType = Array[Byte]
  type BufferType = ByteBuffer
}

sealed trait SByte extends RawByte with Signed
with DefinedInt with DefinedFloat with DefinedDouble

sealed trait UByte extends RawByte with Unsigned
with DefinedIndex with DefinedFloat with DefinedDouble


sealed trait RawShort extends IntegralData

sealed trait SShort extends RawShort with Signed
with DefinedInt with DefinedFloat with DefinedDouble {
  type ArrayType = Array[Short]
  type BufferType = ShortBuffer
}

sealed trait UShort extends RawShort with Unsigned
with DefinedIndex with DefinedFloat with DefinedDouble {
  type ArrayType = Array[Char]
  type BufferType = CharBuffer
}


sealed trait RawInt extends IntegralData {
  type ArrayType = Array[Int]
  type BufferType = IntBuffer
}

sealed trait SInt extends RawInt with Signed
with DefinedInt with DefinedFloat with DefinedDouble

sealed trait UInt extends RawInt with Unsigned
with DefinedIndex with DefinedFloat with DefinedDouble


sealed trait FloatingPointData extends RawData

sealed trait HalfFloat extends FloatingPointData
with DefinedFloat with DefinedDouble {
  type ArrayType = Array[Short]
  type BufferType = ShortBuffer
}

sealed trait RawFloat extends FloatingPointData
with DefinedFloat with DefinedDouble {
  type ArrayType = Array[Float]
  type BufferType = FloatBuffer
}

sealed trait RawDouble extends FloatingPointData
with DefinedDouble {
  type ArrayType = Array[Double]
  type BufferType = DoubleBuffer
}
