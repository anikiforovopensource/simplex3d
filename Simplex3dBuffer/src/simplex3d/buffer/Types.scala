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

import java.nio
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
  final val HFloat = 5131
  final val RFloat = 5126
  final val RDouble = 5130

  def byteLength(rawType: Int) :Int = {
    (rawType: @switch) match {
      case SByte | UByte => 1
      case SShort | UShort | HFloat => 2
      case SInt | UInt | RFloat => 4
      case RDouble => 8
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
      case HFloat => "HFloat"
      case RFloat => "RFloat"
      case RDouble => "RDouble"
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
      case SByte | UByte => ByteStore
      case SShort | HFloat => ShortStore
      case UShort => CharStore
      case SInt | UInt => IntStore
      case RFloat => FloatStore
      case RDouble => DoubleStore
    }
  }
}
