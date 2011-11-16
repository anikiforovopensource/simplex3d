/*
 * Simplex3dData - Core Module
 * Copyright (C) 2010-2011, Aleksey Nikiforov
 *
 * This file is part of Simplex3dData.
 *
 * Simplex3dData is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dData is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.data

import scala.annotation._


/**
 * @author Aleksey Nikiforov (lex)
 */
private[data] object StoreType {
  final val ByteStore = 0
  final val ShortStore = 1
  final val CharStore = 2
  final val IntStore = 3
  final val FloatStore = 4
  final val DoubleStore = 5

  def fromRawType(rawType: Int) = {
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
