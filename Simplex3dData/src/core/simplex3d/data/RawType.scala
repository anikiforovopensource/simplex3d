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

  def toString(rawType: Int) :String = {
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
