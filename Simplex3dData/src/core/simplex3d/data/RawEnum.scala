/*
 * Simplex3dData - Core Module
 * Copyright (C) 2010-2013, Aleksey Nikiforov
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
import scala.reflect._
import scala.reflect.runtime.universe._


/**
 * @author Aleksey Nikiforov (lex)
 */
object RawEnum {
  final val SByte = 5120
  final val UByte = 5121
  final val SShort = 5122
  final val UShort = 5123
  final val SInt = 5124
  final val UInt = 5125
  final val HFloat = 5131
  final val RFloat = 5126
  final val RDouble = 5130

  def byteLength(rawEnum: Int) :Int = {
    (rawEnum: @switch) match {
      case SByte | UByte => 1
      case SShort | UShort | HFloat => 2
      case SInt | UInt | RFloat => 4
      case RDouble => 8
    }
  }

  def toString(rawEnum: Int) :String = {
    (rawEnum: @switch) match {
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
  
  object ClassTags {
    final val SByte = classTag[SByte]
    final val UByte = classTag[UByte]
    final val SShort = classTag[SShort]
    final val UShort = classTag[UShort]
    final val SInt = classTag[SInt]
    final val UInt = classTag[UInt]
    final val HFloat = classTag[HFloat]
    final val RFloat = classTag[RFloat]
    final val RDouble = classTag[RDouble]

    final val AllTangible = List[ClassTag[_ <: Raw with Tangible]](
      SByte, UByte, SShort, UShort, SInt, UInt, HFloat, RFloat, RDouble
    )

    def toRawEnum(t: ClassTag[_]) :Int = {
      t match {
        case SByte => RawEnum.SByte
        case UByte => RawEnum.UByte
        case SShort => RawEnum.SShort
        case UShort => RawEnum.UShort
        case SInt => RawEnum.SInt
        case UInt => RawEnum.UInt
        case HFloat => RawEnum.HFloat
        case RFloat => RawEnum.RFloat
        case RDouble => RawEnum.RDouble
      }
    }

    def fromRawEnum(i: Int) :ClassTag[_] = {
      i match {
        case RawEnum.SByte => SByte
        case RawEnum.UByte => UByte
        case RawEnum.SShort => SShort
        case RawEnum.UShort => UShort
        case RawEnum.SInt => SInt
        case RawEnum.UInt => UInt
        case RawEnum.HFloat => HFloat
        case RawEnum.RFloat => RFloat
        case RawEnum.RDouble => RDouble
      }
    }
  }

  object TypeTags {
    final val SByte = typeTag[SByte]
    final val UByte = typeTag[UByte]
    final val SShort = typeTag[SShort]
    final val UShort = typeTag[UShort]
    final val SInt = typeTag[SInt]
    final val UInt = typeTag[UInt]
    final val HFloat = typeTag[HFloat]
    final val RFloat = typeTag[RFloat]
    final val RDouble = typeTag[RDouble]

    final val AllTangible = List[TypeTag[_ <: Raw with Tangible]](
      SByte, UByte, SShort, UShort, SInt, UInt, HFloat, RFloat, RDouble
    )

    def toRawEnum(t: TypeTag[_]) :Int = {
      t match {
        case SByte => RawEnum.SByte
        case UByte => RawEnum.UByte
        case SShort => RawEnum.SShort
        case UShort => RawEnum.UShort
        case SInt => RawEnum.SInt
        case UInt => RawEnum.UInt
        case HFloat => RawEnum.HFloat
        case RFloat => RawEnum.RFloat
        case RDouble => RawEnum.RDouble
      }
    }

    def fromRawEnum(i: Int) :TypeTag[_] = {
      i match {
        case RawEnum.SByte => SByte
        case RawEnum.UByte => UByte
        case RawEnum.SShort => SShort
        case RawEnum.UShort => UShort
        case RawEnum.SInt => SInt
        case RawEnum.UInt => UInt
        case RawEnum.HFloat => HFloat
        case RawEnum.RFloat => RFloat
        case RawEnum.RDouble => RDouble
      }
    }
  }
}
