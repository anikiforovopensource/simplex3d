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
import scala.reflect._


/**
 * @author Aleksey Nikiforov (lex)
 */
object RawTag {
  import scala.reflect.ClassTag._
  
  final val SByte = classTag[SByte]
  final val UByte = classTag[UByte]
  final val SShort = classTag[SShort]
  final val UShort = classTag[UShort]
  final val SInt = PrimitiveFormat.SInt
  final val UInt = classTag[UInt]
  final val HFloat = classTag[HFloat]
  final val RFloat = PrimitiveFormat.RFloat
  final val RDouble = PrimitiveFormat.RDouble

  final val AllTangible = List[ClassTag[_ <: Raw with Tangible]](
    SByte, UByte, SShort, UShort, SInt, UInt, HFloat, RFloat, RDouble
  )

  def toRawType(m: ClassTag[_]) :Int = {
    m match {
      case SByte => RawType.SByte
      case UByte => RawType.UByte
      case SShort => RawType.SShort
      case UShort => RawType.UShort
      case SInt => RawType.SInt
      case UInt => RawType.UInt
      case HFloat => RawType.HFloat
      case RFloat => RawType.RFloat
      case RDouble => RawType.RDouble
    }
  }
  
  def fromRawType(t: Int) :ClassTag[_] = {
    t match {
      case RawType.SByte => SByte
      case RawType.UByte => UByte
      case RawType.SShort => SShort
      case RawType.UShort => UShort
      case RawType.SInt => SInt
      case RawType.UInt => UInt
      case RawType.HFloat => HFloat
      case RawType.RFloat => RFloat
      case RawType.RDouble => RDouble
    }
  }
}
