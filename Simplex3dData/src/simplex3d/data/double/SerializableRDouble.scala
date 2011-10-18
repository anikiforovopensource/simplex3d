/*
 * Simplex3dData - Double Module
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
package double

import simplex3d.data.common._


/**
 * @author Aleksey Nikiforov (lex)
 */
@SerialVersionUID(8104346712419693669L)
private[data] class PrimitiveRDouble(val rawType: Int) extends SerializablePrimitive with Serializable {
  protected def toReadDataArray(): ReadDataArray[_, _] = {
    import RawType._

    rawType match {
      case SByte => DataArray[RDouble, SByte](content.asInstanceOf[Array[Byte]])
      case UByte => DataArray[RDouble, UByte](content.asInstanceOf[Array[Byte]])
      case SShort => DataArray[RDouble, SShort](content.asInstanceOf[Array[Short]])
      case UShort => DataArray[RDouble, UShort](content.asInstanceOf[Array[Char]])
      case SInt => DataArray[RDouble, SInt](content.asInstanceOf[Array[Int]])
      case UInt => DataArray[RDouble, UInt](content.asInstanceOf[Array[Int]])
      case HFloat => DataArray[RDouble, HFloat](content.asInstanceOf[Array[Short]])
      case RFloat => DataArray[RDouble, RFloat](content.asInstanceOf[Array[Float]])
      case RDouble => DataArray[RDouble, RDouble](content.asInstanceOf[Array[Double]])
    }
  }
}


@SerialVersionUID(8104346712419693669L)
private[data] class CompositeRDouble(val components: Int) extends SerializableComposite with Serializable {
  
  protected def toReadDataArray(
    primitives: ReadDataArray[_ <: PrimitiveFormat, _]
  ): ReadDataArray[_ <: CompositeFormat, _] = {
    components match {
      case 2 => FactoryVec2d.mkReadDataArray(primitives.asInstanceOf[ReadDataArray[RDouble, _ <: DefinedDouble]])
      case 3 => FactoryVec3d.mkReadDataArray(primitives.asInstanceOf[ReadDataArray[RDouble, _ <: DefinedDouble]])
      case 4 => FactoryVec4d.mkReadDataArray(primitives.asInstanceOf[ReadDataArray[RDouble, _ <: DefinedDouble]])
    }
  }
}
