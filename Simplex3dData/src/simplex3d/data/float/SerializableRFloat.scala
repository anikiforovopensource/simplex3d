/*
 * Simplex3dData - Float Module
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
package float

import simplex3d.data.common._


/**
 * @author Aleksey Nikiforov (lex)
 */
@SerialVersionUID(8104346712419693669L)
private[data] class PrimitiveRFloat(val rawType: Int) extends SerializablePrimitive with Serializable {
  protected def toReadDataArray(): ReadDataArray[_, _] = {
    import RawType._

    rawType match {
      case SByte => DataArray[RFloat, SByte](content.asInstanceOf[Array[Byte]])
      case UByte => DataArray[RFloat, UByte](content.asInstanceOf[Array[Byte]])
      case SShort => DataArray[RFloat, SShort](content.asInstanceOf[Array[Short]])
      case UShort => DataArray[RFloat, UShort](content.asInstanceOf[Array[Char]])
      case SInt => DataArray[RFloat, SInt](content.asInstanceOf[Array[Int]])
      case UInt => DataArray[RFloat, UInt](content.asInstanceOf[Array[Int]])
      case HFloat => DataArray[RFloat, HFloat](content.asInstanceOf[Array[Short]])
      case RFloat => DataArray[RFloat, RFloat](content.asInstanceOf[Array[Float]])
    }
  }
}


@SerialVersionUID(8104346712419693669L)
private[data] class CompositeRFloat(val components: Int) extends SerializableComposite with Serializable {
  
  protected def toReadDataArray(
    primitives: ReadDataArray[_ <: PrimitiveFormat, _]
  ): ReadDataArray[_ <: CompositeFormat, _] = {
    components match {
      case 2 => FactoryVec2f.mkReadDataArray(primitives.asInstanceOf[ReadDataArray[RFloat, _ <: DefinedFloat]])
      case 3 => FactoryVec3f.mkReadDataArray(primitives.asInstanceOf[ReadDataArray[RFloat, _ <: DefinedFloat]])
      case 4 => FactoryVec4f.mkReadDataArray(primitives.asInstanceOf[ReadDataArray[RFloat, _ <: DefinedFloat]])
    }
  }
}
