/*
 * Simplex3d, DoubleBuffer module
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
package doublem


/**
 * @author Aleksey Nikiforov (lex)
 */
@serializable @SerialVersionUID(8104346712419693669L)
class SerializableDoubleData(val components: Int, val rawType: Int)
extends SerializableData
{
  protected def toDataArray(): DataArray[_, _] = {
    import RawType._

    components match {
      case 1 =>
        rawType match {
          case SByte => FactoryRDoubleSByte.mkDataArray(content.asInstanceOf[Array[Byte]])
          case UByte => FactoryRDoubleUByte.mkDataArray(content.asInstanceOf[Array[Byte]])
          case SShort => FactoryRDoubleSShort.mkDataArray(content.asInstanceOf[Array[Short]])
          case UShort => FactoryRDoubleUShort.mkDataArray(content.asInstanceOf[Array[Char]])
          case SInt => FactoryRDoubleSInt.mkDataArray(content.asInstanceOf[Array[Int]])
          case UInt => FactoryRDoubleUInt.mkDataArray(content.asInstanceOf[Array[Int]])
          case HFloat => FactoryRDoubleHFloat.mkDataArray(content.asInstanceOf[Array[Short]])
          case RFloat => FactoryRDoubleRFloat.mkDataArray(content.asInstanceOf[Array[Float]])
          case RDouble => FactoryRDoubleRDouble.mkDataArray(content.asInstanceOf[Array[Double]])
        }
      case 2 =>
        rawType match {
          case SByte => FactoryVec2dSByte.mkDataArray(content.asInstanceOf[Array[Byte]])
          case UByte => FactoryVec2dUByte.mkDataArray(content.asInstanceOf[Array[Byte]])
          case SShort => FactoryVec2dSShort.mkDataArray(content.asInstanceOf[Array[Short]])
          case UShort => FactoryVec2dUShort.mkDataArray(content.asInstanceOf[Array[Char]])
          case SInt => FactoryVec2dSInt.mkDataArray(content.asInstanceOf[Array[Int]])
          case UInt => FactoryVec2dUInt.mkDataArray(content.asInstanceOf[Array[Int]])
          case HFloat => FactoryVec2dHFloat.mkDataArray(content.asInstanceOf[Array[Short]])
          case RFloat => FactoryVec2dRFloat.mkDataArray(content.asInstanceOf[Array[Float]])
          case RDouble => FactoryVec2dRDouble.mkDataArray(content.asInstanceOf[Array[Double]])
        }
      case 3 =>
        rawType match {
          case SByte => FactoryVec3dSByte.mkDataArray(content.asInstanceOf[Array[Byte]])
          case UByte => FactoryVec3dUByte.mkDataArray(content.asInstanceOf[Array[Byte]])
          case SShort => FactoryVec3dSShort.mkDataArray(content.asInstanceOf[Array[Short]])
          case UShort => FactoryVec3dUShort.mkDataArray(content.asInstanceOf[Array[Char]])
          case SInt => FactoryVec3dSInt.mkDataArray(content.asInstanceOf[Array[Int]])
          case UInt => FactoryVec3dUInt.mkDataArray(content.asInstanceOf[Array[Int]])
          case HFloat => FactoryVec3dHFloat.mkDataArray(content.asInstanceOf[Array[Short]])
          case RFloat => FactoryVec3dRFloat.mkDataArray(content.asInstanceOf[Array[Float]])
          case RDouble => FactoryVec3dRDouble.mkDataArray(content.asInstanceOf[Array[Double]])
        }
      case 4 =>
        rawType match {
          case SByte => FactoryVec4dSByte.mkDataArray(content.asInstanceOf[Array[Byte]])
          case UByte => FactoryVec4dUByte.mkDataArray(content.asInstanceOf[Array[Byte]])
          case SShort => FactoryVec4dSShort.mkDataArray(content.asInstanceOf[Array[Short]])
          case UShort => FactoryVec4dUShort.mkDataArray(content.asInstanceOf[Array[Char]])
          case SInt => FactoryVec4dSInt.mkDataArray(content.asInstanceOf[Array[Int]])
          case UInt => FactoryVec4dUInt.mkDataArray(content.asInstanceOf[Array[Int]])
          case HFloat => FactoryVec4dHFloat.mkDataArray(content.asInstanceOf[Array[Short]])
          case RFloat => FactoryVec4dRFloat.mkDataArray(content.asInstanceOf[Array[Float]])
          case RDouble => FactoryVec4dRDouble.mkDataArray(content.asInstanceOf[Array[Double]])
        }
    }
  }
}
