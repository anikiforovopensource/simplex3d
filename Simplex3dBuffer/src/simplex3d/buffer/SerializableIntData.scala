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


/**
 * @author Aleksey Nikiforov (lex)
 */
@serializable @SerialVersionUID(8104346712419693669L)
class SerializableIntData(val components: Int, val rawType: Int)
extends SerializableData
{
  protected def toDataArray(): DataArray[_, _] = {
    import RawType._

    components match {
      case 1 =>
        rawType match {
          case SByte => FactorySIntSByte.mkDataArray(content.asInstanceOf[Array[Byte]])
          case UByte => FactorySIntUByte.mkDataArray(content.asInstanceOf[Array[Byte]])
          case SShort => FactorySIntSShort.mkDataArray(content.asInstanceOf[Array[Short]])
          case UShort => FactorySIntUShort.mkDataArray(content.asInstanceOf[Array[Char]])
          case SInt => FactorySIntSInt.mkDataArray(content.asInstanceOf[Array[Int]])
          case UInt => FactorySIntUInt.mkDataArray(content.asInstanceOf[Array[Int]])
        }
      case 2 =>
        rawType match {
          case SByte => FactoryVec2iSByte.mkDataArray(content.asInstanceOf[Array[Byte]])
          case UByte => FactoryVec2iUByte.mkDataArray(content.asInstanceOf[Array[Byte]])
          case SShort => FactoryVec2iSShort.mkDataArray(content.asInstanceOf[Array[Short]])
          case UShort => FactoryVec2iUShort.mkDataArray(content.asInstanceOf[Array[Char]])
          case SInt => FactoryVec2iSInt.mkDataArray(content.asInstanceOf[Array[Int]])
          case UInt => FactoryVec2iUInt.mkDataArray(content.asInstanceOf[Array[Int]])
        }
      case 3 =>
        rawType match {
          case SByte => FactoryVec3iSByte.mkDataArray(content.asInstanceOf[Array[Byte]])
          case UByte => FactoryVec3iUByte.mkDataArray(content.asInstanceOf[Array[Byte]])
          case SShort => FactoryVec3iSShort.mkDataArray(content.asInstanceOf[Array[Short]])
          case UShort => FactoryVec3iUShort.mkDataArray(content.asInstanceOf[Array[Char]])
          case SInt => FactoryVec3iSInt.mkDataArray(content.asInstanceOf[Array[Int]])
          case UInt => FactoryVec3iUInt.mkDataArray(content.asInstanceOf[Array[Int]])
        }
      case 4 =>
        rawType match {
          case SByte => FactoryVec4iSByte.mkDataArray(content.asInstanceOf[Array[Byte]])
          case UByte => FactoryVec4iUByte.mkDataArray(content.asInstanceOf[Array[Byte]])
          case SShort => FactoryVec4iSShort.mkDataArray(content.asInstanceOf[Array[Short]])
          case UShort => FactoryVec4iUShort.mkDataArray(content.asInstanceOf[Array[Char]])
          case SInt => FactoryVec4iSInt.mkDataArray(content.asInstanceOf[Array[Int]])
          case UInt => FactoryVec4iUInt.mkDataArray(content.asInstanceOf[Array[Int]])
        }
    }
  }
}
