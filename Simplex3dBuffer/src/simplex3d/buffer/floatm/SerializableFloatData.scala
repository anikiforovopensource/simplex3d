/*
 * Simplex3d, FloatBuffer module
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
package floatm


/**
 * @author Aleksey Nikiforov (lex)
 */
@serializable @SerialVersionUID(8104346712419693669L)
class SerializableFloatData(val components: Int, val rawType: Int)
extends SerializableData
{
  protected def toDataArray(): DataArray[_, _] = {
    import RawType._

    components match {
      case 1 =>
        rawType match {
          case SByte => FactoryFloat1SByte.mkDataArray(content.asInstanceOf[Array[Byte]])
          case UByte => FactoryFloat1UByte.mkDataArray(content.asInstanceOf[Array[Byte]])
          case SShort => FactoryFloat1SShort.mkDataArray(content.asInstanceOf[Array[Short]])
          case UShort => FactoryFloat1UShort.mkDataArray(content.asInstanceOf[Array[Char]])
          case SInt => FactoryFloat1SInt.mkDataArray(content.asInstanceOf[Array[Int]])
          case UInt => FactoryFloat1UInt.mkDataArray(content.asInstanceOf[Array[Int]])
          case HalfFloat => FactoryFloat1HalfFloat.mkDataArray(content.asInstanceOf[Array[Short]])
          case RawFloat => FactoryFloat1RawFloat.mkDataArray(content.asInstanceOf[Array[Float]])
        }
      case 2 =>
        rawType match {
          case SByte => FactoryVec2fSByte.mkDataArray(content.asInstanceOf[Array[Byte]])
          case UByte => FactoryVec2fUByte.mkDataArray(content.asInstanceOf[Array[Byte]])
          case SShort => FactoryVec2fSShort.mkDataArray(content.asInstanceOf[Array[Short]])
          case UShort => FactoryVec2fUShort.mkDataArray(content.asInstanceOf[Array[Char]])
          case SInt => FactoryVec2fSInt.mkDataArray(content.asInstanceOf[Array[Int]])
          case UInt => FactoryVec2fUInt.mkDataArray(content.asInstanceOf[Array[Int]])
          case HalfFloat => FactoryVec2fHalfFloat.mkDataArray(content.asInstanceOf[Array[Short]])
          case RawFloat => FactoryVec2fRawFloat.mkDataArray(content.asInstanceOf[Array[Float]])
        }
      case 3 =>
        rawType match {
          case SByte => FactoryVec3fSByte.mkDataArray(content.asInstanceOf[Array[Byte]])
          case UByte => FactoryVec3fUByte.mkDataArray(content.asInstanceOf[Array[Byte]])
          case SShort => FactoryVec3fSShort.mkDataArray(content.asInstanceOf[Array[Short]])
          case UShort => FactoryVec3fUShort.mkDataArray(content.asInstanceOf[Array[Char]])
          case SInt => FactoryVec3fSInt.mkDataArray(content.asInstanceOf[Array[Int]])
          case UInt => FactoryVec3fUInt.mkDataArray(content.asInstanceOf[Array[Int]])
          case HalfFloat => FactoryVec3fHalfFloat.mkDataArray(content.asInstanceOf[Array[Short]])
          case RawFloat => FactoryVec3fRawFloat.mkDataArray(content.asInstanceOf[Array[Float]])
        }
      case 4 =>
        rawType match {
          case SByte => FactoryVec4fSByte.mkDataArray(content.asInstanceOf[Array[Byte]])
          case UByte => FactoryVec4fUByte.mkDataArray(content.asInstanceOf[Array[Byte]])
          case SShort => FactoryVec4fSShort.mkDataArray(content.asInstanceOf[Array[Short]])
          case UShort => FactoryVec4fUShort.mkDataArray(content.asInstanceOf[Array[Char]])
          case SInt => FactoryVec4fSInt.mkDataArray(content.asInstanceOf[Array[Int]])
          case UInt => FactoryVec4fUInt.mkDataArray(content.asInstanceOf[Array[Int]])
          case HalfFloat => FactoryVec4fHalfFloat.mkDataArray(content.asInstanceOf[Array[Short]])
          case RawFloat => FactoryVec4fRawFloat.mkDataArray(content.asInstanceOf[Array[Float]])
        }
    }
  }
}
