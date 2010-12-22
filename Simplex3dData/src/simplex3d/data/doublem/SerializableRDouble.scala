/*
 * Simplex3d, DoubleData module
 * Copyright (C) 2010, Simplex3d Team
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
package doublem


/**
 * @author Aleksey Nikiforov (lex)
 */
@serializable @SerialVersionUID(8104346712419693669L)
private[buffer] class PrimitiveRDouble(val rawType: Int) extends SerializablePrimitive {
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


@serializable @SerialVersionUID(8104346712419693669L)
private[buffer] class CompositeRDouble(val components: Int) extends SerializableComposite {
  protected def toReadDataArray(primitive: ReadDataArray[_ <: Primitive, _]): ReadDataArray[_ <: Composite, _] = {
    components match {
      case 2 => FactoryVec2d.mkReadDataArray(primitive.asInstanceOf[ReadDataArray[RDouble, _ <: DefinedDouble]])
      case 3 => FactoryVec3d.mkReadDataArray(primitive.asInstanceOf[ReadDataArray[RDouble, _ <: DefinedDouble]])
      case 4 => FactoryVec4d.mkReadDataArray(primitive.asInstanceOf[ReadDataArray[RDouble, _ <: DefinedDouble]])
    }
  }
}
